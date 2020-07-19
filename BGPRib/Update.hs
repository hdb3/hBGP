{-# LANGUAGE RecordWildCards #-}
module BGPRib.Update(modifyPathAttributes,endOfRib,parseUpdate,deparseUpdate,ParsedUpdate(..),makeUpdate,originateWithdraw,originateUpdate,ibgpUpdate,myHash) where
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Binary
import Data.Either
import Data.Digest.Murmur64

import BGPlib.BGPlib
import BGPRib.Common

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
myHash :: L.ByteString -> Int
myHash = fromIntegral . asWord64 . hash64 . L.toStrict

data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [XPrefix], withdrawn :: [XPrefix], hash :: Int } | NullUpdate
instance Show ParsedUpdate where
      show ParsedUpdate{..} = "<" 
                              ++ if null puPathAttributes then "*" else show (getASPath puPathAttributes)
                              ++ " " ++ show nlri
                              ++ "|" ++ show withdrawn
                              ++ ">" 

modifyPathAttributes :: ([PathAttribute] -> [PathAttribute]) -> ParsedUpdate -> ParsedUpdate
modifyPathAttributes f pu = pu { puPathAttributes = f $ puPathAttributes pu }

deparseUpdate :: ParsedUpdate -> BGPMessage
deparseUpdate ParsedUpdate{..} = BGPUpdate { withdrawn = withdrawn , attributes = encode puPathAttributes , nlri = nlri }

endOfRib :: BGPMessage
endOfRib = BGPUpdate { withdrawn = [] , attributes = L.empty , nlri = [] }

parseUpdate :: BGPMessage -> ParsedUpdate
parseUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = decode attributes , nlri = nlri , withdrawn = withdrawn , hash = myHash attributes }

originateWithdraw prefixes = ParsedUpdate [] [] (map mkzXPrefix prefixes) 0

{-  ADDPATH considerations
    Herein is the boundary between the pathID agnostic world and the binary / NLRI encoding specific world
    In this instance there is no intent to generate multiple routes for a single prefix - if/when there is
    such a requirement then the calling context of  originateUpdate would need have such awareness, and should generate
    NLRI format with XPrefix (or other named type).
    For now, makeUpdate is the locus of the conversion, and we should only ever generate pathID 0 
-}
originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
originateUpdate origin path nextHop prefixes =
     head $ makeUpdate prefixes
                       []
                       [PathAttributeOrigin origin, PathAttributeASPath (ASPath4 path), PathAttributeNextHop nextHop]

ibgpUpdate = originateUpdate _BGP_ORIGIN_IGP []

{-
-- makeUpdateSimple is so dangerous that the apps which use it should instead define it locally to make clear
-- that they discard silenetly potentially significant data...
makeUpdateSimple :: [PathAttribute] -> [Prefix] -> [Prefix] -> ParsedUpdate
makeUpdateSimple p n w = head $ makeUpdate n w p
-}

-- observations
--   chunkPrefixes partitions prefixes into groups of limited binary size
--   This enables a sub-optimal division, e.g. doesn't pack withdraw content optimally
--   Also, having calculated wire format sizes without encoding in chunkPrefixes
--   (but discarding the result before returning it...), we then use Binray encode
--   to decide whether to combine the withdraw and update in the same message!!!

makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]

makeUpdate nlri' withdrawn' attributes = result where
      nlri = map mkzXPrefix nlri'
      withdrawn = map mkzXPrefix withdrawn'
      pathAttributesLength = L.length (encode attributes)
      nonPrefixLength = 16 + 2 + 1 + 2 + 2 + pathAttributesLength
      availablePrefixSpace = fromIntegral (4096 - nonPrefixLength) :: Int64
      chunkedNlri = chunkPrefixes availablePrefixSpace nlri
      chunkedWithdrawn = chunkPrefixes availablePrefixSpace withdrawn

      makeUpdate' nlri withdrawn attributes = ParsedUpdate attributes nlri withdrawn ( myHash $ encode attributes) where

      updates = map (\pfxs -> makeUpdate' pfxs [] attributes) (tail chunkedNlri)
      withdraws = map (\pfxs -> makeUpdate' [] pfxs attributes) (tail chunkedWithdrawn)
      result = if availablePrefixSpace >= L.length (encode (head chunkedNlri))
                                          + L.length (encode (head chunkedWithdrawn))
                  then [makeUpdate' (head chunkedNlri) (head chunkedWithdrawn) attributes] ++ withdraws ++ updates
                  else [makeUpdate' [] (head chunkedWithdrawn) attributes,
                        makeUpdate' (head chunkedNlri) [] attributes] ++ withdraws ++ updates
