{-# LANGUAGE RecordWildCards #-}
module BGPRib.Update(modifyPathAttributes,endOfRib,parseUpdate,deparseUpdate,ParsedUpdate(..),makeUpdate,makeUpdateSimple,igpUpdate,originateWithdraw,originateUpdate,myHash) where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Int
import Data.Binary
import Data.Either
import FarmHash(hash64)

import BGPlib.BGPlib
import BGPRib.Common

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
myHash :: B.ByteString -> Int
myHash = fromIntegral . hash64

data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int } | NullUpdate -- deriving Show
instance Show ParsedUpdate where
      show ParsedUpdate{..} = "<" 
                              ++ if null puPathAttributes then "*" else show (getASPath puPathAttributes)
                              ++ " " ++ show nlri
                              ++ "|" ++ show withdrawn
                              ++ ">" 

modifyPathAttributes :: ([PathAttribute] -> [PathAttribute]) -> ParsedUpdate -> ParsedUpdate
modifyPathAttributes f pu = pu { puPathAttributes = f $ puPathAttributes pu }

deparseUpdate :: ParsedUpdate -> BGPMessage
deparseUpdate ParsedUpdate{..} = BGPUpdate { withdrawn = withdrawn , attributes = encodePathAttributes puPathAttributes , nlri = nlri }

endOfRib :: BGPMessage
endOfRib = BGPUpdate { withdrawn = [] , attributes = B.empty , nlri = [] }

parseUpdate :: BGPMessage -> ParsedUpdate
parseUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = decodePathAttributes attributes , nlri = nlri , withdrawn = withdrawn , hash = myHash attributes }

originateWithdraw prefixes = ParsedUpdate [] [] prefixes 0

originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
originateUpdate origin path nextHop prefixes =
     head $ makeUpdate prefixes
                       []
                       [PathAttributeOrigin origin, PathAttributeASPath (ASPath4 path), PathAttributeNextHop nextHop]

makeUpdateSimple :: [PathAttribute] -> [Prefix] -> [Prefix] -> ParsedUpdate
makeUpdateSimple p n w = head $ makeUpdate n w p

makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeUpdate = makeSegmentedUpdate
makeUpdate' nlri withdrawn attributes = ParsedUpdate attributes nlri withdrawn ( myHash $ encodePathAttributes attributes)

makeSegmentedUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeSegmentedUpdate nlri withdrawn attributes = result where
                                                    pathAttributesLength = B.length (encodePathAttributes attributes)
                                                    nonPrefixLength = 16 + 2 + 1 + 2 + 2 + pathAttributesLength
                                                    availablePrefixSpace = fromIntegral (4096 - nonPrefixLength) :: Int64
                                                    chunkedNlri = chunkPrefixes availablePrefixSpace nlri
                                                    chunkedWithdrawn = chunkPrefixes availablePrefixSpace withdrawn
                                                    updates = map (\pfxs -> makeUpdate' pfxs [] attributes) (tail chunkedNlri)
                                                    withdraws = map (\pfxs -> makeUpdate' [] pfxs attributes) (tail chunkedWithdrawn)
                                                    result = if availablePrefixSpace >= L.length (encode (head chunkedNlri))
                                                                                        + L.length (encode (head chunkedWithdrawn))
                                                             then [makeUpdate' (head chunkedNlri) (head chunkedWithdrawn) attributes] ++ withdraws ++ updates
                                                             else [makeUpdate' [] (head chunkedWithdrawn) attributes,
                                                                   makeUpdate' (head chunkedNlri) [] attributes] ++ withdraws ++ updates


igpUpdate = originateUpdate _BGP_ORIGIN_IGP []
