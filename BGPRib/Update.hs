{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module BGPRib.Update(decodeUpdate,modifyPathAttributes,endOfRib,encodeUpdates,ungetUpdate,ParsedUpdate(..),makeUpdate,makeUpdateSimple,igpUpdate,originateWithdraw,originateUpdate,myHash) where
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Binary
import Data.Either
import FarmHash(hash64)

import BGPlib.BGPlib
import BGPRib.Common

-- 'hash' will become 'routeId' when it is inserted into the RouteData record....
myHash :: L.ByteString -> Int
myHash = fromIntegral . hash64 . L.toStrict

data ParsedUpdate = ParsedUpdate { puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int } | NullUpdate deriving ( Show , Generic, NFData)

decodeUpdate :: BGPMessage -> ParsedUpdate
decodeUpdate BGPUpdate{..} = ParsedUpdate { puPathAttributes = decode attributes 
                                          , nlri = decode nlri 
                                          , withdrawn = decode withdrawn
                                          , hash = fromIntegral $ FarmHash.hash64 $ L.toStrict attributes 
                                          }

modifyPathAttributes :: ([PathAttribute] -> [PathAttribute]) -> ParsedUpdate -> ParsedUpdate
modifyPathAttributes f pu = pu { puPathAttributes = f $ puPathAttributes pu }

encodeUpdates :: [ParsedUpdate] -> [BGPMessage]
encodeUpdates = map ungetUpdate

-- TODO rename getUpdate/ungetUpdate encodeUpdate/decodeUpdate
ungetUpdate :: ParsedUpdate -> BGPMessage
ungetUpdate ParsedUpdate{..} = BGPUpdate { withdrawn = encode withdrawn , attributes = encode puPathAttributes , nlri = encode nlri } 

endOfRib :: BGPMessage
endOfRib = BGPUpdate { withdrawn = L.empty , attributes = L.empty , nlri = L.empty }

originateWithdraw prefixes = ParsedUpdate []  [] prefixes 0

originateUpdate :: Word8 -> [ASSegment Word32] -> IPv4 -> [Prefix] -> ParsedUpdate
originateUpdate origin path nextHop prefixes = ParsedUpdate attributes prefixes [] hash where
    attributes = [PathAttributeOrigin origin, PathAttributeASPath (ASPath4 path), PathAttributeNextHop nextHop]
    hash = myHash $ encode attributes

makeUpdateSimple :: [PathAttribute] -> [Prefix] -> [Prefix] -> ParsedUpdate
makeUpdateSimple p n w  = head $ makeUpdate n w p

makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeUpdate = makeSegmentedUpdate
makeUpdate' nlri withdrawn attributes = ParsedUpdate attributes nlri withdrawn ( myHash $ encode attributes)

makeSegmentedUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> [ParsedUpdate]
makeSegmentedUpdate nlri withdrawn attributes = result where
                                                    pathAttributesLength = L.length (encode attributes)
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
