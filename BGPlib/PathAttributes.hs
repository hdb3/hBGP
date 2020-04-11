{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module BGPlib.PathAttributes
  ( module BGPlib.Codes,
    module BGPlib.PathAttributes,
    module BGPlib.ASPath,
  )
where

import BGPlib.ASPath
import BGPlib.Codes
import BGPlib.LibCommon
import qualified Data.ByteString as B
import Data.Hashable
import Data.List (deleteBy, find, sortOn)
import Data.Word

-- for some this function may be all that is ever wanted....
-- decodeAttributes :: L.ByteString -> [PathAttribute]
-- decodeAttributes = runGet (get :: Get [PathAttribute])

-- decodePathAttributes :: B.ByteString -> [PathAttribute]
-- decodePathAttributes = decodeAttributes . L.fromStrict

-- encodePathAttributes :: [PathAttribute] -> B.ByteString
-- encodePathAttributes = L.toStrict . encode

data ExtendedCommunities = ExtendedCommunities deriving (Show, Eq)

type LargeCommunity = (Word32, Word32, Word32)

getPathAttribute :: PathAttributeTypeCode -> [PathAttribute] -> Maybe PathAttribute
getPathAttribute code = find ((code ==) . identify)

deletePathAttributeType :: PathAttributeTypeCode -> [PathAttribute] -> [PathAttribute]
deletePathAttributeType t = filter ((t /=) . identify)

insertPathAttribute :: PathAttribute -> [PathAttribute] -> [PathAttribute]
-- replaces an existing attribute of the same type
insertPathAttribute attr = sortPathAttributes . (attr :) . deleteBy sameSort attr
  where
    sameSort a b = identify a == identify b

sortPathAttributes :: [PathAttribute] -> [PathAttribute]
sortPathAttributes = sortOn identify

substitutePathAttribute :: PathAttribute -> [PathAttribute] -> [PathAttribute]
-- silently ignores request if attribute is missing, use insertPathAttribute if this is not waht is required
substitutePathAttribute attr = map (f attr)
  where
    f a b = if sameSort a b then a else b
    sameSort a b = identify a == identify b

updatePathAttribute :: PathAttributeTypeCode -> (PathAttribute -> PathAttribute) -> [PathAttribute] -> [PathAttribute]
-- note silently ignores request if attribute is missing :-(
updatePathAttribute t f = map f'
  where
    f' a
      | t == identify a = f a
      | otherwise = a

data PathAttribute
  = PathAttributeOrigin Word8
  | PathAttributeASPath ASPath
  | PathAttributeNextHop IPv4
  | PathAttributeMultiExitDisc Word32
  | PathAttributeLocalPref Word32
  | PathAttributeAtomicAggregate
  | PathAttributeAggregator (Word32, IPv4)
  | PathAttributeCommunities [Word32]
  | PathAttributeMPREachNLRI B.ByteString
  | PathAttributeMPUnreachNLRI B.ByteString
  | PathAttributeExtendedCommunities [Word64]
  | PathAttributeAS4Path ASPath
  | PathAttributeAS4Aggregator (Word32, Word32)
  | PathAttributeConnector B.ByteString
  | PathAttributeASPathlimit B.ByteString
  | PathAttributeLargeCommunity [LargeCommunity]
  | PathAttributeAttrSet B.ByteString
  | PathAttributeUnknown B.ByteString
  deriving (Show, Eq, Generic)

instance Hashable PathAttribute

instance Hashable IPv4

identify :: PathAttribute -> PathAttributeTypeCode
identify PathAttributeOrigin {} = TypeCodePathAttributeOrigin
identify PathAttributeASPath {} = TypeCodePathAttributeASPath
identify PathAttributeNextHop {} = TypeCodePathAttributeNextHop
identify PathAttributeMultiExitDisc {} = TypeCodePathAttributeMultiExitDisc
identify PathAttributeLocalPref {} = TypeCodePathAttributeLocalPref
identify PathAttributeAtomicAggregate {} = TypeCodePathAttributeAtomicAggregate
identify PathAttributeAggregator {} = TypeCodePathAttributeAggregator
identify PathAttributeCommunities {} = TypeCodePathAttributeCommunities
identify PathAttributeMPREachNLRI {} = TypeCodePathAttributeMPREachNLRI
identify PathAttributeMPUnreachNLRI {} = TypeCodePathAttributeMPUnreachNLRI
identify PathAttributeExtendedCommunities {} = TypeCodePathAttributeExtendedCommunities
identify PathAttributeAS4Path {} = TypeCodePathAttributeAS4Path
identify PathAttributeAS4Aggregator {} = TypeCodePathAttributeAS4Aggregator
identify PathAttributeConnector {} = TypeCodePathAttributeConnector
identify PathAttributeASPathlimit {} = TypeCodePathAttributeASPathlimit
identify PathAttributeLargeCommunity {} = TypeCodePathAttributeLargeCommunity
identify PathAttributeAttrSet {} = TypeCodePathAttributeAttrSet
identify PathAttributeUnknown {} = TypeCodePathAttributeUnknown
