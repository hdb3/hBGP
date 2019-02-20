module Codes where
import Data.Word
import Data.Bits

import LibCommon

data PathAttributeTypeCode = TypeCodePathAttributeOrigin | TypeCodePathAttributeASPath | TypeCodePathAttributeNextHop | TypeCodePathAttributeMultiExitDisc |
                             TypeCodePathAttributeLocalPref | TypeCodePathAttributeAtomicAggregate | TypeCodePathAttributeAggregator |
                             TypeCodePathAttributeCommunities | TypeCodePathAttributeMPREachNLRI | TypeCodePathAttributeMPUnreachNLRI |
                             TypeCodePathAttributeExtendedCommunities | TypeCodePathAttributeAS4Path | TypeCodePathAttributeAS4Aggregator |
                             TypeCodePathAttributeConnector | TypeCodePathAttributeASPathlimit | TypeCodePathAttributeLargeCommunity | TypeCodePathAttributeAttrSet |
                             TypeCodePathAttributeUnknown
                             deriving (Show,Eq,Ord)

allPathAttributeTypeCodes = [ TypeCodePathAttributeOrigin , TypeCodePathAttributeASPath , TypeCodePathAttributeNextHop , TypeCodePathAttributeMultiExitDisc ,
                              TypeCodePathAttributeLocalPref , TypeCodePathAttributeAtomicAggregate , TypeCodePathAttributeAggregator , TypeCodePathAttributeCommunities ,
                              TypeCodePathAttributeMPREachNLRI , TypeCodePathAttributeMPUnreachNLRI , TypeCodePathAttributeExtendedCommunities ,
                              TypeCodePathAttributeAS4Path , TypeCodePathAttributeAS4Aggregator , TypeCodePathAttributeConnector , TypeCodePathAttributeASPathlimit ,
                              TypeCodePathAttributeLargeCommunity , TypeCodePathAttributeAttrSet]

optional   = 0x80 :: Word8
transitive = 0x40 :: Word8
partial    = 0x20 :: Word8
extended   = 0x10 :: Word8
extendedBitTest :: Word8 -> Bool
extendedBitTest w = testBit w 4 -- counting up from LSB numbered as 0
setExtended :: Word8 -> Word8
setExtended w = w .|. extended

requiredPathAttributes = [TypeCodePathAttributeOrigin,TypeCodePathAttributeASPath,TypeCodePathAttributeNextHop]

flagCheck :: Word8 -> PathAttributeTypeCode -> Bool
flagCheck _ TypeCodePathAttributeUnknown = True
flagCheck flags code = (flags .&. 0xc0) == flagsOf code

flagsOf :: PathAttributeTypeCode -> Word8
flagsOf e | e == TypeCodePathAttributeOrigin = transitive 
          | e == TypeCodePathAttributeASPath = transitive
          | e == TypeCodePathAttributeNextHop = transitive
          | e == TypeCodePathAttributeMultiExitDisc = optional
          | e == TypeCodePathAttributeLocalPref = transitive
          | e == TypeCodePathAttributeAtomicAggregate = transitive
          | e == TypeCodePathAttributeAggregator = transitive .|. optional
          | e == TypeCodePathAttributeCommunities = transitive .|. optional
          | e == TypeCodePathAttributeMPREachNLRI = optional
          | e == TypeCodePathAttributeMPUnreachNLRI = optional
          | e == TypeCodePathAttributeExtendedCommunities = transitive .|. optional
          | e == TypeCodePathAttributeAS4Path = transitive .|. optional
          | e == TypeCodePathAttributeAS4Aggregator = transitive .|. optional
          | e == TypeCodePathAttributeConnector = transitive .|. optional
          | e == TypeCodePathAttributeASPathlimit = transitive .|. optional
          | e == TypeCodePathAttributeLargeCommunity = transitive .|. optional
          | e == TypeCodePathAttributeAttrSet = transitive .|. optional
          | e == TypeCodePathAttributeUnknown = optional

instance EnumWord8 PathAttributeTypeCode where
instance Enum PathAttributeTypeCode where

    toEnum n   | n == 1 = TypeCodePathAttributeOrigin
               | n == 2 = TypeCodePathAttributeASPath
               | n == 3 = TypeCodePathAttributeNextHop
               | n == 4 = TypeCodePathAttributeMultiExitDisc
               | n == 5 = TypeCodePathAttributeLocalPref
               | n == 6 = TypeCodePathAttributeAtomicAggregate
               | n == 7 = TypeCodePathAttributeAggregator
               | n == 8 = TypeCodePathAttributeCommunities
               | n == 14 = TypeCodePathAttributeMPREachNLRI
               | n == 15 = TypeCodePathAttributeMPUnreachNLRI
               | n == 16 = TypeCodePathAttributeExtendedCommunities
               | n == 17 = TypeCodePathAttributeAS4Path
               | n == 18 = TypeCodePathAttributeAS4Aggregator
               | n == 20 = TypeCodePathAttributeConnector
               | n == 21 = TypeCodePathAttributeASPathlimit
               | n == 32 = TypeCodePathAttributeLargeCommunity
               | n == 128 = TypeCodePathAttributeAttrSet
               | otherwise = TypeCodePathAttributeUnknown

    fromEnum e | e == TypeCodePathAttributeOrigin = 1
               | e == TypeCodePathAttributeASPath = 2
               | e == TypeCodePathAttributeNextHop = 3
               | e == TypeCodePathAttributeMultiExitDisc = 4
               | e == TypeCodePathAttributeLocalPref = 5
               | e == TypeCodePathAttributeAtomicAggregate = 6
               | e == TypeCodePathAttributeAggregator = 7
               | e == TypeCodePathAttributeCommunities = 8
               | e == TypeCodePathAttributeMPREachNLRI = 14
               | e == TypeCodePathAttributeMPUnreachNLRI = 15
               | e == TypeCodePathAttributeExtendedCommunities = 16
               | e == TypeCodePathAttributeAS4Path = 17
               | e == TypeCodePathAttributeAS4Aggregator = 18
               | e == TypeCodePathAttributeConnector = 20
               | e == TypeCodePathAttributeASPathlimit = 21
               | e == TypeCodePathAttributeLargeCommunity = 32
               | e == TypeCodePathAttributeAttrSet = 128

-- ----------------------------------------
-- for ASPath?.hs
-- ----------------------------------------
--

data ASSegmentElementTypeCode = EnumASSet | EnumASSequence deriving (Show,Eq)

instance EnumWord8 ASSegmentElementTypeCode where
instance Enum ASSegmentElementTypeCode where

    toEnum n   | n == 1 = EnumASSet
               | n == 2 = EnumASSequence

    fromEnum e | e == EnumASSet = 1
               | e == EnumASSequence = 2


enumASSet = 1 :: Word8
enumASSequence = 2 :: Word8
