{-# LANGUAGE RecordWildCards #-}
module FilterMoreSpecifics ( filterLS , filterRIBIPV4UnicastOrPeerTable, filterRIBIPV4Unicast , filterSlash25 , filterSlash32 , filterSlash25to31 , mrtToTree , mrtFromTree , Overlap.size , Overlap.count , Overlap.toList ) where

{-

the purpose of this function is to filter a list of entries for overlapped prefixes
it does so by excluding and replacing longer prefixes for shorter ones on the same underlying address
this requires a 'least specific lookup' of some form
a naive implmentation would require lookups over every possible length, which is quite expensive (and complex)
-}
import Data.Maybe(mapMaybe)
import Data.IP
import Data.Word(byteSwap32)
import MRTlib
import MRTPrefixes
import Overlap

type CustomFilter = MRTRecord -> Bool

filterRIBIPV4UnicastOrPeerTable :: CustomFilter
filterRIBIPV4UnicastOrPeerTable RIBIPV4Unicast{..} = True
filterRIBIPV4UnicastOrPeerTable MRTPeerIndexTable{..} = True
filterRIBIPV4UnicastOrPeerTable _ = False

filterRIBIPV4Unicast :: CustomFilter
filterRIBIPV4Unicast RIBIPV4Unicast{..} = True
filterRIBIPV4Unicast _ = False

filterSlash32 :: CustomFilter
filterSlash32 RIBIPV4Unicast{..} = 32 == re4Length
filterSlash32 _ = True

filterSlash25to31 :: CustomFilter
filterSlash25to31 RIBIPV4Unicast{..} = (24 < re4Length) && ( 32 > re4Length)
filterSlash25to31 _ = True

filterSlash25 :: CustomFilter
filterSlash25 RIBIPV4Unicast{..} = (25 > re4Length)
--filterSlash25 RIBIPV4Unicast{..} | (24 < re4Length) = True
filterSlash25 _ = True

filterLS :: [MRTRecord] -> [MRTRecord]
filterLS = mrtFromTree . mrtToTree

mrtToTree :: [MRTRecord] -> Tree [RIBEntry]
mrtToTree = Overlap.fromList . mapMaybe mrtToLeaf where
    -- reject default route - otherwise the answer empty when it is present ;-)
    mrtToLeaf RIBIPV4Unicast{..} | (0 < re4Length) = Just (Prefix(re4Length, byteSwap32 $ toHostAddress re4Address) , re4RIB)
    mrtToLeaf _ = Nothing

mrtFromTree :: Tree [RIBEntry] -> [MRTRecord]
mrtFromTree = map mrtFromLeaf . zip [0..] . Overlap.toListLS where
    mrtFromLeaf (n,(Prefix(l,v),ribs)) = RIBIPV4Unicast (fromIntegral n) l (fromHostAddress $ byteSwap32 v) ribs
