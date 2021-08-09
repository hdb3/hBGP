module MRTXRib where

{-
MRTXRib -s actually more primituve than MRTrib
MRTXRib creates a 'raw' RIB from a MRT file, hodling preciesly the RIBEntry objects presnt in the file, indexed on the prefix.
Two Ribs are generated - IPv4 and IPv6.
Useful derived RIBs can contain e.g. a bit map represnting the peer indices which refernce a given prefix
-}

import Data.Bits
import qualified Data.IntMap.Strict as Map
import Data.Maybe (fromJust)
import Data.Word
import MRTlib
import Text.Printf

type RawRIB = Map.IntMap [RIBEntry] -- either IPv4/6 - as long as there is a defined mapping from a prefix to a word64 (int)

type BiRIB = (MRTRecord, RawRIB, RawRIB) -- (IPv4,IPv6)

mrtToRIB :: [MRTRecord] -> BiRIB
mrtToRIB (mrt : mrts) = (mrt, Map.fromList $ map rib4entry (filter notSlash24 l4), Map.fromList $ map rib6entry (filter notSlash56 l6))
  where
    (l4, l6) = buildBiList mrts
    notSlash56 RIBIPV6Unicast {..} = 57 > re6Length
    notSlash24 RIBIPV4Unicast {..} = 25 > re4Length
    rib4entry RIBIPV4Unicast {..} = (v4hash (re4Address, re4Length), re4RIB)
    rib4entry mrt = error $ "rib4entry only defined on RIBIPV4Unicast: " ++ show mrt
    rib6entry RIBIPV6Unicast {..} = (v6hash (re6Address, re6Length), re6RIB)
    rib6entry mrt = error $ "rib6entry only defined on RIBIPV6Unicast: " ++ show mrt

    buildBiList :: [MRTRecord] -> ([MRTRecord], [MRTRecord])
    buildBiList = foldl insertBiList ([], [])
      where
        insertBiList (ip4s, ip6s) mrt4@RIBIPV4Unicast {} = (mrt4 : ip4s, ip6s)
        insertBiList (ip4s, ip6s) mrt6@RIBIPV6Unicast {} = (ip4s, mrt6 : ip6s)
        insertBiList x _ = x

type PeerRepRIB = Map.IntMap Word64

peerRepRIB :: RawRIB -> PeerRepRIB
peerRepRIB = Map.map getPeerRep
  where
    getPeerRep :: [RIBEntry] -> Word64
    getPeerRep = foldl f 0
      where
        f bitmap RIBEntry {..}
          | 64 > rePeerIndex = bitmap `setBit` fromIntegral rePeerIndex
          | otherwise = error $ "getPeerRep only defined for peerIdices < 64: " ++ show rePeerIndex

showRawRIBAssoc :: (Int, [RIBEntry]) -> String
showRawRIBAssoc (prefix, ribEntrys) = show ip ++ "/" ++ show l ++ " - " ++ show (length ribEntrys) where (ip, l) = v4unhash prefix

showIPv4PfxHash :: Int -> String
showIPv4PfxHash prefix = show ip ++ "/" ++ show l where (ip, l) = v4unhash prefix

showPeerRep :: (Int, Word64) -> String
showPeerRep (prefix, bitMap) = show ip ++ "/" ++ show l ++ " - " ++ printf "%08x" bitMap where (ip, l) = v4unhash prefix

checkPeerRepRIB :: PeerRepRIB -> IP4Prefix -> PeerIndex -> Bool
checkPeerRepRIB rib pfx px | 64 > px = testBit (fromJust $ Map.lookup (v4hash pfx) rib) (fromIntegral px)
