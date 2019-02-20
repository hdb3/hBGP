{-# LANGUAGE RecordWildCards #-}

-- deprecated - only the test harness uses this library
--
-- the function getMRTTableDumpV2 was relocated to MRTlib
--

module MRTquest where
import Data.Array.IArray
import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as Map
import qualified MRTlib

getGroupedMRT :: IO [(MRTTypes, [MRTlib.MRTRecord])]
getGroupedMRT = fmap group getMRT
    where
    group recs = fmap (\(a,b) -> (unidentify a,b)) $ Map.toList $ foldl f Map.empty recs
    f m v = Map.insertWith f' (identify' v) [v] m
    f' [u] ux = u:ux 

getMRT :: IO [MRTlib.MRTRecord]
getMRT = fmap MRTlib.mrtParse BS.getContents

parsePeerRecord :: MRTlib.MRTRecord -> (MRTlib.BGPid,String,[MRTlib.MRTPeer])
parsePeerRecord MRTlib.MRTPeerIndexTable{..} = ( tdBGPID , tdViewName , peerTable )

getMRTTableDumpV2 :: IO (MRTlib.MRTRecord,[MRTlib.MRTRecord]) -- first member is guaranteed to be MRTlib.MRTPeerIndexTable
getMRTTableDumpV2 = do
    mrtList <- getMRT
    return $ tableDump mrtList where
    tableDump ( peerTable : mrtx) | MRTPeerIndexTable == identify peerTable = (peerTable, mrtFilterN [RIBIPV4Unicast, RIBIPV6Unicast] mrtx)
                                  | otherwise = error "expected MRTPeerIndexTable as first record in RIB file"

{-

-- standalone version integrated back into MRTlib

getMRTTableDumpV2 :: IO (MRTlib.MRTRecord,[MRTlib.MRTRecord]) -- first member is guaranteed to be MRTlib.MRTPeerIndexTable
getMRTTableDumpV2 = do
    mrtList <- getMRT
    return $ validate mrtList
    where
    validate ( a@MRTlib.MRTPeerIndexTable{} : b) = (a,b)
    validate _ = error "expected MRTPeerIndexTable as first record in RIB file" 
    getMRT = fmap MRTlib.mrtParse BS.getContents
-}

getMRTUpdates :: IO [MRTlib.MRTRecord]
getMRTUpdates = fmap ( mrtFilter BGP4MPMessageAS4 ) getMRT

identify :: MRTlib.MRTRecord -> MRTTypes
identify MRTlib.MRTPeerIndexTable{..} = MRTPeerIndexTable
identify MRTlib.RIBIPV4Unicast{..} = RIBIPV4Unicast
identify MRTlib.RIBIPV6Unicast{..} = RIBIPV6Unicast
identify MRTlib.MRTUnimplemented{..} = MRTUnimplemented
identify MRTlib.BGP4MPMessageAS4{..} = BGP4MPMessageAS4
identify MRTlib.BGP4MPStateChangeAS4{..} = BGP4MPStateChangeAS4
identify MRTlib.RIBv1IPv4{..} = RIBv1IPv4
identify MRTlib.RIBv1IPv6{..} = RIBv1IPv6

identify' :: MRTlib.MRTRecord -> Int
identify' = fromEnum . identify

unidentify :: Int -> MRTTypes
unidentify = toEnum

data MRTTypes = MRTPeerIndexTable|RIBIPV4Unicast|RIBIPV6Unicast|MRTUnimplemented|BGP4MPMessageAS4|BGP4MPStateChangeAS4|RIBv1IPv4|RIBv1IPv6
     deriving (Show,Enum,Bounded,Ix,Eq,Ord)

mrtFilter :: MRTTypes -> [MRTlib.MRTRecord] -> [MRTlib.MRTRecord]
mrtFilter t = filter ( (t ==) . identify)

mrtFilterN :: [MRTTypes] -> [MRTlib.MRTRecord] -> [MRTlib.MRTRecord]
mrtFilterN types = filter p where
    p mrtrec = elem (identify mrtrec) types

mrtTypes :: [MRTlib.MRTRecord] -> [MRTTypes]
mrtTypes = map identify

mrtTypeCount  :: [MRTlib.MRTRecord] -> [(MRTTypes,Int)]
mrtTypeCount mrtRecs = assocs arr where arr :: Array MRTTypes Int
                                        arr = accumArray (+) 0 bnds $ map (\mrt -> (identify mrt,1)) mrtRecs
                                        bnds = (minBound :: MRTTypes, maxBound :: MRTTypes)
