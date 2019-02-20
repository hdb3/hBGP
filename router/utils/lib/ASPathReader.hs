{-# LANGUAGE FlexibleInstances,FlexibleContexts, OverloadedStrings #-}
module ASPathReader(reportPaths) where
import qualified Data.List
import Data.List(sort)

import BGPlib
import ASPathUtils

reportPaths :: [[ASSegment4]] -> IO ()
reportPaths paths = do
    let
        simplePaths = map flattenPath paths
        longestPath = Data.List.maximum (map length simplePaths)
        simplerPaths = map removePrepends simplePaths
        longestPathWithoutPrepending = Data.List.maximum (map length simplerPaths)
        uniqueAScount = length $ Data.List.nub $ concat simplePaths
        endAScount = length $ Data.List.nub $ map last simplePaths
        transitASes = concatMap (tail.reverse) simplerPaths 
        transitAScount = length $ Data.List.nub transitASes
        transitASDistribution = distribution_ 10 transitASes
        loopedPaths = sort $ filter hasLoop simplerPaths

    putStrLn   "\nAS analysis"
    putStrLn $ "longestPath:  " ++ show longestPath
    putStrLn $ "longestPathWithoutPrepending:  " ++ show longestPathWithoutPrepending
    putStrLn $ "uniqueAScount:  " ++ show uniqueAScount
    putStrLn $ "endAScount:     " ++ show endAScount
    putStrLn $ "transitAScount: " ++ show transitAScount
    putStrLn $ "transitAS distribution:\n" ++ unlines ( map show transitASDistribution )
    putStrLn $ reportSegments paths

    putStrLn $ "path loop analysis: " ++ show (length loopedPaths )  ++ " loops found"
    putStrLn $ unlines $ map show loopedPaths

    where

    customShowRoute = showPath . getASPathContent

    mapt (f,g) = map (\(a,b) -> (f a ++ " " ++ g b))
    
    reportSegments paths = unlines [heading,all,sequences,sequenceSet1,sequenceSetN,seqSetSeq] where
        heading = "\nSequence Analysis"
        all = "all " ++ show (length paths)
        sequences = "sequences " ++ show ( length $ filter matchSeq paths)
        sequenceSet1 = "sequenceSet1 " ++ show ( length $ filter matchSeqSet1 paths)
        sequenceSetN = "sequenceSetN " ++ show ( length $ filter matchSeqSet paths)
        seqSetSeq = "seqSetSeq " ++ show ( length $ filter matchSeqSetSeq paths)
    
        matchSeq [ASSequence _] = True
        matchSeq _ = False
    
        matchSeqSet1 [ASSequence _ , ASSet [_]] = True
        matchSeqSet1 _ = False
    
        matchSeqSet [ASSequence _ , ASSet [_]] = False
        matchSeqSet [ASSequence _ , ASSet _] = True
        matchSeqSet _ = False
    
        matchSeqSetSeq [ASSequence _ , ASSet _, ASSequence _] = True
        matchSeqSetSeq _ = False
