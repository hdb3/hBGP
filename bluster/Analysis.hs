{-# LANGUAGE RecordWildCards #-}
module Analysis where
import Data.List(sort,group)
import Text.Printf

import Core
import Containers
import Operations

basicAnalysis :: State -> String
basicAnalysis st@State{..} =
    "clusters: " ++ show clusterCount
                 ++ " groups: " ++ show groupCount
                 ++ " prefixes: " ++ show prefixCount
                 ++ "\n"
                 ++ "singleton prefixes: " ++ show singletonPrefixes
                 ++ "  simple clusters: " ++ show simpleClusters 
                 ++ "  (prefixes: " ++ show simpleClusterPrefixCount
                 ++ "  avg size: " ++ showF (fromIntegral simpleClusterPrefixCount/fromIntegral simpleClusters)
                 ++ ") % simple/single = " ++ showF (100.0 * fromIntegral (simpleClusterPrefixCount+singletonPrefixes) / fromIntegral prefixCount)
                 ++ "%\n"
                 ++ "acceleration factor: " ++ showF accelerationFactor
                 -- ++ " ( best acceleration factor: " ++ show bestAccelerationFactor
                 -- ++ "  simple acceleration factor: " ++ show simpleAccelerationFactor
                 ++ ")\n"
    where
    showF :: Float -> String
    showF f = printf "%2.2f" f
    clusterCount = length clusterList
    groupCount = length groupRib
    prefixCount = length prefixRib
    singletonPrefixes = length $ singletonPrefixClusters st
    simpleClusters =  length ( getSimpleClusters st ) - singletonPrefixes
    simpleClusterPrefixCount = prefixClusterCount ( getSimpleClusters st ) - singletonPrefixes
    accelerationFactor = fromIntegral prefixCount / fromIntegral groupCount :: Float
    -- bestAccelerationFactor = fromIntegral prefixCount / fromIntegral clusterCount :: Float
    -- simpleAccelerationFactor = fromIntegral prefixCount / fromIntegral simpleClusters :: Float
                       

prefixClusterCount :: [Cluster] -> Int
prefixClusterCount = sum . map countPrefixes
    where
    countPrefixes = length . concatMap basicPrefixes . clBasicGroups
 
singletonPrefixClusters :: State -> [Cluster]
singletonPrefixClusters st = filter ( (1 ==) . length . basicPrefixes . head . clBasicGroups ) (getSimpleClusters st)

getSimpleClusters :: State -> [Cluster]
getSimpleClusters st = filter ( (1 ==) . length . clBasicGroups ) (elems $ clusterList st )

analysis :: State -> String
analysis s = unlines results
    where
    results = [ analysis1 s, analysis2 s, analysis3 s ]

    analysis1 = partitionOrderAnalysis
    analysis2 = clusterSizeAnalysis
    analysis3 = clusterGroupSizeAnalysis

partitionOrderAnalysis :: State -> String
partitionOrderAnalysis State{..} = "Partition Order Analysis: " ++ showHistogram partionSizes
    where
    partionSizes = map ( ((-1) +) . fromIntegral . length . clBasicGroups) (elems clusterList)

clusterSizeAnalysis :: State -> String
clusterSizeAnalysis State{..} = "Cluster Size Analysis: " ++ showHistogram clusterSizes
    where
    clusterSizes = map ( fromIntegral . length . concatMap basicPrefixes . clBasicGroups) (elems clusterList)

clusterGroupSizeAnalysis :: State -> String
clusterGroupSizeAnalysis State{..} = "Cluster Group Size Analysis: " ++ showHistogram clusterSizes
    where
    clusterSizes = map ( fromIntegral . length . clCompositeGroups) (elems clusterList)

histogram :: [Int] -> [(Int, Int)]
histogram = map countAndTell . group . sort
    where
    countAndTell ax = (head ax, fromIntegral $ length ax)

showHistogram :: [Int] -> String
showHistogram tx = "[" ++ show (length tx) ++ "] " ++ show ( histogram tx ) ++ "\n" ++ show (showPercentiles [75,90,99] tx ) ++ "\n\n"
-- showHistogram tx = show ( histogram tx ) ++ "\n" ++ showPercentoGram ( percentoGram $ histogram tx ) ++ "\n\n"

showPercentiles :: Ord a => [Float] -> [a] -> [(a, Float)]
-- 
-- show the first 'a' for which the cumulative count exceeds a specific percentage of all 'a's
--
-- start by counting the totals and then the corresponding (Int) values which must be met or exceeded to trigger a note
showPercentiles percentiles vals = go 0 percentiles (sort vals)
    where
    count = length vals
    percentileToCount percentile = ceiling ( percentile / 100 * fromIntegral count) :: Int
    go :: Int -> [Float] -> [a] -> [(a, Float)]
    go _ _ [] = []
    go _ [] _ = []
    go cnt (brk:brks) (a:ax) | percentileToCount brk < cnt = (a,brk) : go (cnt+1) brks ax
                             | otherwise = go (cnt+1) (brk:brks) ax

showPercentoGram :: (Show a) => [(a, Float)] -> String
showPercentoGram = concatMap showAPercentoGram
    where showAPercentoGram (a,f) = "(" ++ show a ++ " , " ++ printf "%2.2f" f ++ ")"

percentoGram :: [(a, Int)] -> [(a, Float)]
-- display a percentage take on a histogram
-- snd parameter is a count, want to show this as either % or %.(1-).cumulative
percentoGram tx = asProportionSnd total (cumulativeSnd tx)
    where
    total = fromIntegral $ sum (map snd tx) :: Float
    float n = fromIntegral n :: Float
    cumulativeSnd = go 0
        where
        go _ [] = []
        go n ((a,b):abx) = (a,b+n) : go (b+n) abx
    asProportionSnd n = map (\(a,b) -> (a, 100 * float b/n))
