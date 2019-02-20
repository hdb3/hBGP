module Main where
import Text.Printf
import Control.Monad(mapM_)
import MRTrib
import MRTRibAnalysis
import PrefixGroupMetrics
import FilterMoreSpecifics
import qualified ClusterMetrics


main :: IO ()
main = do
    mrtss <- getMRTTableDumps
    if null mrtss then
        putStrLn "no RIB records found in file"
    else do
        putStr $ show (sum $ map length mrtss) ++ " raw records read "
        let ribV4FilteredMrtss = map (filter filterRIBIPV4UnicastOrPeerTable) mrtss
        putStr $ show (sum $ map length ribV4FilteredMrtss) ++ " ribv4 filtered records read "
        let bogonFilteredMrtss = map mrtBogonFilter mrtss
        putStr $ show (sum $ map length bogonFilteredMrtss) ++ " bogon filtered records read "
        let filteredMrtss = map (filter filterSlash25) bogonFilteredMrtss
        putStr $ show (sum $ map length filteredMrtss) ++ " /24+ filtered records read "
        let ipv4PeerTable = getMRTRibs filteredMrtss
        putStrLn $ showMRTRibV4 ipv4PeerTable
        --simplePeerMetrics ipv4PeerTable
        --simpleGroupMetrics ipv4PeerTable

extendedMetrics ribDB = do
    let validTables = preFilterTable 0.03 ribDB
    putStrLn $ "extendedMetrics (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn "(n , empty , subset , superset , multiple , multiple/partial)"
    mapM_ showExtendedMetrics (pairs validTables)
    where
    showExtendedMetrics ((pi0,p0,m0),(pi1,p1,m1)) = putStrLn $ printf "(%2d,%2d) " pi0 pi1  ++ compareRouteMapv4 m0 m1
    -- showExtendedMetrics ((pi0,p0,m0),(pi1,p1,m1)) = putStrLn $ printf "(%2d,%2d) " pi0 pi1  ++ ClusterMetrics.compareRouteMapv4 m0 m1

simplePeerMetrics ribDB = do
    let validTables = preFilterTable 0.03 ribDB
    putStrLn $ "simplePeerMetrics (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn "(peer index , peer , routes , prefixes"
    mapM_ showPeerMetrics validTables
    where
    showPeerMetrics (pi,p,m) = putStrLn $ show pi ++ " - " ++ show p ++ " - " ++ showStatsRouteMap m

simpleGroupMetrics ribDB = do
    let validTables = sortOnLength $ preFilterTable 0.05 ribDB
    putStrLn $ "simpleGroupMetrics - full tables only - (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn $ "using RIs from : " ++ show ( map (\(pi,_,_) -> pi) validTables )
    putStrLn $ ClusterMetrics.simpleCompareRouteMaps ( map (\(_,_,peer) -> peer) validTables)

    let validTables = sortOnLength ribDB
    putStrLn $ "simpleGroupMetrics - all tables - (sample set size " ++ show ( length validTables) ++ ")"
    putStrLn $ "using RIs from : " ++ show ( map (\(pi,_,_) -> pi) validTables )
    putStrLn $ ClusterMetrics.simpleCompareRouteMaps ( map (\(_,_,peer) -> peer) validTables)

groupMetrics ribDB = do
    let validTables = sortOnLength $ preFilterTable 0.03 ribDB
    putStrLn $ "groupMetrics (sample set size " ++ show ( length validTables) ++ ")"
    mapM_ (showGroupMetrics validTables) [2.. length validTables]
    where
    showGroupMetrics t l = do
        let peerList = take l t
        putStrLn $ "using RIs from " ++ show l ++ " : " ++ show ( map (\(pi,_,_) -> pi) peerList )
        putStrLn $ ClusterMetrics.compareRouteMaps ( map (\(_,_,peer) -> peer) peerList)
