module Main where

import qualified ClusterMetrics
import Control.Monad (mapM_)
import FilterMoreSpecifics
import MRTRibAnalysis
import MRTrib
import PrefixGroupMetrics
import Text.Printf

main :: IO ()
main = do
  mrtss <- getMRTTableDumps
  if null mrtss
    then putStrLn "no RIB records found in file"
    else do
      let filteredMrtss = map (filter filterSlash25) mrtss
          rawRecordCount = sum $ map length mrtss
          filteredRecordCount = sum $ map length filteredMrtss
      putStrLn $ show rawRecordCount ++ " raw records read "
      putStrLn $ show (rawRecordCount - filteredRecordCount) ++ " records discarded (> /24)"
      let ipv4PeerTable = getMRTRibs filteredMrtss
          ipv6PeerTable = getMRTRibsV6 filteredMrtss
      putStrLn $ showMRTRibV4 ipv4PeerTable
      putStrLn $ showMRTRibV6 ipv6PeerTable
      doRIBv4 ipv4PeerTable
      doRIBv6 ipv6PeerTable
      simplePeerMetrics ipv4PeerTable
      simpleGroupMetrics ipv4PeerTable
      groupMetrics ipv4PeerTable
      extendedMetrics ipv4PeerTable

doRIBv4 ribDB = do
  comparePeerStats ribDB
  putStrLn $ showMRTRibV4 ribDB
  putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB)
  putStrLn $ "max path count is: " ++ show (maxPathCount ribDB)
  let v4table = preFilterTable 0.02 ribDB
  putStrLn $ showMRTRibV4 v4table
  putStrLn $ showMetrics v4table

doRIBv6 ribDB6 = do
  comparePeerStats ribDB6
  putStrLn $ showMRTRibV6 ribDB6
  putStrLn $ "max prefix count is: " ++ show (maxPrefixCount ribDB6)
  putStrLn $ "max path count is: " ++ show (maxPathCount ribDB6)
  let v6table = preFilterTable 0.02 ribDB6
  putStrLn $ showMRTRibV6 v6table
  putStrLn $ showMetrics v6table

extendedMetrics ribDB = do
  let validTables = preFilterTable 0.03 ribDB
  putStrLn $ "extendedMetrics (sample set size " ++ show (length validTables) ++ ")"
  putStrLn "(n , empty , subset , superset , multiple , multiple/partial)"
  mapM_ showExtendedMetrics (pairs validTables)
  where
    showExtendedMetrics ((pi0, p0, m0), (pi1, p1, m1)) = putStrLn $ printf "(%2d,%2d) " pi0 pi1 ++ compareRouteMapv4 m0 m1

simplePeerMetrics ribDB = do
  let validTables = preFilterTable 0.03 ribDB
  putStrLn $ "simplePeerMetrics (sample set size " ++ show (length validTables) ++ ")"
  putStrLn "(peer index , peer , routes , prefixes"
  mapM_ showPeerMetrics validTables
  where
    showPeerMetrics (pi, p, m) = putStrLn $ show pi ++ " - " ++ show p ++ " - " ++ showStatsRouteMap m

simpleGroupMetrics ribDB = do
  let validTables = sortOnLength $ preFilterTable 0.05 ribDB
  putStrLn $ "simpleGroupMetrics - full tables only - (sample set size " ++ show (length validTables) ++ ")"
  putStrLn $ "using RIBs from : " ++ show (map (\(pi, _, _) -> pi) validTables)
  putStrLn $ ClusterMetrics.simpleCompareRouteMaps (map (\(_, _, peer) -> peer) validTables)

  let validTables = sortOnLength ribDB
  putStrLn $ "simpleGroupMetrics - all tables - (sample set size " ++ show (length validTables) ++ ")"
  putStrLn $ "using RIBs from : " ++ show (map (\(pi, _, _) -> pi) validTables)
  putStrLn $ ClusterMetrics.simpleCompareRouteMaps (map (\(_, _, peer) -> peer) validTables)

groupMetrics ribDB = do
  let validTables = sortOnLength $ preFilterTable 0.03 ribDB
  putStrLn $ "groupMetrics (sample set size " ++ show (length validTables) ++ ")"
  mapM_ (showGroupMetrics validTables) [2 .. length validTables]
  where
    showGroupMetrics t l = do
      let peerList = take l t
      putStrLn $ "using RIBs from " ++ show l ++ " : " ++ show (map (\(pi, _, _) -> pi) peerList)
      putStrLn $ ClusterMetrics.compareRouteMaps (map (\(_, _, peer) -> peer) peerList)
