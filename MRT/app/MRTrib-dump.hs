module Main where

import MRTBuilder
import MRTrib
import System.IO

main :: IO ()
main = do
  putStrLn "MRTrib-dump"
  mrtss <- getMRTTableDumps
  if null mrtss
    then putStrLn "no RIB records found in file"
    else do
      putStr $ show (sum $ map length mrtss) ++ " records read "
      let ipv4PeerTable = getMRTRibs mrtss
      putStrLn $ showMRTRibV4 ipv4PeerTable
      let (_, p, r) = last (sortRibsOnPrefixCount ipv4PeerTable)
      dumpRib p (fromRouteMapv4 r)

dumpRib :: MRTPeer -> [(BGPAttributes, IP4PrefixList)] -> IO ()
dumpRib p routes = do
  putStrLn $ "dumping rib for " ++ show p
  let (attr, pfxs) = head routes
  putStrLn $ show pfxs ++ " : " ++ show attr
  h <- openFile "updates.raw" WriteMode
  hPutUpdate h attr pfxs
  hClose h
