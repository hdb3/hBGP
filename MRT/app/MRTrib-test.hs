module Main where

import MRTrib

main :: IO ()
main = do
  -- (pt:rib) <- getMRTTableDumpV2
  mrtss <- getMRTTableDumps
  -- if null rib then
  if null mrtss
    then putStrLn "no RIB records found in file"
    else do
      putStr $ show (sum $ map length mrtss) ++ " records read "
      -- let ipv4PeerTable = getMRTRibV4 (pt:rib)
      let ipv4PeerTable = getMRTRibs mrtss
      putStrLn $ showMRTRibV4 ipv4PeerTable

-- let ipv6PeerTable = getMRTRibV6 (pt:rib)
-- putStrLn $ showMRTRibV6 ipv6PeerTable
