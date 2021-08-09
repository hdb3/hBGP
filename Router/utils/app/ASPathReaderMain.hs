{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import BGPReader
import BGPlib
import BGPutils (reportPaths)

--import ASPathReader(reportPaths)

main :: IO ()
main = do
  rib <- readGroupedRib
  putStrLn $ "routes: " ++ show (length rib)
  let prefixCount = sum (map (length . snd) rib)
  putStrLn $ "prefixes: " ++ show prefixCount
  let paths = map (getASPathContent . snd . fst) rib
  reportPaths paths
