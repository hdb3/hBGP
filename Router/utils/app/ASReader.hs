{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPReader
import qualified BGPlib
import Comm
import Common
import Data.IP
import qualified Data.List
import qualified Overlap
import Prefixes
import System.IO

main = do
  rawRib <- readRib
  putStrLn $ "got " ++ show (length rawRib) ++ " routes"
  let flatRib = flatten rawRib
  putStrLn $ "length rib: " ++ show (length rawRib)
  putStrLn $ "length flatRib: " ++ show (length flatRib)
  let routeIdRib = map removePathAttributes flatRib
  print' (head routeIdRib)
  print' (last routeIdRib)
  let tree = Overlap.fromList flatRib
      tree' = Overlap.fromList routeIdRib
  putStrLn $ "tree size: " ++ show (Overlap.size tree')

flatten :: [(a, [Prefix])] -> [(a, Prefix)]
flatten = concatMap flatten'

flatten' :: (a, [Prefix]) -> [(a, Prefix)]
flatten' (a, pfxs) = map (\pfx -> (a, pfx)) pfxs

removePathAttributes (a, pfxs) = (fst a, pfxs)

print' (a, p) = putStrLn $ hex a ++ " " ++ show p

-- print' (a,v) = putStrLn $ hex a ++ " " ++ if length v < 3 then show v else show (take 2 v) ++ "..(+" ++ show (length v - 2)
