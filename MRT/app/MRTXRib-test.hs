{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap.Strict as Map
import MRTXRib
import MRTlib

main :: IO ()
main = do
  putStrLn "MRTXRib-test"
  f <- BS.getContents
  let mrtMsgs = take 20000000 $ mrtParse f
  putStrLn $ show (length mrtMsgs) ++ " MRT messages loaded"
  let (mrtPeerTable, v4rib, v6rib) = mrtToRIB mrtMsgs
  putStrLn $ show (length $ peerTable mrtPeerTable) ++ " peers in MRT Peer Table"
  putStrLn $ show (length v4rib) ++ " IPv4 prefixes loaded"
  putStrLn $ show (length v6rib) ++ " IPv6 prefixes loaded"
  let repRib4 = peerRepRIB v4rib
  putStrLn $ unlines $ map showPeerRep $ Map.assocs repRib4
  putStrLn $ unlines $ map showRawRIBAssoc $ Map.assocs v4rib
  putStrLn "done"
