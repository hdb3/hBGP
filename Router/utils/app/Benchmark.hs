{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPRib.BGPReader (readGroupedRib, readRib, updateRib)
import qualified BGPRib.BGPRib as BGPRib
import BGPlib.BGPlib
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Maybe (isJust)
import qualified Data.Time.Clock.System as DT
import qualified IP4Prefix
import MapRib
import RIBData
import RibDef
import Stopwatch
import System.Random
import System.Random.Shuffle (shuffle')
import Text.Printf

shuffle l = do
  random <- getStdGen
  return $ shuffle' l (length l) random

peer1 = Peer "peer1" True 64501 "10.0.0.1" "10.0.0.1" "10.0.0.99"

peer2 = Peer "peer2" False 64502 "10.0.0.2" "10.0.0.2" "10.0.0.99"

peer3 = Peer "peer3" False 64503 "10.0.0.3" "10.0.0.3" "10.0.0.99"

emptyMapRib = mkRib compare :: MapRib

emptyMapRib' = ([], emptyMapRib)

parseRibRoute ((_, attributes), prefix) = (RIBData.makeRoute True attributes, IP4Prefix.fromAddrRange $ toAddrRange prefix)

-- parseRibRoute ((_,attributes),prefix) = (IP4Prefix.fromAddrRange $ toAddrRange prefix, RIBData.makeRoute True attributes)

getRoutes = do
  rib <- readRib
  putStrLn $ "got " ++ show (length rib) ++ " routes"
  return $ map parseRibRoute rib

-- query :: ([(RibDef.Prefix, (Peer, Route))], MapRib) -> IO ()
query rib = query' ([], rib)

query' (updates, rib) = do
  let r = RibDef.lookup "255.255.255.255" rib
  putStrLn $ show (length updates) ++ if isJust r then " query complete!" else " query complete"

dumpRib' (_, rib) = dumpRib rib

dumpRib rib = do
  let locRib = getLocRib rib
      arbitraryFunction (peer, route) = peerAS peer + localPref route
      arbitraryHashSum = foldl (\s (_, r) -> s + arbitraryFunction r) 0 locRib
  putStrLn $ "locRib dumped " ++ if even arbitraryHashSum then "e" else "o"

main = test5

test5 = do
  t0 <- systime
  routes <- getRoutes
  print $ last routes
  t1 <- stopwatch "loaded rib" t0 t0
  shuffledRoutes <- shuffle routes
  print $ last shuffledRoutes
  t2 <- stopwatch "shuffled rib" t0 t1
  let grouped = RIBData.group shuffledRoutes
  -- let grouped = groupBy (\(_,a) (_, b) -> a == b) shuffledRoutes
  print $ "route count = " ++ show (length grouped)
  -- print $ last $ last grouped
  -- putStrLn $ unlines $ map show grouped
  t3 <- stopwatch "done" t0 t2
  print $ "ungrouped prefix count = " ++ show (length $ ungroup grouped)
  stopwatch "done" t0 t3

test4 = do
  let mapRib0 = emptyMapRib'
      build = buildUpdateSequence'
      q = query'
      d = dumpRib'
  t0 <- systime
  routes <- getRoutes
  let peerRoutes peer routes = map (\(rte, pfx) -> (routePrePendAS (peerAS peer) rte, pfx)) routes
        where
          routePrePendAS p r = r {pathAttributes = prePendAS p (pathAttributes r)}
      peer1Routes = peerRoutes peer1 routes
      peer2Routes = peerRoutes peer2 routes
      peer3Routes = peerRoutes peer3 routes
  t1 <- stopwatch "loaded rib" t0 t0
  let mapRib1 = build peer2 peer2Routes mapRib0
  q mapRib1
  t2 <- stopwatch "populated mapRib with peer 2" t0 t1

  let mapRib2 = build peer1 peer1Routes mapRib1
  q mapRib2
  t3 <- stopwatch "populated mapRib with peer 1" t0 t2

  let mapRib3 = build peer3 peer3Routes mapRib2
  q mapRib3
  t4 <- stopwatch "populated mapRib with peer 3" t0 t3

  let mapRib4 = build peer2 peer2Routes mapRib3
  q mapRib4
  t5 <- stopwatch "repopulated mapRib with peer 2" t0 t4

  let mapRib5 = build peer1 peer1Routes mapRib4
  q mapRib5
  t6 <- stopwatch "repopulated mapRib with peer 1" t0 t5

  let mapRib6 = build peer3 peer3Routes mapRib5
  q mapRib6
  t7 <- stopwatch "repopulated mapRib with peer 3" t0 t6

  d mapRib6
  t8 <- stopwatch "dumped locRib" t0 t7

  let mapRib7 = removePeerM peer3 mapRib6
  q mapRib7
  t9 <- stopwatch "removed peer 3" t0 t8

  let mapRib8 = build peer3 (take 10 peer3Routes) mapRib7
  q mapRib8
  t10 <- stopwatch "rerepopulated mapRib with peer 3 (10 routes)" t0 t9

  let mapRib9 = removePeerM peer3 mapRib8
  q mapRib9
  t11 <- stopwatch "reremoved peer 3" t0 t10

  return ()

{-

    let mapRib3 = removePeer_ peer2 mapRib2
    query mapRib3
    stopwatch "depopulated mapRib with pref peer" t0

    let mapRib2 = buildUpdateSequence peer3 (take 10 routes) mapRib
        mapRib2' = buildUpdateSequence' peer3 (take 10 routes) mapRib'
    query mapRib2
    stopwatch "populated mapRib with non-pref peer" t0
    let mapRib3 = removePeer_ peer3 mapRib2
    query mapRib3
    stopwatch "depopulated mapRib with non-pref peer" t0

-}

test1 = do
  putStrLn "test1 - read file with BGPReader(readRib)"
  t0 <- systime
  rib <- readRib
  let routes = map parseRibRoute rib
  t1 <- stopwatch "loaded rib" t0 t0
  -- shuffledRoutes <- shuffle rib
  -- t2 <- stopwatch "shuffled rib" t0 t1
  -- putStrLn $ "loaded rib in " ++ show (diffSystemTime t0 t1)
  putStrLn $ "got " ++ show (length rib) ++ " routes"
  print (last rib)
  stopwatch "printed from rib" t0 t1

test2 = do
  t0 <- systime
  contents <- L.getContents
  putStrLn $ "file length: " ++ show (L.length contents) ++ " bytes"
  t1 <- stopwatch "after file read" t0 t0
  let bgpByteStrings = runGet getBGPByteStrings contents
  putStrLn $ "BGP message count : " ++ show (length bgpByteStrings)
  t2 <- stopwatch "after wireformat parse" t0 t1
  let bgpMessages = map decodeBGPByteString bgpByteStrings
      updates = map BGPRib.parseUpdate $ filter isUpdate bgpMessages
  rib <- BGPRib.newRib BGPRib.dummyPeerData
  mapM_ (updateRib rib) updates
  stopwatch "after full (?) parse into rib" t0 t2

test3 = do
  t0 <- systime
  rib <- readRib
  putStrLn $ "rib length: " ++ show (length rib)
  t1 <- stopwatch "rib read duration " t0 t0
  grib <- readGroupedRib
  putStrLn $ "grib length: " ++ show (length grib)
  stopwatch "grib read duration " t0 t1

testN = do
  t0 <- systime
  rib <- readRib
  putStrLn $ "rib length: " ++ show (length rib)
  stopwatch "after rib read" t0 t0
