{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BGPRib.Rib (Rib, ribPush, newRib, getLocRib, addPeer, delPeer, getPeersInRib, lookupRoutes, pullAllUpdates, getNextHops) where

import BGPRib.AdjRIBOut
import BGPRib.BGPData
import BGPRib.Common (groupBySecond)
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPlib.BGPlib
import Control.Arrow (second)
import Control.Concurrent
import Control.Monad (when)
import Data.List (nub)
import qualified Data.Map.Strict as Data.Map
import Data.Maybe (fromJust)
import Data.Word (Word32)

type Rib = MVar Rib'

type AdjRIB = Data.Map.Map PeerData AdjRIBTable

data Rib' = Rib'
  { prefixTable :: PrefixTable,
    adjRib :: AdjRIB
  }

newRib :: PeerData -> IO Rib
newRib localPeer = do
  adjRib <- newAdjRIBTable
  newMVar $ Rib' newPrefixTable (Data.Map.singleton localPeer adjRib)

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
  (Rib' _ adjRib) <- readMVar rib
  return $ Data.Map.keys adjRib

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyMVar_ rib (delPeer'' peer)
  where
    delPeer'' :: PeerData -> Rib' -> IO Rib'
    delPeer'' peer Rib' {..} = do
      let prefixes = getPeerPrefixes prefixTable peer
          (prefixTable', withdraws) = BGPRib.PrefixTable.updateC prefixTable prefixes (Withdraw peer)
      mapM_ (updatePeer adjRib) withdraws
      return $ Rib' prefixTable' (Data.Map.delete peer adjRib)

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyMVar_ rib (addPeer' peer)
  where
    addPeer' :: PeerData -> Rib' -> IO Rib'
    addPeer' peer Rib' {..} = do
      let ribDump = map f (PrefixTableUtils.getAdjRIBOut prefixTable)
          f (rd, pfxs) = (pfxs, routeId rd)
      aro <- fifo ribDump
      let adjRib' = Data.Map.insert peer aro adjRib
      return $ Rib' prefixTable adjRib'

lookupRoutes :: Rib -> AdjRIBEntry -> IO (Maybe (RouteData, [Prefix]))
lookupRoutes rib (prefixes, routeHash) = do
  rib' <- readMVar rib
  let myLookup = map (\pfx -> (queryPrefixTableC (prefixTable rib') pfx, pfx))
      myFilter = id
      unchangedPrefixes = myFilter $ myLookup prefixes
      route = fst $ head unchangedPrefixes -- safe becuase only called after test for null
  return $ if null unchangedPrefixes then Nothing else Just (route, map snd unchangedPrefixes)

getNextHops :: Rib -> [Prefix] -> IO [(Prefix, Maybe IPv4)]
getNextHops rib prefixes = do
  rib' <- readMVar rib
  return $
    map
      (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTableC (prefixTable rib') prefix)
      prefixes

pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
  (Rib' _ arot) <- readMVar rib
  maybe (return []) dequeueAll (arot Data.Map.!? peer)

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
  rib' <- readMVar rib
  return (prefixTable rib')

evalLocalPref :: PeerData -> [PathAttribute] -> [Prefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

checkPoison :: PeerData -> [PathAttribute] -> [Prefix] -> IO Bool
checkPoison peerData pathAttributes _ = return $ elemASPath 666 pathAttributes

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO ()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)
  where
    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate {..} rib = ribUpdateMany peerData puPathAttributes hash nlri rib >>= ribWithdrawMany peerData withdrawn
    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables)
      | null pfxs = return (Rib' prefixTable adjRibOutTables)
      | otherwise = do
          localPref <- evalLocalPref peerData pathAttributes pfxs
          poisoned <- checkPoison peerData pathAttributes pfxs
          when poisoned (putStrLn ("poisoned route detected " ++ show peerData ++ " " ++ show pfxs))
          let routeData = makeRouteData peerData pathAttributes routeId 100 poisoned
              routeData' = if importFilter routeData then trace "importFilter: filtered" $ Withdraw peerData else routeData
              (!prefixTable', !updates) = BGPRib.PrefixTable.updateC prefixTable pfxs routeData
              reducedUpdates = reduce updates

          mapM_ (updateAllPeers adjRibOutTables) reducedUpdates
          return $ Rib' prefixTable' adjRibOutTables
      where
        reduce :: [(PeerData, RouteData, [Prefix])] -> [(RouteData, [Prefix])]
        reduce = nub . map (\(_, a, b) -> (a, b))

        makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> Bool -> RouteData
        makeRouteData peerData pathAttributes routeHash overrideLocalPref poisoned = RouteData {..}
          where
            (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
            fromEBGP = isExternal peerData
            med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
            localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
            nextHop = getNextHop pathAttributes
            origin = getOrigin pathAttributes

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
      | null pfxs = return (Rib' prefixTable adjRibOutTables)
      | otherwise = do
          let (!prefixTable', !withdraws) = BGPRib.PrefixTable.updateC prefixTable pfxs (Withdraw peerData)
          mapM_ (updatePeer adjRibOutTables) withdraws
          return $ Rib' prefixTable' adjRibOutTables

updatePeer :: AdjRIB -> (PeerData, RouteData, [Prefix]) -> IO ()
updatePeer adjRib (peer, route, prefixes) = insertAdjRIBTable (prefixes, routeHash route) (fromJust $ Data.Map.lookup peer adjRib)

updateAllPeers :: AdjRIB -> (RouteData, [Prefix]) -> IO ()
updateAllPeers adjRib (route, prefixes) = mapM_ (insertAdjRIBTable (prefixes, routeHash route)) fifos
  where
    fifos = Data.Map.elems adjRib

updateRibOutWithPeerData :: PeerData -> [(Prefix, RouteData)] -> AdjRIB -> IO ()
updateRibOutWithPeerData triggerPeer updates adjRIB =
  sequence_ $ Data.Map.mapWithKey action adjRIB
  where
    action :: PeerData -> AdjRIBTable -> IO ()
    action targetPeer = insertNAdjRIBTable (f updates)
      where
        f0 :: [(Prefix, RouteData)] -> [(Prefix, Int)]
        f0 = map (second routeId)
        f1 :: [(Prefix, Int)] -> [([Prefix], Int)]
        f1 = groupBySecond
        f :: [(Prefix, RouteData)] -> [([Prefix], Int)]
        f = f1 . f0 . applyExportFilter exportFilter
        applyExportFilter :: (PeerData -> PeerData -> RouteData -> RouteData) -> [(Prefix, RouteData)] -> [(Prefix, RouteData)]
        applyExportFilter xf = map (\(pfx, rd) -> (pfx, xf triggerPeer targetPeer rd))

importFilter :: RouteData -> Bool
importFilter route@RouteData {} = pathLoopCheck route
  where
    pathLoopCheck r = elemASPath (myAS $ globalData $ peerData r) (pathAttributes r)
importFilter _ = error "importFilter only defined for updates"

exportFilter :: PeerData -> PeerData -> RouteData -> RouteData
exportFilter trigger target route@RouteData {} = if checks then route else trace "export filtered" $ Withdraw undefined
  where
    checks = iBGPRelayCheck && noReturnCheck
    iBGPRelayCheck = fromEBGP route || isExternal target
    noReturnCheck = target /= peerData route
exportFilter trigger target NullRoute = trace "export filter applied to null route" NullRoute
exportFilter trigger target Withdraw {} =
  if checks
    then trace "export withdraw allowed" $ Withdraw undefined
    else trace "export withdraw filtered" NullRoute
  where
    checks = iBGPRelayCheck && noReturnCheck
    iBGPRelayCheck = isExternal trigger || isExternal target
    noReturnCheck = target /= trigger
