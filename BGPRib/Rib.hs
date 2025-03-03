{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BGPRib.Rib (Rib, ribPush, newRib, getLocRib, addPeer, delPeer, getPeersInRib, lookupRoutes, pullAllUpdates, getNextHops) where

-- import Debug.Trace

import BGPRib.AdjRIBOut
import BGPRib.BGPData
import BGPRib.Common (groupBySecond)
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPlib.BGPlib
import Control.Arrow (second)
import Control.Concurrent
import Control.Monad (when)
import Data.IP
import Data.List (nub)
import qualified Data.Map.Strict as Data.Map
import Data.Maybe (fromJust)
import Data.Word (Word32)

-- trace _ x = x

type Rib = MVar Rib'

-- TODO rename AdjRIB -> AdjRIBMap
-- and create a type 'AdjRIBMapEntry = (PeerData,AdjRIBTable)'

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
    -- -- master
    -- delPeer' :: PeerData -> Rib' -> IO Rib'
    -- delPeer' peer Rib' {..} = do
    --   let (prefixTable', prefixes) = withdrawPeer prefixTable peer
    --   updateRibOutWithPeerData peer prefixes adjRib
    --   return $ Rib' prefixTable' (Data.Map.delete peer adjRib)
    -- from Controller
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
      -- get a complete RIB dump for the new peer...
      let ribDump = map f (PrefixTableUtils.getAdjRIBOut prefixTable)
          f (rd, pfxs) = (pfxs, routeId rd)
      -- make the RIB dump into a Fifo
      aro <- fifo ribDump
      -- TODO - this would be the place to insert an end-of-rib marker
      let adjRib' = Data.Map.insert peer aro adjRib
      return $ Rib' prefixTable adjRib'

{-
    delayed route look-up logic

    This function performs three functions:
       takes routeIDs and returns full RouteData objects
       checks that the route did not change since the update was originally scheduled
       drops the update for the case where it has changed.

    redux -

    regrading filters - optimal correct behaviour requires either preserved state from last sent route or recalculated equivalent.
    unsolicited withdraw would allow safe behaviour - but this is only needed in the context of export filter capability, and can easily be implmented
    when that capability is built.

    an alternate implementation might simply return the latest route, and mark that prefix/route combination as sent using a sequence number
    then later queued updates can be ignored.  Whether that is better is unclear, but it is more complex, and so not taken up for now.
    A superficial analysis argues tgat defrred transmission is sensible, because updates for churning prefixes would thereby be deferred in favour of stable ones.
-}

lookupRoutes :: Rib -> AdjRIBEntry -> IO (Maybe (RouteData, [Prefix]))
lookupRoutes rib (prefixes, routeHash) = do
  rib' <- readMVar rib
  let myLookup = map (\pfx -> (queryPrefixTableC (prefixTable rib') pfx, pfx))
      -- myFilter = filter ((routeHash ==) . routeId .fst )
      -- the following line removes the suppress changed routes check
      -- which is essential for filters to work correctly
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
-- TODO - look into why this is sometimes called with a peer not in the map...
-- this happens at peer down and the suspicion is that it implies that
-- there may be some withdrawals which are improperly dropped
pullAllUpdates peer rib = do
  (Rib' _ arot) <- readMVar rib
  -- dequeueAll (arot Data.Map.! peer) -- from Controller
  maybe (return []) dequeueAll (arot Data.Map.!? peer)

-- TODO write and use the function 'getAdjRibForPeer'

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
    -- ribPush' peerData ParsedUpdate{..} rib0 =

    --     ribUpdateMany peerData puPathAttributes hash nlri rib0 >>= ribWithdrawMany peerData withdrawn

    ribPush' peerData ParsedUpdate {..} rib = ribUpdateManyC peerData puPathAttributes hash nlri rib >>= ribWithdrawManyC peerData withdrawn
    -- ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    -- ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables)
    --   | null pfxs = return (Rib' prefixTable adjRibOutTables)
    --   | otherwise = do
    --     localPref <- evalLocalPref peerData pathAttributes pfxs
    --     let routeData = makeRouteData peerData pathAttributes routeId localPref
    --         routeData' = if importFilter routeData then trace "importFilter: filtered" $ Withdraw peerData else routeData
    --         (!prefixTable', !updates) = BGPRib.PrefixTable.update prefixTable pfxs routeData'
    --     updateRibOutWithPeerData peerData updates adjRibOutTables
    --     return $ Rib' prefixTable' adjRibOutTables
    --   where
    --     makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    --     makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
    --       where
    --         (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
    --         fromEBGP = isExternal peerData
    --         med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
    --         localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
    --         nextHop = getNextHop pathAttributes
    --         origin = getOrigin pathAttributes
    --
    ribUpdateManyC :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateManyC peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables)
      | null pfxs = return (Rib' prefixTable adjRibOutTables)
      | otherwise = do
          localPref <- evalLocalPref peerData pathAttributes pfxs
          poisoned <- checkPoison peerData pathAttributes pfxs
          when poisoned (putStrLn ("poisoned route detected " ++ show peerData ++ " " ++ show pfxs))
          let routeData = makeRouteData peerData pathAttributes routeId 100 poisoned
              routeData' = if importFilter routeData then trace "importFilter: filtered" $ Withdraw peerData else routeData
              (!prefixTable', !updates) = BGPRib.PrefixTable.updateC prefixTable pfxs routeData
              reducedUpdates = reduce updates

          -- this version is the minimal update applicable in the ADDPATH case
          -- mapM_ (updatePeer adjRibOutTables) updates

          -- this version is the promicuous update applicable in the best-external case
          mapM_ (updateAllPeers adjRibOutTables) reducedUpdates
          return $ Rib' prefixTable' adjRibOutTables
      where
        --
        -- reduce simply discards the per-peer updates
        -- DANGER - if there were both withdraws and updates in this list for different peers then this would not work
        -- CONCLUSION - best external model should not use this return value, rather it should use the default signature...
        reduce :: [(PeerData, RouteData, [Prefix])] -> [(RouteData, [Prefix])]
        -- generally there is a 1:1 relation between in and out for this function, however we can't assume that
        reduce = nub . map (\(_, a, b) -> (a, b))
        --
        makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> Bool -> RouteData
        makeRouteData peerData pathAttributes routeHash overrideLocalPref poisoned = RouteData {..}
          where
            (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
            fromEBGP = isExternal peerData
            med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
            localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
            nextHop = getNextHop pathAttributes
            origin = getOrigin pathAttributes

    -- ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    -- ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
    --   | null pfxs = return (Rib' prefixTable adjRibOutTables)
    --   | otherwise = do
    --     let (!prefixTable', !withdraws) = BGPRib.PrefixTable.update prefixTable pfxs (Withdraw peerData)
    --     updateRibOutWithPeerData peerData withdraws adjRibOutTables
    --     return $ Rib' prefixTable' adjRibOutTables
    ribWithdrawManyC :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawManyC peerData pfxs (Rib' prefixTable adjRibOutTables)
      | null pfxs = return (Rib' prefixTable adjRibOutTables)
      | otherwise = do
          let (!prefixTable', !withdraws) = BGPRib.PrefixTable.updateC prefixTable pfxs (Withdraw peerData)
          mapM_ (updatePeer adjRibOutTables) withdraws
          return $ Rib' prefixTable' adjRibOutTables

-- CONTROLLER
updatePeer :: AdjRIB -> (PeerData, RouteData, [Prefix]) -> IO ()
updatePeer adjRib (peer, route, prefixes) = insertAdjRIBTable (prefixes, routeHash route) (fromJust $ Data.Map.lookup peer adjRib)

-- -- insert the given route update into the update fifos of all peers
updateAllPeers :: AdjRIB -> (RouteData, [Prefix]) -> IO ()
updateAllPeers adjRib (route, prefixes) = mapM_ (insertAdjRIBTable (prefixes, routeHash route)) fifos
  where
    fifos = Data.Map.elems adjRib

updateRibOutWithPeerData :: PeerData -> [(Prefix, RouteData)] -> AdjRIB -> IO ()
updateRibOutWithPeerData triggerPeer updates adjRIB =
  sequence_ $ Data.Map.mapWithKey action adjRIB
  where
    -- reminder: type AdjRIB = Data.Map.Map PeerData AdjRIBTable
    --         : insertNAdjRIBTable :: [([Prefix],Int)] -> AdjRIBTable -> IO ()
    --
    --         - applying an action to every AdjRIBTable in an 'AdjRIB' is achived by
    --         -    sequence_ $ Data.Map.map action
    --         - where 'action' is  AdjRIBTable -> IO ()
    --         - If the requirement is to execute an action which is sependent upon the peer context then
    --         -    sequence_ $ Data.Map.mapWithKey action'
    --         - is required, where 'action'' is PeerData -> AdjRIBTable -> IO ()

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

-- ## TODO - consider whether WIthdraw constructor should carry PeerData at all....
-- (the use is for input to RIB, not export (RIB never exports Withdraw, ony NullRoute))

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
