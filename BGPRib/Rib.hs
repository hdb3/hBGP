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
import Data.IP
import qualified Data.Map.Strict as Data.Map
import Data.Word (Word32)

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
delPeer rib peer = modifyMVar_ rib (delPeer' peer)
  where
    delPeer' :: PeerData -> Rib' -> IO Rib'
    delPeer' peer Rib' {..} = do
      -- drain the prefix table and save the resulting withdraws
      let (prefixTable', prefixes) = withdrawPeer prefixTable peer
      -- schedule the withdraw dissemination
      -- NOTE - this does not change the AdjRIBMap
      updateRibOutWithPeerData peer prefixes adjRib
      -- now remove this peer completely from the AdjRIBMap
      -- it is liekly that this could be done before the previous action.....
      -- but the semantics should be identical as long as we didn't try to send withdraw messages to the peer which has gone away...
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
      aro <- fmap AdjRIBTable (mkFifo ribDump)
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
  let myLookup = map (\pfx -> (queryPrefixTable (prefixTable rib') pfx, pfx))
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
      (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTable (prefixTable rib') prefix)
      prefixes

pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
  (Rib' _ arot) <- readMVar rib
  maybe (return []) dequeueAll (fmap fifo (arot Data.Map.!? peer))

-- TODO write and use the function 'getAdjRibForPeer'

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
  rib' <- readMVar rib
  return (prefixTable rib')

evalLocalPref :: PeerData -> [PathAttribute] -> [Prefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

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
          let routeData = makeRouteData peerData pathAttributes routeId localPref
              routeData' = if importFilter routeData then Withdraw peerData else routeData
              (!prefixTable', !updates) = BGPRib.PrefixTable.update prefixTable pfxs routeData'
          updateRibOutWithPeerData peerData updates adjRibOutTables
          return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
      | null pfxs = return (Rib' prefixTable adjRibOutTables)
      | otherwise = do
          let (!prefixTable', !withdraws) = BGPRib.PrefixTable.update prefixTable pfxs (Withdraw peerData)
          updateRibOutWithPeerData peerData withdraws adjRibOutTables
          return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
      where
        (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
        fromEBGP = isExternal peerData
        med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: PeerData -> [(Prefix, RouteData)] -> AdjRIB -> IO ()
updateRibOutWithPeerData triggerPeer updates adjRIB =
  sequence_ $ Data.Map.mapWithKey action adjRIB
  where
    action :: PeerData -> AdjRIBTable -> IO ()
    action targetPeer = insertNAdjRIBTable exports
      where
        f0 :: [(Prefix, RouteData)] -> [(Prefix, Int)]
        f0 = map (second routeId)
        f1 :: [(Prefix, Int)] -> [([Prefix], Int)]
        f1 = groupBySecond
        exports = f1 . f0 $ updates
        -- NB - remove export filter, should only be applied, if stateful, on export
        -- exports = f1 . f0 . applyExportFilter exportFilter $ updates
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
exportFilter trigger target route@RouteData {} = if checks then route else Withdraw undefined
  where
    checks = iBGPRelayCheck && noReturnCheck
    iBGPRelayCheck = fromEBGP route || isExternal target
    noReturnCheck = target /= peerData route
exportFilter trigger target NullRoute = NullRoute
exportFilter trigger target Withdraw {} = if checks then Withdraw undefined else NullRoute
  where
    checks = iBGPRelayCheck && noReturnCheck
    iBGPRelayCheck = isExternal trigger || isExternal target
    noReturnCheck = target /= trigger
