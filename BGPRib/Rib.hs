module BGPRib.Rib (Rib, FilterState, filterLookupManyRoutesMVar, ribPush, newRib, addPeer, delPeer, getPeersInRib, pullAllUpdates, getLocRib, getNextHops, getPeerAdjRIBOut, newFilterState) where

import BGPRib.AdjRIBOut
import BGPRib.BGPData
import BGPRib.Common (groupBySecond)
import qualified BGPRib.PT as PT (ptBest)
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPlib.BGPlib
import Control.Arrow (second)
import Control.Concurrent
import Control.Logger.Simple
import qualified Data.HashMap.Strict as Data.Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Text as T
import Data.Word (Word32)

type FilterState = IntSet

type AdjRIBOut = Data.Map.HashMap PeerData PeerAdjRIBOut

data Rib = Rib
  { locRIB :: MVar PrefixTable,
    adjRibOut :: MVar AdjRIBOut
  }

newRib :: PeerData -> IO Rib
newRib localPeer = do
  peerAdjRib <- newPeerAdjRIBOut
  adjRib <- newMVar (Data.Map.singleton localPeer peerAdjRib)
  locRIB <- newMVar newPrefixTable
  return $ Rib locRIB adjRib

getPeerAdjRIBOut :: PeerData -> Rib -> IO PeerAdjRIBOut
getPeerAdjRIBOut peer rib = withMVar (adjRibOut rib) (\adjribout -> return $ adjribout Data.Map.! peer)

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
  adjRib <- readMVar (adjRibOut rib)
  return $ Data.Map.keys adjRib

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = do
  withdraws <- modifyMVar (locRIB rib) (\rib -> return $ withdrawPeer rib peer)
  modifyMVar_
    (adjRibOut rib)
    ( \adjrib -> do
        -- schedule the withdraw dissemination
        -- NOTE - this does not change the AdjRIBMap
        updateRibOutWithPeerData withdraws adjrib
        return $ (Data.Map.delete peer adjrib)
    )

addPeer :: Rib -> PeerData -> IO PeerAdjRIBOut
addPeer rib peer =
  do
    ribDump <-
      withMVar
        (locRIB rib)
        ( \rib ->
            do
              let f (rd, pfxs) = (pfxs, routeId rd)
              -- get a complete RIB dump for the new peer...
              return $ map f (PrefixTableUtils.getAdjRIBOut rib)
        )

    -- make the RIB dump into a Fifo
    aro <- mkFifo ribDump
    modifyMVar_ (adjRibOut rib) (\adjrib -> do return $ Data.Map.insert peer aro adjrib)
    return aro

{- foldf / foldmf
    Generic combinator/sequencers for state threaded working
    Perhaps State m might do the same, but all I seek is foldM
 -}
foldmf :: (s -> a -> (s, Maybe b)) -> s -> [a] -> (s, [b])
foldmf f s = foldl' f' (s, [])
  where
    f' (state, acc) item = let (state', result) = f state item in maybe (state', acc) (\r -> (state', r : acc)) result

foldf :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
foldf f s = foldl' f' (s, [])
  where
    f' (state, acc) item = let (state', result) = f state item in (state, result : acc)

filterLookupRoutesMVar :: Rib -> FilterState -> PeerData -> PathChange -> IO (FilterState, Maybe (RouteData, [Prefix]))
filterLookupRoutesMVar rib state peer pathchange =
  withMVar
    (locRIB rib)
    (\rib -> return $ filterLookupRoutes peer rib state pathchange)

filterLookupManyRoutesMVar :: Rib -> FilterState -> PeerData -> [PathChange] -> IO (FilterState, [(RouteData, [Prefix])])
filterLookupManyRoutesMVar rib state peer pathchange =
  withMVar
    (locRIB rib)
    (\rib -> return $ filterLookupManyRoutes peer rib state pathchange)

newFilterState :: FilterState
newFilterState = IntSet.empty

filterLookupManyRoutes peer locrib = foldmf (filterLookupRoutes peer locrib)

filterLookupRoutes :: PeerData -> PrefixTable -> FilterState -> PathChange -> (FilterState, Maybe (RouteData, [Prefix]))
filterLookupRoutes peerData _ s (prefixes, 0) = (s, Just (NullRoute, prefixes))
filterLookupRoutes peerData _ s (prefixes, -1) = (s, Just (Withdraw peerData, prefixes))
filterLookupRoutes peer locrib state (prefixes, pathref) = (state, phase1 prefixes)
  where
    get :: Prefix -> Maybe RouteData
    get pfx = (PT.ptBest (fromPrefix pfx) locrib)

    phase1 :: [Prefix] -> Maybe (RouteData, [Prefix])
    phase1 [] = Nothing
    phase1 (pfx : pfxs) = maybe (phase1 pfxs) (phase1a pfx pfxs) (get pfx)
    phase1a pfx pfxs route = if pathref == (routeHash route) then phase2 [pfx] route pfxs else phase1 pfxs

    phase2 :: [Prefix] -> RouteData -> [Prefix] -> Maybe (RouteData, [Prefix])
    phase2 acc route [] = Just (route, acc)
    phase2 acc route (pfx : pfxs) = if (Just route) == (get pfx) then phase2 (pfx : acc) route pfxs else phase2 acc route pfxs

filterCheck :: FilterState -> Prefix -> Bool -> (FilterState, Bool)
filterCheck state prefix True = (IntSet.insert (fromPrefix prefix) state, IntSet.member (fromPrefix prefix) state)
filterCheck state prefix False = (IntSet.delete (fromPrefix prefix) state, IntSet.member (fromPrefix prefix) state)

-- filterCheck state prefix isUpdate = (state', isAnnounced)
--   where
--     state' = state
--     isAnnounced = True
-- lookupRoutes :: Rib -> FilterState -> PeerData -> PathChange -> IO (FilterState, Maybe (RouteData, [Prefix]))
-- -- lookupRoutes :: Rib -> PeerData -> PathChange -> IO (Maybe (RouteData, [Prefix]))
-- --- objective:
-- --- The objective is to filter the input prefixes so that all remaining have the property that the route identified is still best for that prefix.
-- --- Additionally, a copy of that (common) route is needed for further processing.
-- --- narrative:
-- ---    Drop from the head of the list whilst the prperty is not satisfied,
-- ---    Return the first found result, reducing the remaining list with a simpler similar filter taht does not capture the RouteData returned by the lookup

-- lookupRoutes _ s peerData (prefixes, 0) = return $ (s, Just (Withdraw peerData, prefixes))
-- lookupRoutes _ s peerData (prefixes, -1) = return $ (s, Just (Withdraw peerData, prefixes))
-- lookupRoutes rib s target (prefixes, hash) =
--   withMVar
--     (locRIB rib)
--     ( \locrib -> do
--         let hashMatch route = hash == (routeHash route)
--             hashMatch_ route
--               | hashMatch route = Just route
--               | otherwise = Nothing
--             reduce :: [Prefix] -> Maybe (RouteData, [Prefix])
--             reduce [] = Nothing
--             reduce (a : ax) = maybe (reduce ax) (\route -> Just (route, a : (filter check ax))) (get a)
--             check :: Prefix -> Bool
--             check = isJust . get
--             get :: Prefix -> Maybe RouteData
--             get pfx = (PT.ptBest (fromPrefix pfx) locrib) >>= hashMatch_

--         return $
--           ( s,
--             fmap
--               ( \(route, remainingPrefixes) ->
--                   if exportFilter target route
--                     then (route, remainingPrefixes)
--                     else (NullRoute, remainingPrefixes)
--               )
--               (reduce prefixes)
--           )
--     )

{- export filter rationale
  the context of an export filter is the route itself and the respective source and target peers
  simple checks are purely peer related.., but the source peer is part of the Route, so the route and target peer are always the parameters for the test
-}
exportFilter :: PeerData -> RouteData -> Bool
exportFilter target route@RouteData {} = iBGPRelayCheck && noReturnCheck
  where
    iBGPRelayCheck = fromEBGP route || isExternal target
    noReturnCheck = target /= peerData route
exportFilter _ NullRoute = undefined
exportFilter _ Withdraw {} = undefined

getNextHops :: Rib -> [Prefix] -> IO [(Prefix, Maybe IPv4)]
getNextHops rib prefixes =
  withMVar
    (locRIB rib)
    ( \locrib ->
        return $
          map
            (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTable locrib prefix)
            prefixes
    )

pullAllUpdates :: PeerAdjRIBOut -> IO [PathChange]
pullAllUpdates adjrib = dequeueAll adjrib

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = readMVar $ locRIB rib

evalLocalPref :: PeerData -> [PathAttribute] -> [Prefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO ()
{-
--- narrative
composition of
    a) the LocRIB insertion process, which modifies LocRIB and returns sets of changes [modifyMVar]
    b) the path change dissemination into the peer adjrib fifos [modifyMVar_]
-}
ribPush _ _ NullUpdate = logDebug "ribPush NullUpdate"
ribPush rib peerData ParsedUpdate {..} = do
  logDebug $ T.pack $ "ribPush " ++ show peerData ++ " - "
  pathChangesA <- modifyMVar (locRIB rib) (ribUpdateMany peerData puPathAttributes hash nlri)
  pathChangesB <- modifyMVar (locRIB rib) (ribWithdrawMany peerData withdrawn)
  -- possibly there is some race condition here due to releasing the RIB before inserting in the adjribs
  -- BUT I don't think so because when the adjribout is worked the ordering doesn't matter - stale changes are dropped....
  withMVar (adjRibOut rib) (updateRibOutWithPeerData pathChangesA)
  withMVar (adjRibOut rib) (updateRibOutWithPeerData pathChangesB)
  where
    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> PrefixTable -> IO (PrefixTable, [(Prefix, RouteData)])
    ribUpdateMany peerData pathAttributes routeId pfxs locRIB
      | null pfxs = do
        logDebug "ribUpdateMany []"
        return $ (locRIB, [])
      | otherwise = do
        logDebug . T.pack $ "ribUpdateMany " ++ show pfxs
        localPref <- evalLocalPref peerData pathAttributes pfxs
        let routeData = makeRouteData peerData pathAttributes routeId localPref
            routeData' = if importFilter routeData then (Withdraw peerData) else routeData
            (locRIB', updates) = BGPRib.PrefixTable.update locRIB pfxs routeData'
        logDebug . T.pack $ "ribUpdateMany " ++ show (length updates) ++ " changes"
        return $ (locRIB', updates)

    ribWithdrawMany :: PeerData -> [Prefix] -> PrefixTable -> IO (PrefixTable, [(Prefix, RouteData)])
    ribWithdrawMany peerData pfxs locRIB
      | null pfxs = do
        logDebug "ribWithdrawMany []"
        return $ (locRIB, [])
      | otherwise = do
        logDebug . T.pack $ "ribWithdrawMany " ++ show pfxs
        let (locRIB', withdraws) = BGPRib.PrefixTable.update locRIB pfxs (Withdraw peerData)
        logDebug . T.pack $ "ribWithdrawMany " ++ show (length withdraws) ++ " changes"
        return $ (locRIB', withdraws)

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
      where
        (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
        fromEBGP = isExternal peerData
        med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: [(Prefix, RouteData)] -> AdjRIBOut -> IO ()
updateRibOutWithPeerData updates adjRIB =
  sequence_ $ Data.Map.mapWithKey action adjRIB
  where
    action :: PeerData -> PeerAdjRIBOut -> IO ()
    action targetPeer = insertPathChanges $ groupBySecond $ map (second routeId) updates

importFilter :: RouteData -> Bool
importFilter route@RouteData {} = pathLoopCheck route
  where
    pathLoopCheck r = elemASPath (myAS $ globalData $ peerData r) (pathAttributes r)
importFilter _ = error "importFilter only defined for updates"
