{-# LANGUAGE RecordWildCards, TupleSections, BangPatterns #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates,getNextHops) where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Data.Word(Word32)
import Data.IP
import Data.Maybe(isJust)
import Control.Arrow(second)
import BGPlib.BGPlib

import BGPRib.BGPData
import BGPRib.PrefixTable
import qualified BGPRib.PT as PT (ptBest)
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPRib.AdjRIBOut
import BGPRib.Common(groupBySecond)

type Rib = MVar Rib'

type AdjRIB = Data.Map.Map PeerData AdjRIBTable
data Rib' = Rib' { prefixTable :: PrefixTable
                 , adjRib :: AdjRIB }

newRib :: PeerData -> IO Rib
newRib localPeer = do
    adjRib <- newAdjRIBTable
    newMVar $ Rib' newPrefixTable ( Data.Map.singleton localPeer adjRib )

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
    (Rib' _ adjRib ) <- readMVar rib
    return $ Data.Map.keys adjRib

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyMVar_ rib ( delPeer' peer )

    where

    delPeer' :: PeerData -> Rib' -> IO Rib'
    delPeer' peer Rib' {..} = do
        -- drain the prefix table and save the resulting withdraws
        let (prefixTable',prefixes) = withdrawPeer prefixTable peer
        -- schedule the withdraw dissemination
        -- NOTE - this does not change the AdjRIBMap
        updateRibOutWithPeerData prefixes adjRib
        -- now remove this peer completely from the AdjRIBMap
        -- it is liekly that this could be done before the previous action.....
        -- but the semantics should be identical as long as we didn't try to send withdraw messages to the peer which has gone away...
        return $ Rib' prefixTable' ( Data.Map.delete peer adjRib )

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyMVar_ rib ( addPeer' peer )

    where

    addPeer' :: PeerData -> Rib' -> IO Rib'
    addPeer' peer Rib' {..} = do
            -- get a complete RIB dump for the new peer...
        let ribDump = map f (PrefixTableUtils.getAdjRIBOut prefixTable)
            f (rd,pfxs) = (pfxs , routeId rd)
            -- make the RIB dump into a Fifo
        aro <- fmap AdjRIBTable (mkFifo ribDump)
            -- TODO - this would be the place to insert an end-of-rib marker
        let adjRib' = Data.Map.insert peer aro adjRib
        return $ Rib' prefixTable adjRib'

lookupRoutes :: Rib -> AdjRIBEntry -> IO (Maybe (RouteData,[Prefix]))
--- objective:
--- The objective is to filter the input prefixes so that all remaining have the property that the route identified is still best for that prefix.
--- Additionally, a copy of that (common) route is needed for further processing.
--- narrative:
---    Drop from the head of the list whilst the prperty is not satisfied,
---    Return the first found result, reducing the remaining list with a simpler similar filter taht does not capture the RouteData returned by the lookup

lookupRoutes rib (prefixes, 0) = return $ Just (Withdraw undefined, prefixes)
lookupRoutes rib (prefixes, -1) = return $ Just (Withdraw undefined, prefixes)
lookupRoutes rib (prefixes,hash) = do
        rib' <- readMVar rib
        let 
            hashMatch route = hash == (routeHash route)
            hashMatch_ route | hashMatch route = Just route
                             | otherwise = Nothing 
            reduce :: [Prefix] -> Maybe (RouteData, [Prefix])
            reduce [] = Nothing 
            reduce (a : ax) = maybe (reduce ax) (\route -> Just (route, a : (filter check ax))) (get a)
            check :: Prefix -> Bool
            check = isJust . get
            get :: Prefix -> Maybe RouteData
            get pfx = (PT.ptBest (fromPrefix pfx) (prefixTable rib')) >>= hashMatch_
            result = reduce prefixes
        return $ reduce prefixes

getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    return $ map
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

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)

    where

    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate{..} rib = ribUpdateMany peerData puPathAttributes hash nlri rib >>= ribWithdrawMany peerData withdrawn

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              localPref <- evalLocalPref peerData pathAttributes pfxs
              let routeData = makeRouteData peerData pathAttributes routeId localPref
                  routeData' = if importFilter routeData then Withdraw peerData else routeData
                  ( !prefixTable' , !updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData'
              updateRibOutWithPeerData updates adjRibOutTables
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( !prefixTable' , !withdraws ) = BGPRib.PrefixTable.update prefixTable pfxs (Withdraw peerData)
            updateRibOutWithPeerData withdraws adjRibOutTables
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

updateRibOutWithPeerData :: [(Prefix,RouteData)] -> AdjRIB -> IO ()
updateRibOutWithPeerData updates adjRIB = 
    sequence_ $ Data.Map.mapWithKey action adjRIB

    where 
        action :: PeerData -> AdjRIBTable -> IO ()
        action targetPeer = insertNAdjRIBTable $ groupBySecond $ map (second routeId) updates

importFilter :: RouteData -> Bool
importFilter route@RouteData{} = pathLoopCheck route where
    pathLoopCheck r = elemASPath (myAS $ globalData $ peerData r) (pathAttributes r)
importFilter _ = error "importFilter only defined for updates"

-- -- (TDOD) in future, exportFilter will be used for processing in the output leg (post FiFo)
-- exportFilter :: PeerData -> PeerData -> RouteData -> RouteData
-- exportFilter trigger target route@RouteData {} = if checks then route else Withdraw undefined where
--     checks = iBGPRelayCheck && noReturnCheck
--     iBGPRelayCheck = fromEBGP route || isExternal target
--     noReturnCheck = target /= peerData route
-- exportFilter trigger target NullRoute = NullRoute
-- exportFilter trigger target Withdraw{} = if checks then Withdraw undefined else NullRoute where
--     checks = iBGPRelayCheck && noReturnCheck
--     iBGPRelayCheck = isExternal trigger || isExternal target
--     noReturnCheck = target /= trigger
