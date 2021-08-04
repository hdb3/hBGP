{-# LANGUAGE RecordWildCards, TupleSections, BangPatterns #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates,getNextHops) where
import Control.Concurrent
import qualified Data.HashMap.Strict as Data.Map
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

type AdjRIBOut = Data.Map.HashMap PeerData ( MVar PeerAdjRIBOut)
data Rib' = Rib' { locRIB :: PrefixTable
                 , adjRibOut :: MVar AdjRIBOut }

newRib :: PeerData -> IO Rib
newRib localPeer = do
    -- in future newPeerAdjRIBOut is a pure function, but not yet....
    peerAdjRib <- newPeerAdjRIBOut
    peerAdjRibMVar <- newMVar peerAdjRib
    let newAdjRIBOut = Data.Map.singleton localPeer peerAdjRibMVar
    newAdjRIBOutMVar <- newMVar newAdjRIBOut
    let rib = Rib' newPrefixTable newAdjRIBOutMVar
    newMVar rib

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
    (Rib' _ adjRib ) <- readMVar rib
    adjRib' <- readMVar adjRib
    return $ Data.Map.keys adjRib'

readAdjRIBOut :: Rib -> IO AdjRIBOut
readAdjRIBOut rib = do
  (Rib' _ adjRib) <- readMVar rib
  readMVar adjRib

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyMVar_ rib ( delPeer' peer )

    where

    delPeer' :: PeerData -> Rib' -> IO Rib'
    delPeer' peer Rib' {..} = do

        -- first take the peer out of the visible RIB, avoid any further updates being scheduled...
        -- we can discard its content, as we won't be talking to it again....
        adjRibOut' <- takeMVar adjRibOut
        let adjRibOut'' = Data.Map.delete peer adjRibOut'
        putMVar adjRibOut adjRibOut''

        -- drain the prefix table and save the resulting changes
        let (locRIB',changes) = withdrawPeer locRIB peer

        -- now, schedule the change dissemination, using the reduced new AdjRIB
        updateAdjRIBOut changes adjRibOut''


        return $ Rib' locRIB' adjRibOut

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyMVar_ rib ( addPeer' peer )

    where

    addPeer' :: PeerData -> Rib' -> IO Rib'
    addPeer' peer Rib' {..} = do
            -- get a complete RIB dump for the new peer...
        let ribDump = map f (PrefixTableUtils.getAdjRIBOut locRIB)
            f (rd,pfxs) = (pfxs , routeId rd)
        -- make the RIB dump into a Fifo
        -- TODO - this would be the place to insert an end-of-rib marker
        peerAdjRib <- fmap PeerAdjRIBOut (mkFifo ribDump)
        -- and wrap it in an MVar
        peerAdjRibMVar <- newMVar peerAdjRib

        -- this is probaly a withMVar...
        adjRibOut' <- takeMVar adjRibOut    
        let adjRibOut'' = Data.Map.insert peer peerAdjRibMVar adjRibOut'
        putMVar adjRibOut adjRibOut''

        return $ Rib' locRIB adjRibOut

lookupRoutes :: Rib -> PeerData -> PathChange -> IO (Maybe (RouteData, [Prefix]))
--- objective:
--- The objective is to filter the input prefixes so that all remaining have the property that the route identified is still best for that prefix.
--- Additionally, a copy of that (common) route is needed for further processing.
--- narrative:
---    Drop from the head of the list whilst the prperty is not satisfied,
---    Return the first found result, reducing the remaining list with a simpler similar filter taht does not capture the RouteData returned by the lookup

lookupRoutes _ _ (prefixes, 0) = return $ Just (Withdraw undefined, prefixes)
lookupRoutes _  _ (prefixes, -1) = return $ Just (Withdraw undefined, prefixes)
lookupRoutes rib target (prefixes, hash) = do
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
            get pfx = (PT.ptBest (fromPrefix pfx) (locRIB rib')) >>= hashMatch_
            
        -- return $ reduce prefixes
        return $ fmap (\(route, remainingPrefixes) ->
            if exportFilter target route
            then (route, remainingPrefixes)
            else (NullRoute, remainingPrefixes)) (reduce prefixes)
{- export filter rationale
  the context of an export filter is the route itself and the respective source and target peers
  simple checks are purely peer related.., but the source peer is part of the Route, so the route and target peer are always the parameters for the test
-}
exportFilter :: PeerData -> RouteData -> Bool
exportFilter target route@RouteData {} = iBGPRelayCheck && noReturnCheck where
    iBGPRelayCheck = fromEBGP route || isExternal target
    noReturnCheck = target /= peerData route
exportFilter _ NullRoute = undefined
exportFilter _ Withdraw{} = undefined


getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    return $ map
             (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTable (locRIB rib') prefix)
             prefixes


pullAllUpdates :: PeerData -> Rib -> IO [PathChange]
pullAllUpdates peer rib = do
    (Rib' _ adjRIBOut) <- readMVar rib
    adjRIBOut' <- readMVar adjRIBOut
    let peerAdjRIBOut = adjRIBOut' Data.Map.! peer

    -- this take/put is null while the fifois itself MVar based...
    peerAdjRIBOut' <- takeMVar peerAdjRIBOut
    let fifo = pathChanges peerAdjRIBOut'
    changes <- dequeueAll fifo
    putMVar peerAdjRIBOut peerAdjRIBOut'
    return changes

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
    rib' <- readMVar rib
    return (locRIB rib')

evalLocalPref :: PeerData -> [PathAttribute] -> [Prefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)

    where

    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate{..} rib = ribUpdateMany peerData puPathAttributes hash nlri rib >>= ribWithdrawMany peerData withdrawn

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' locRIB adjRibOut )
        | null pfxs = return (Rib' locRIB adjRibOut )
        | otherwise = do
              localPref <- evalLocalPref peerData pathAttributes pfxs
              adjRibOut' <- readMVar adjRibOut
              let routeData = makeRouteData peerData pathAttributes routeId localPref
                  routeData' = if importFilter routeData then Withdraw peerData else routeData
                  ( !locRIB' , !updates ) = BGPRib.PrefixTable.update locRIB pfxs routeData'
              updateAdjRIBOut updates adjRibOut'
              return $ Rib' locRIB' adjRibOut

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' locRIB adjRibOut)
        | null pfxs = return (Rib' locRIB adjRibOut )
        | otherwise = do
            let ( !locRIB' , !withdraws ) = BGPRib.PrefixTable.update locRIB pfxs (Withdraw peerData)
            adjRibOut' <- readMVar adjRibOut
            updateAdjRIBOut withdraws adjRibOut'
            return $ Rib' locRIB' adjRibOut

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
        where
        (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
        fromEBGP = isExternal peerData
        med = getMED pathAttributes -- currently not used for tiebreak -- only present value is for forwarding on IBGP
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateAdjRIBOut :: [(Prefix, RouteData)] -> AdjRIBOut -> IO ()
updateAdjRIBOut updates adjRIB = 
    sequence_ $ Data.Map.mapWithKey action adjRIB

    where 
        action :: PeerData -> MVar PeerAdjRIBOut -> IO ()
        action targetPeer peerAdjRIBOut = do
            -- this take/put is null while the fifois itself MVar based...
            peerAdjRIBOut' <- takeMVar peerAdjRIBOut
            let pathChanges = groupBySecond $ map (second routeId) updates
            insertPathChanges pathChanges peerAdjRIBOut'
            putMVar peerAdjRIBOut peerAdjRIBOut'

importFilter :: RouteData -> Bool
importFilter route@RouteData{} = pathLoopCheck route where
    pathLoopCheck r = elemASPath (myAS $ globalData $ peerData r) (pathAttributes r)
importFilter _ = error "importFilter only defined for updates"
