{-# LANGUAGE RecordWildCards, TupleSections, BangPatterns #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates,getNextHops) where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Control.Monad(unless,when,void)
-- TODO replace Data.List with Data.List.EXtra and use nubOrd not nub
import Data.List(intercalate)
import Data.Word(Word32)
import Data.IP

import BGPlib.BGPlib

import BGPRib.BGPData
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPRib.Update
import BGPRib.AdjRIBOut
import BGPRib.Common(group)

type Rib = MVar Rib'
-- TODO rename AdjRIB -> AdjRIBMap
-- and create a type 'AdjRIBMapEntry = (PeerData,AdjRIBTable)'

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
        updateRibOutWithPeerData peer NullRoute prefixes adjRib
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
        aro <- fifo ribDump
            -- TODO - this would be the place to insert an end-of-rib marker
        let adjRib' = Data.Map.insert peer aro adjRib
        return $ Rib' prefixTable adjRib'

lookupRoutes :: Rib -> AdjRIBEntry -> IO (Maybe (RouteData,[Prefix]))
lookupRoutes rib (prefixes,routeHash) = do
        rib' <- readMVar rib
        let myLookup = map (\pfx -> ( queryPrefixTable (prefixTable rib') pfx, pfx))
            myFilter = filter ((routeHash ==) . routeId .fst )
            unchangedPrefixes = myFilter $ myLookup prefixes
            route = fst $ head unchangedPrefixes  -- safe becuase only called after test for null
        return $ if null unchangedPrefixes then Nothing else Just (route,map snd unchangedPrefixes)

getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    return $ map
             (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTable (prefixTable rib') prefix)
             prefixes

pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
    (Rib' _ arot) <- readMVar rib
    dequeueAll (arot Data.Map.! peer)

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
    ribPush' peerData ParsedUpdate{..} rib = 
        ribUpdateMany peerData puPathAttributes hash nlri rib >>= ribWithdrawMany peerData withdrawn

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              localPref <- evalLocalPref peerData pathAttributes pfxs
              let routeData = makeRouteData peerData pathAttributes routeId localPref
                  ( !prefixTable' , !updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData
              updateRibOutWithPeerData peerData routeData updates adjRibOutTables
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.withdraw prefixTable pfxs peerData
            updateRibOutWithPeerData peerData NullRoute withdraws adjRibOutTables
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
        where
        pathLength = getASPathLength pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: PeerData -> RouteData -> [Prefix] -> AdjRIB -> IO ()
updateRibOutWithPeerData originPeer routeData updates adjRib = sequence_ $ Data.Map.mapWithKey updateWithKey adjRib
    where updateWithKey _ = insertAdjRIBTable (updates, routeId routeData)
    