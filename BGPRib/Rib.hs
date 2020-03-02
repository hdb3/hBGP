{-# LANGUAGE RecordWildCards, TupleSections #-}
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
        updateRibOutWithPeerData peer nullRoute prefixes adjRib
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
{-
    delayed route look-up logic
    After an update/withdraw is scheduled the best route may change, or be removed, leaving no route at all.
    This raises two issues: firstly, the original route may no longer be available, so the immediate update cannot be sent.
    Secondly, even if the route is still valid, or a withdraw was requested, the optimal behaviour may not be to send the originally selected route.
    The simple course of action is to discard tbe immediate update, safe in the knowledge that another update for the specific peer is in the pipeline.
    However, the logic fails when the request is withdraw, since it is possible that the replacement route would be filtered for the specific target peer.
    Therefore scheduled withdraw should not be discarded.  A similar problem arises when the current route is null, in which case a withdraw is in the pipeline,
    but if we discard the earlier route and it was also the first route selected for this prefix/peer then a withdraw will be generated without a matching update.
    This outcome is likely to be rare, and does not compromise route integrity, although a peer may detect the anomaly.
    A full resoultion is not possible in this design - it motivates a change from use of route hashes to actual route referneces in ARO entries.

    Summary: withdrawals are never discarded, changed non-null routes are...

    New design

    lookupRoutes should return [(Maybe RouteData,[Prefix])] rather than [(RouteData,[Prefix])],
    where Maybe RouteData allows a withdraw to be represented.  The prescribed filter logic is implemented after initial lookup and before function return.
    The initial lookup function returns an ARE record (tuple) augmented with the lookup outcome.
    Before grouping on the lookup outcome the list is filtered according to the description above. 
-}

lookupRoutes :: Rib -> AdjRIBEntry -> IO [(Maybe RouteData,[Prefix])]
lookupRoutes rib (prefixes,routeHash) = if 0 == routeHash
    then return [(Nothing,prefixes)] 
    else do
        rib' <- readMVar rib
        let pxrs = map (\prefix -> (queryPrefixTable (prefixTable rib') prefix, prefix)) prefixes
            discardChanged (mrd,_) = maybe True (\rd -> routeHash == routeId rd) mrd
        return $ group $ filter discardChanged pxrs

getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    let getNextHop prefix = (prefix,) $ nextHop <$> queryPrefixTable (prefixTable rib') prefix
    return $ map getNextHop prefixes

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
                  ( prefixTable' , updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData
              updateRibOutWithPeerData peerData routeData updates adjRibOutTables
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.withdraw prefixTable pfxs peerData
            updateRibOutWithPeerData peerData nullRoute withdraws adjRibOutTables
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeId overrideLocalPref = RouteData {..}
        where
        pathLength = getASPathLength pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: PeerData -> RouteData -> [Prefix] -> AdjRIB -> IO ()
updateRibOutWithPeerData originPeer routeData updates adjRib = sequence_ $ Data.Map.mapWithKey updateWithKey adjRib
    where updateWithKey destinationPeer table = when ( destinationPeer /= originPeer )
                                                     ( insertAdjRIBTable (updates, routeId routeData ) table )
    -- returning routes to the originator seems unprofitable
    -- however real routers do exactly this (e.g. Cisco IOS) and it may simplify matters to follow suit...
    -- hence this version of updateWithKey can be replaced with a simpler one....
          updateWithKey' _ = insertAdjRIBTable (updates, routeId routeData)
    -- in future this could be a run-time configuration option.....

    
