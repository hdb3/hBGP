{-# LANGUAGE RecordWildCards, TupleSections #-}
module StdRib(ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress,StdRib.addPeer,StdRib.ribPush,RibHandle) where
import Control.Monad.Extra(when,concatMapM)
import System.Timeout(timeout)
import Data.Maybe(fromMaybe)
import Data.Word

import BGPlib
import BGPRib
import Log

type RibHandle = (Rib,PeerData)

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer rib peer = do
    trace $ "addPeer " ++ show peer
    BGPRib.addPeer rib peer
    return (rib,peer)

ribPush :: RibHandle -> ParsedUpdate -> IO()
ribPush (rib,peer) update = do
    trace $ "ribPush " ++ show peer ++ ":" ++ show update
    BGPRib.ribPush rib peer update

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress rib port ip = do
    peers <- filter (\pd -> peerIPv4 pd == ip && peerPort pd == port) <$> getPeersInRib rib
    if null peers then
        warn $ "delPeerByAddress failed for " ++ show ip ++ ":" ++ show port
    else do
        when ( length peers > 1 ) $ warn $ "delPeerByAddress failed for (multiplepeers!) " ++ show ip ++ ":" ++ show port
        mapM_ (delPeer rib) peers
    
ribPull :: RibHandle -> IO [ParsedUpdate]
ribPull (rib,peer) = pullAllUpdates peer rib >>= updateFromAdjRibEntrys rib peer

msgTimeout :: Int -> IO [a] -> IO [a]
msgTimeout t f = fromMaybe [] <$> timeout (1000000 * t) f

addRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IPv4 -> IO()
addRouteRib rib peer prefix nextHop = BGPRib.ribPush rib peer (igpUpdate nextHop [fromAddrRange prefix])

delRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IO()
delRouteRib rib peer prefix = BGPRib.ribPush rib peer (originateWithdraw [fromAddrRange prefix])

buildUpdate :: PeerData -> [IPrefix] -> RouteData -> [ParsedUpdate]
-- there are three distinct 'peers' and associated PeerData potentially in scope here
--     the peer which originated the route
--     the peer which will receive this update ('target')
--     the local 'peer' (not used)
--
-- the relavant peers / cases are:
--     for the iBGP/eBGP choice - the peer which will receive this update ('target')
--     for the onward NextHop attribute - the peer which will receive this update ('target')
--     for localPref (iBGP only) - the setting is a policy one, but should be the same regardless of target,
--          hence taken from the route origin ('peerData')
--
-- Note: the Route source peer can be reached from the RouteData record via peerData
--

buildUpdate target iprefixes RouteData{..} = if isExternal target then egpUpdate else igpUpdate
    where
    igpUpdate = makeUpdate (toPrefixes iprefixes)
                           []
                           ( sortPathAttributes $
                           -- TODO - consider why (re-)setting the origin here is sensible -
                           --        surely it should have been set correctly on ingress and not chabged, regardless of
                           --        destination peer???
                           --        Specifically, this overrides local route distinctions!!!
                           setOrigin _BGP_ORIGIN_INCOMPLETE $
                           -- this is reflector/controller default, bur for a router next-hop-self is default:
                           -- setNextHop (nextHop route) $
                           setNextHop (localIPv4 peerData ) $ -- next hop self!
                           setLocalPref (localPref peerData )
                           pathAttributes 
                           )
    egpUpdate = makeUpdate (toPrefixes iprefixes)
                           []
                           ( sortPathAttributes $
                           setOrigin _BGP_ORIGIN_INCOMPLETE $
                           -- setNextHop (nextHop route) $ -- reflector default
                           setNextHop (localIPv4 peerData ) $ -- next hop self!
                           prePendAS ( myAS $ globalData peerData )
                           pathAttributes 
                           )

updateFromAdjRibEntrys :: Rib -> PeerData -> [AdjRIBEntry] -> IO [ParsedUpdate]
updateFromAdjRibEntrys rib target = concatMapM (updateFromAdjRibEntry rib target)
    -- ToDO - Restore the capability to generate WITHDRAWs!!!!
    where

    updateFromAdjRibEntry :: Rib -> PeerData -> AdjRIBEntry -> IO [ParsedUpdate]
    updateFromAdjRibEntry rib target (iprefixes,routeHash) =
        concatMap (\(route,iprefixes) -> buildUpdate target iprefixes route) <$> lookupRoutes rib (iprefixes,routeHash)

routesFromAdjRibEntrys :: Rib -> [AdjRIBEntry] -> IO [(IPrefix,IPv4)]
routesFromAdjRibEntrys rib = concatMapM (routesFromAdjRibEntry rib)
    where

    routesFromAdjRibEntry :: Rib -> AdjRIBEntry -> IO [(IPrefix,IPv4)]
    routesFromAdjRibEntry rib (iprefixes,routeHash) =
        concatMap (\(route,iprefixes) -> map (,nextHop route) iprefixes ) <$> lookupRoutes rib (iprefixes,routeHash)
