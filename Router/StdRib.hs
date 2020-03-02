{-# LANGUAGE RecordWildCards #-}
module Router.StdRib (ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,delPeerByAddress,Router.StdRib.addPeer,Router.StdRib.ribPush,RibHandle) where
import Control.Monad.Extra(when,concatMapM)
import System.Timeout(timeout)
import Data.Maybe(fromMaybe)
import Data.Word

import BGPlib.BGPlib hiding (nlri,withdrawn)
import BGPRib.BGPRib
import qualified BGPRib.BGPRib as BGPRib
import Router.Log

type RibHandle = (Rib,PeerData)

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer rib peer = do
    event $ "Peer Up: " ++ show (peerIPv4 peer)
    BGPRib.addPeer rib peer
    return (rib,peer)

ribPush :: RibHandle -> ParsedUpdate -> IO()
ribPush (_,peer) NullUpdate = return ()
ribPush (rib,peer) update@ParsedUpdate{} =do
    if null (nlri update)
        then if null (withdrawn update)
            then event $ "EOR: " ++ show (peerIPv4 peer)
            else event $ "Withdraw: " ++ show (peerIPv4 peer) ++ show (withdrawn update)
        else event $ "Update: " ++ show (peerIPv4 peer) ++ show update
    BGPRib.ribPush rib peer update

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress rib port ip = do
    peers <- filter (\pd -> peerIPv4 pd == ip && peerPort pd == port) <$> getPeersInRib rib
    if null peers then
        warn $ "delPeerByAddress failed for " ++ show ip ++ ":" ++ show port
    else do
        when ( length peers > 1 ) $ warn $ "delPeerByAddress failed for (multiplepeers!) " ++ show ip ++ ":" ++ show port
        mapM_ (delPeer rib) peers

ribPull :: RibHandle -> IO [BGPMessage]
ribPull (rib,peer) = map deparseUpdate <$> (pullAllUpdates peer rib >>= updateFromAdjRibEntrys rib peer)

msgTimeout :: Int -> IO [a] -> IO [a]
msgTimeout t f = fromMaybe [] <$> timeout (1000000 * t) f

addRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IPv4 -> IO()
addRouteRib rib peer prefix nextHop = BGPRib.ribPush rib peer (ibgpUpdate nextHop [fromAddrRange prefix])

delRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IO()
delRouteRib rib peer prefix = BGPRib.ribPush rib peer (originateWithdraw [fromAddrRange prefix])

buildUpdate :: PeerData -> [Prefix] -> Maybe RouteData -> [ParsedUpdate]
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
buildUpdate target prefixes Nothing = makeUpdate [] prefixes []
buildUpdate target prefixes (Just RouteData{..}) = if isExternal target then egpUpdate else igpUpdate
-- TODO - combine the i/e gpUpdates and use the isExternal switch to drive variant behaviour
    where
    igpUpdate = makeUpdate prefixes
                           []
                           ( sortPathAttributes $
                           setOrigin origin $

                           -- for iBGP we want to route directly to the egress AS interface
                           -- as a router the internal interface is probably equivalent and next-hop-self would work
                           -- but RR or controller cannot use its own address - the coice is between using the next hop as received in EBGP,
                           -- or allowing route selection to specify an alternative - this coding follows the latter path
                           -- however it is likely that the existing next-hop is the same as the override
                           -- (it is questionalble whether it is useful to explicitly hold an alternate next-hop in RouteData,
                           --  since any manipulation may be target sensitive)
                           setNextHop nextHop $
                           -- setNextHop (localIPv4 peerData ) $ -- next hop self alternate
                           setLocalPref 1000000
                                          -- unlike nextHop there is no obvious argument for per-peer variable local preference behaviour,
                                          -- other than the removal of the attribute for EBGP 
                                          --  1000000 -> BGPPROTECTION always wants to win -- this could be configurable
                                          -- or the route selection process could set it in the RouteData
                           pathAttributes 
                           )
    egpUpdate = makeUpdate prefixes
                           []
                           ( sortPathAttributes $
                           setOrigin origin $
                           setNextHop (localIPv4 target ) $ -- next hop self!
                           prePendAS ( myAS $ globalData peerData ) $
                           delLocalPref pathAttributes
                           )

updateFromAdjRibEntrys :: Rib -> PeerData -> [AdjRIBEntry] -> IO [ParsedUpdate]
updateFromAdjRibEntrys rib target = concatMapM (updateFromAdjRibEntry rib target)
    where

    updateFromAdjRibEntry :: Rib -> PeerData -> AdjRIBEntry -> IO [ParsedUpdate]
    updateFromAdjRibEntry rib target (prefixes,routeHash) =
        concatMap (\(route,prefixes) -> buildUpdate target prefixes route) <$> lookupRoutes rib (prefixes,routeHash)
