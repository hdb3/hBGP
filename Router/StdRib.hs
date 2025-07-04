{-# LANGUAGE RecordWildCards #-}

module Router.StdRib (ribPull, msgTimeout, addRouteRib, delRouteRib, updateFromAdjRibEntrys, delPeerByAddress, Router.StdRib.addPeer, Router.StdRib.ribPush, RibHandle) where

-- module Router.StdRib (ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,delPeerByAddress,Router.StdRib.addPeer,Router.StdRib.ribPush,RibHandle) where

import BGPRib.BGPRib
import qualified BGPRib.BGPRib as BGPRib
import BGPlib.BGPlib (BGPOutput, ParsedUpdate (..), Prefix, delLocalPref, makeUpdate, prePendAS, setLocalPref, setNextHop, setOrigin, sortPathAttributes)
import Control.Monad.Extra (when)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word
import Router.Log
import System.Timeout (timeout)

type RibHandle = (Rib, PeerData)

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer rib peer = do
  event $ "Peer Up: " ++ show (peerIPv4 peer)
  BGPRib.addPeer rib peer
  return (rib, peer)

ribPush :: RibHandle -> ParsedUpdate -> IO ()
ribPush (_, peer) NullUpdate = return ()
ribPush (rib, peer) update@ParsedUpdate {} = do
  if null (nlri update)
    then
      if null (withdrawn update)
        then event $ "EOR: " ++ show (peerIPv4 peer)
        else event $ "Withdraw: " ++ show (peerIPv4 peer) ++ show (withdrawn update)
    else event $ "Update: " ++ show (peerIPv4 peer) ++ show update
  BGPRib.ribPush rib peer update

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress rib port ip = do
  peers <- filter (\pd -> peerIPv4 pd == ip && peerPort pd == port) <$> getPeersInRib rib
  if null peers
    then
      warn $ "delPeerByAddress failed for " ++ show ip ++ ":" ++ show port
    else do
      when (length peers > 1) $ warn $ "delPeerByAddress failed for (multiple peers!) " ++ show ip ++ ":" ++ show port
      mapM_ (delPeer rib) peers

ribPull :: RibHandle -> IO [BGPOutput]
ribPull (rib, peer) = pullAllUpdates peer rib >>= updateFromAdjRibEntrys rib peer

msgTimeout :: Int -> IO [a] -> IO [a]
msgTimeout t f = fromMaybe [] <$> timeout (1000000 * t) f

addRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IPv4 -> IO ()
addRouteRib = error "addRouteRib: undefined"

-- addRouteRib rib peer prefix nextHop = BGPRib.ribPush rib peer (igpUpdate nextHop [fromAddrRange prefix])

delRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IO ()
delRouteRib = error "delRouteRib: undefined"

-- delRouteRib rib peer prefix = BGPRib.ribPush rib peer (bgpWithdraw [fromAddrRange prefix])

buildUpdate :: PeerData -> [Prefix] -> RouteData -> BGPOutput
-- there are three distinct 'peers' and associated PeerData potentially in scope here
--     the peer which originated the route
--     the peer which will receive this update ('target')
--     the local 'peer' (not used)
--
-- the relevant peers / cases are:
--     for the iBGP/eBGP choice - the peer which will receive this update ('target')
--     for the onward NextHop attribute - the peer which will receive this update ('target')
--     for localPref (iBGP only) - the setting is a policy one, but should be the same regardless of target,
--          hence taken from the route origin ('peerData')
--
-- Note: the Route source peer can be reached from the RouteData record via peerData
--
buildUpdate target prefixes NullRoute = makeUpdate [] prefixes []
buildUpdate target prefixes Withdraw {} = makeUpdate [] prefixes []
buildUpdate target prefixes RouteData {..} = if isExternal target then egpUpdate else igpUpdate
  where
    igpUpdate =
      makeUpdate
        prefixes
        []
        ( sortPathAttributes $
            setOrigin origin $
              -- this is reflector/controller default, bur for a router next-hop-self is default:
              setNextHop nextHop $
                -- setNextHop (localIPv4 peerData ) $ -- next hop self! - but not very good if the route is actually local, unless we set the local peer ip4...
                setLocalPref
                  localPref
                  pathAttributes
        )
    egpUpdate =
      makeUpdate
        prefixes
        []
        ( sortPathAttributes $
            setOrigin origin $
              setNextHop (localIPv4 target) $ -- next hop self!
                prePendAS (myAS $ globalData peerData) $
                  delLocalPref pathAttributes
        )

updateFromAdjRibEntrys :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPOutput]
updateFromAdjRibEntrys rib target xs = catMaybes <$> mapM updateFromAdjRibEntry xs
  where
    updateFromAdjRibEntry :: AdjRIBEntry -> IO (Maybe BGPOutput)
    updateFromAdjRibEntry (prefixes, routeHash) =
      fmap (\(route, prefixes') -> buildUpdate target prefixes' route) <$> lookupRoutes rib (prefixes, routeHash)
