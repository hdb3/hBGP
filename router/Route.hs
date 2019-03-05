{-# LANGUAGE RecordWildCards, TupleSections #-}
module Route where
import Control.Monad.Extra(concatMapM)
import System.Timeout(timeout)
import Data.Maybe(fromMaybe)

import BGPlib
import BGPRib

buildUpdates :: Rib -> PeerData -> IO [ParsedUpdate]
buildUpdates rib peer = pullAllUpdates peer rib >>= updateFromAdjRibEntrys rib peer

buildUpdates' :: Rib -> PeerData -> IO [ParsedUpdate]
buildUpdates' rib peer = do
    updates <- pullAllUpdates peer rib
    updateFromAdjRibEntrys rib peer updates
--buildUpdates rib peer = do
--    updates <- pullAllUpdates peer rib
--    lookupRoutes' rib peer updates

msgTimeout :: Int -> IO [a] -> IO [a]
msgTimeout t f = fromMaybe [] <$> timeout (1000000 * t) f

addRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IPv4 -> IO()
addRouteRib rib peer prefix nextHop = BGPRib.ribUpdater rib peer (igpUpdate nextHop [fromAddrRange prefix])

delRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IO()
delRouteRib rib peer prefix = BGPRib.ribUpdater rib peer (originateWithdraw [fromAddrRange prefix])


--lookupRoutes :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
--lookupRoutes rib peer ares = encodeUpdates <$> lookupRoutes' rib peer ares

lookupRoutes' :: Rib -> PeerData -> [AdjRIBEntry] -> IO [ ParsedUpdate ]
lookupRoutes' rib peer = concatMapM (lookupRoute rib peer)
    where

    lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO [ ParsedUpdate ]
    lookupRoute _ _ (iprefixes, 0 ) = return [ originateWithdraw $ toPrefixes iprefixes ]
    lookupRoute _ _ ([], _ ) = do
        putStrLn "empty prefix list in lookupRoute"
        return []

    lookupRoute rib peer (iprefixes, _ ) = do
        -- TODO - fix major dysfunction!!!!!
        -- this code assumes that prefix groups are always atomic!!!!!!
        -- so actually this code is running with the theoretical speedup already built in!!!!!
        -- :-((((
        --
        maybeRoute <- queryRib rib (head iprefixes)
        maybe (do putStrLn "failed lookup in lookupRoute"
                  return []
              )
              (return <$> buildUpdate peer iprefixes)
              maybeRoute

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

lookupNextHop :: Rib -> IPrefix -> IO (Maybe IPv4)
lookupNextHop rib iprefix = do
    maybeRoute <- queryRib rib iprefix
    maybe (do putStrLn "failed lookup in lookupRoute"
              return Nothing
          )
          (return . Just . nextHop
          )
          maybeRoute

-- ======================================================================

updateFromAdjRibEntrys :: Rib -> PeerData -> [AdjRIBEntry] -> IO [ParsedUpdate]
updateFromAdjRibEntrys rib target = concatMapM (updateFromAdjRibEntry rib target)

updateFromAdjRibEntry :: Rib -> PeerData -> AdjRIBEntry -> IO [ParsedUpdate]
updateFromAdjRibEntry rib target (iprefixes,routeHash) =
    concatMap (\(route,iprefixes) -> buildUpdate target iprefixes route) <$> lookupRoutes rib (iprefixes,routeHash)

--updateFromAdjRibEntry' rib target (iprefixes,routeHash) = do
--    routes <- lookupRoutes rib (iprefixes,routeHash)
--    return $ concatMap f routes where
--        f (route,iprefixes) = buildUpdate target iprefixes route

routesFromAdjRibEntry :: Rib -> AdjRIBEntry -> IO [(IPrefix,IPv4)]
routesFromAdjRibEntry rib (iprefixes,routeHash) =
    concatMap (\(route,iprefixes) -> map (,nextHop route) iprefixes ) <$> lookupRoutes rib (iprefixes,routeHash)

routesFromAdjRibEntrys :: Rib -> [AdjRIBEntry] -> IO [(IPrefix,IPv4)]
routesFromAdjRibEntrys rib = concatMapM (routesFromAdjRibEntry rib)

