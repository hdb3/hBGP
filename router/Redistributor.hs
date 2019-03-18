{-# LANGUAGE RecordWildCards #-}
module Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void)

import BGPReader(pathReadRib)
import BGPRib
import BGPlib
import qualified CustomRib as Rib
--import qualified StdRib as Rib
import Global
import Config
import ZServ
import Log

redistribute :: Global -> IO ()
redistribute global@Global{..} = do
    insertTestRoutes global (configTestRoutePath config) (configTestRouteCount config)
    if not (configEnableDataPlane config )
    then info "configEnableDataPlane not set, not starting zserv API"
    else do threadId <- myThreadId
            trace $ "Thread " ++ show threadId ++ " starting redistributor"
            ( zStreamIn, zStreamOut ) <- getZServerStreamUnix "/var/run/quagga/zserv.api"
            zservRegister zStreamOut _ZEBRA_ROUTE_BGP
            if configEnableRedistribution config
            then void $ forkIO (zservReader global (localPeer gd) ( zStreamIn, zStreamOut ))
            else info "configEnableRedistribution not enabled - not staring Zserv listener"

            let routeInstall (route, nextHop) = do
                    trace $ "install " ++ show route ++ " via " ++ show nextHop
                    addRoute zStreamOut (toAddrRange $ toPrefix route) nextHop
                routeDelete route = do
                    trace $ "delete " ++ show route
                    delRoute zStreamOut (toAddrRange $ toPrefix route)

            ribUpdateListener (routeInstall,routeDelete) global ( localPeer gd ) 1


ribUpdateListener (routeInstall,routeDelete) global@Global{..} peer timeout = do
    updates <- Rib.msgTimeout timeout (pullAllUpdates peer rib)
    if null updates then
        yield -- null op - could check if exit from thread is needed...
    else do 
        trace $ show (length updates) ++ " updates for " ++ show peer
        let (update,withdraw) = foldl disc ([],[]) updates
            disc (u,w) (pfxs,0) = (u,w++pfxs) -- withdraw has 0 for the route index
            disc (u,w) (pfxs,ri) = ((pfxs,ri):u,w) -- alternate case is an update, not withdraw , the routeIndex is preserved for the route lookup
        routes <- Rib.routesFromAdjRibEntrys rib update
        mapM_ routeDelete withdraw
        mapM_ routeInstall routes

    -- rinse and repeat...

    ribUpdateListener (routeInstall,routeDelete) global peer timeout


zservReader Global{..} peer ( zStreamIn, zStreamOut ) = do
    zservRequestRouterId zStreamOut
    zservRequestInterface zStreamOut
    zservRequestRedistributeAll zStreamOut
    loop zStreamIn
    where
    loop stream = do
        msg <- Streams.read stream
        maybe ( trace "end of messages")
              ( \zMsg -> do 
                              -- print zMsg
                              maybe (trace "--")
                                    -- (\s -> putStrLn $ "local route:" ++ show s)
                                    (\(pfx,maybeNH) -> maybe (do trace $ "delete route: " ++ show pfx
                                                                 Rib.delRouteRib rib peer pfx )
                                                             (\nh -> do trace $ "add route: " ++ show pfx ++ " via " ++ show nh
                                                                        Rib.addRouteRib rib peer pfx nh)
                                                             maybeNH

                                    )
                                    ( getZRoute zMsg )
                              -- let route = getZRoute zMsg
                              -- print route
                              loop stream )
              msg


insertTestRoutes _ "" _ = info "no test route data specified"
insertTestRoutes Global{..} path count = do
    info $ "test route set requested: " ++ path
    updates <- pathReadRib path
    let count' = if count == 0 then length updates else count
    info $ "inserting " ++ show count' ++ " routes"
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) updates
        updates'' = if 0 == count then updates' else take count updates'
    mapM_ (ribPush rib ( localPeer gd )) updates''
    info "done"
