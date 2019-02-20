{-# LANGUAGE RecordWildCards #-}
module Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void)

import BGPReader(pathReadRib)
import BGPRib
import BGPlib
import Route
import Global
import Config
import ZServ

redistribute :: Global -> IO ()
redistribute global@Global{..} = do
    insertTestRoutes global (configTestRoutePath config) (configTestRouteCount config)
    if not (configEnableDataPlane config )
    then putStrLn "configEnableDataPlane not set, not starting zserv API"
    else do threadId <- myThreadId
            putStrLn $ "Thread " ++ show threadId ++ " starting redistributor"
            ( zStreamIn, zStreamOut ) <- getZServerStreamUnix "/var/run/quagga/zserv.api"
            zservRegister zStreamOut _ZEBRA_ROUTE_BGP
            if configEnableRedistribution config
            then void $ forkIO (zservReader global (localPeer gd) ( zStreamIn, zStreamOut ))
            else putStrLn "configEnableRedistribution not enabled - not staring Zserv listener"

            let routeInstall (route, Nothing) = putStrLn $ "route not in Rib!: " ++ show route
                routeInstall (route, Just nextHop) = do putStrLn $ "install " ++ show route ++ " via " ++ show nextHop
                                                        addRoute zStreamOut (toAddrRange $ toPrefix route) nextHop
                routeDelete route = do putStrLn $ "delete " ++ show route
                                       delRoute zStreamOut (toAddrRange $ toPrefix route)

             -- addPeer rib peerData
             -- delPeer rib peerData
            ribUpdateListener (routeInstall,routeDelete) global ( localPeer gd ) 1


ribUpdateListener (routeInstall,routeDelete) global@Global{..} peer timeout = do
    updates <- pullAllUpdates (1000000 * timeout) peer rib
    if null updates then
        yield -- null op - could check if exit from thread is needed...
    else do routes <- lookupRoutes rib peer updates
            putStr "Ready to install routes"
            if 11 > length updates then
                print $ map fst updates
            else do
                print $ map fst (take 10 updates)
                putStrLn $ "and " ++ show (length updates - 10) ++ " more"
            let getNextHop rib pfx = do nh <- lookupNextHop rib pfx
                                        return (pfx,nh)
            let (update,withdraw) = foldl disc ([],[]) updates
                disc (u,w) (pfxs,0) = (u,w++pfxs) -- withdraw has 0 for the route index
                disc (u,w) (pfxs,_) = (u++pfxs,w) -- alternate case is an update, not withdraw - and we don't care what was the route index at the time...
            routes <- mapM ( getNextHop rib ) update
            mapM_ routeDelete withdraw
            mapM_ routeInstall routes

    -- rinse and repeat...

    ribUpdateListener (routeInstall,routeDelete) global peer timeout


zservReader global@Global{..} peer ( zStreamIn, zStreamOut ) = do
    zservRequestRouterId zStreamOut
    zservRequestInterface zStreamOut
    zservRequestRedistributeAll zStreamOut
    loop zStreamIn
    where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              -- print zMsg
                              maybe (putStrLn "--")
                                    -- (\s -> putStrLn $ "local route:" ++ show s)
                                    (\(pfx,maybeNH) -> maybe (do putStrLn $ "delete route: " ++ show pfx
                                                                 delRouteRib rib peer pfx )
                                                             (\nh -> do putStrLn $ "add route: " ++ show pfx ++ " via " ++ show nh
                                                                        addRouteRib rib peer pfx nh)
                                                             maybeNH

                                    )
                                    ( getZRoute zMsg )
                              -- let route = getZRoute zMsg
                              -- print route
                              loop stream )
              msg


insertTestRoutes _ "" _ = putStrLn "no test route data specified"
insertTestRoutes Global{..} path count = do
    putStrLn $ "test route set requested: " ++ path
    updates <- pathReadRib path
    let count' = if count == 0 then length updates else count
    putStrLn $ "inserting " ++ show count' ++ " routes"
    let updates' = concatMap (\((_,pas),pfxs) -> makeUpdate pfxs [] pas) updates
        updates'' = if 0 == count then updates' else take count updates'
    mapM (ribUpdater rib ( localPeer gd )) updates''
    putStrLn "done"
