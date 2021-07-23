{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void,when)

import BGPlib.Prefixes ( toAddrRange )
import BGPRib.BGPData ( localPeer )
import BGPRib.Rib ( getNextHops, pullAllUpdates )
--- ************* VERY DANGEROUS - should re-export from BGPfsm to avoid conflict!!!!
---import qualified CustomRib as Rib
import qualified Router.StdRib as Rib
import ZServ.ZServ

import Router.Global
import Router.Config
import Router.Log

redistribute :: Global -> IO ()
redistribute global@Global{..} = do
    when (configEnableDataPlane config )
         ( do   threadId <- myThreadId
                trace $ "Thread " ++ show threadId ++ " starting redistributor"
                ( zStreamIn, zStreamOut ) <- getZServerStreamUnix "/var/run/quagga/zserv.api"
                zservRegister zStreamOut _ZEBRA_ROUTE_BGP
                if configEnableRedistribution config
                then void $ forkIO (zservReader global (localPeer gd) ( zStreamIn, zStreamOut ))
                else info "configEnableRedistribution not enabled - not staring Zserv listener"

                let routeInstall (route, Just nextHop) = do
                        trace $ "install " ++ show route ++ " via " ++ show nextHop
                        addRoute zStreamOut (toAddrRange route) nextHop
                    routeInstall (route, Nothing) = do
                        trace $ "delete " ++ show route
                        delRoute zStreamOut (toAddrRange route)
                        -- delRoute zStreamOut (toAddrRange $ toPrefix route)

                ribUpdateListener routeInstall global ( localPeer gd ) 1
         )


ribUpdateListener routeInstall global@Global{..} peer timeout = do
    updates <- Rib.msgTimeout timeout (pullAllUpdates peer rib)
    if null updates then
        yield -- null op - could check if exit from thread is needed...
    else do
        trace $ show (length updates) ++ " updates for " ++ show peer
        -- this function is simpler than the re-advertisement over BGP case
        -- we need only use the prefix lists to query the RIB for the current nexthops, or delete if not found...
        let prefixes = concatMap fst updates
        routes <- getNextHops rib prefixes
        mapM_ routeInstall routes

    -- rinse and repeat...

    ribUpdateListener routeInstall global peer timeout


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
                              maybe (trace "--")
                                    (\(pfx,maybeNH) -> maybe (do trace $ "delete route: " ++ show pfx
                                                                 Rib.delRouteRib rib peer pfx )
                                                             (\nh -> do trace $ "add route: " ++ show pfx ++ " via " ++ show nh
                                                                        Rib.addRouteRib rib peer pfx nh)
                                                             maybeNH

                                    )
                                    ( getZRoute zMsg )
                              loop stream )
              msg
