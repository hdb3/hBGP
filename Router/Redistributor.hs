{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void,unless)
import Data.IP
import Text.Read
import System.IO(hFlush,stdout)
import Data.Char(toLower)
import Data.Word(Word32)
import System.Exit(exitSuccess)

import BGPlib.BGPlib
import BGPRib.BGPRib
import BGPRib.BGPReader(pathReadRib)
-- ************* VERY DANGEROUS - should re-export from BGPfsm to avoid conflict!!!!
--import qualified CustomRib as Rib
import qualified Router.StdRib as Rib
import ZServ.ZServ

import Router.Global
import Router.Config
import Router.Log
import Router.CreateUpdate

redistribute :: Global -> IO ()
redistribute global@Global{..} = do
    insertTestRoutes global (configTestRoutePath config) (configTestRouteCount config)
    startConsole global
    if not (configEnableDataPlane config )
    then info "configEnableDataPlane not set, not starting zserv API"
    else do threadId <- myThreadId
            trace $ "Thread " ++ show threadId ++ " starting redistributor"
            ( zStreamIn, zStreamOut ) <- getZServerStreamUnix "/var/run/quagga/zserv.api"
            zservRegister zStreamOut _ZEBRA_ROUTE_BGP
            if configEnableRedistribution config
            then void $ forkIO (zservReader global (localPeer gd) ( zStreamIn, zStreamOut ))
            else info "configEnableRedistribution not enabled - not staring Zserv listener"

            let routeInstall (route, Just nextHop) = do
                    trace $ "install " ++ show route ++ " via " ++ show nextHop
                    addRoute zStreamOut (toAddrRange route) nextHop
                    -- addRoute zStreamOut (toAddrRange $ toPrefix route) nextHop
                routeInstall (route, Nothing) = do
                    trace $ "delete " ++ show route
                    delRoute zStreamOut (toAddrRange route)
                    -- delRoute zStreamOut (toAddrRange $ toPrefix route)

            ribUpdateListener routeInstall global ( localPeer gd ) 1


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

data CState = CState { push :: (ParsedUpdate -> IO ())
                     , exit :: IO ()
                     , csPath :: [Word32]
                     , csNlri :: [AddrRange IPv4]
                     , csNextHop :: IPv4
                     , csLocalPref :: Word32 }

startConsole global = do
   let push = ribPush (rib global) (localPeer $ gd global)
       exit = putMVar (exitFlag global) ()
       consoleThread = console $ CState push exit [] [] "0.0.0.0" 100
                       --   putMVar (exitFlag global) () 
   void $ forkIO consoleThread

updateFrom CState{..} = mapM_ push $ iBGPUpdate csPath csNlri csNextHop csLocalPref
withdrawFrom CState{..} = mapM_ push ( bgpWithdraw csNlri )

console :: CState -> IO ()
console cstate@CState{..} = do
    prompt
    input <- getLine
    let (command:px) = words input ++ repeat ""
    case map toLower command of

        "" -> console cstate

        "s" -> do putStrLn $ "Route: " ++ show csPath ++ " : " ++ show csNlri ++ " nh=" ++ show csNextHop ++ " lp=" ++ show csLocalPref
                  console cstate

        "u" -> do putStrLn $ "Sending update: " ++ show csPath ++ " : " ++ show csNlri ++ " : " ++ show csNextHop
                  updateFrom cstate
                  console cstate

        "w" -> do putStrLn $ "Sending withdraw: " ++ show csNlri
                  withdrawFrom cstate
                  console cstate

        "h" -> maybe (putStrLn "couldn't parse nexthop")
                     (\p -> do putStrLn $ "Nexthop: " ++ show p
                               console cstate {csNextHop = p}
                     )
                     (parseAddress $ px !! 0)

        "p" -> maybe (putStrLn "couldn't parse a path")
                     (\p -> do putStrLn $ "Path: " ++ show p
                               console cstate {csPath = p}
                     )
                     (parsePath $ px !! 0)
        
        "n" -> maybe (putStrLn "couldn't parse prefixes")
                     (\p -> do putStrLn $ "nlri: " ++ show p
                               console cstate {csNlri = p}
                     )
                     (parsePrefixes $ px !! 0)

        "l" -> maybe (putStrLn "couldn't parse local preference")
                     (\p -> do putStrLn $ "LocPref: " ++ show p
                               console cstate {csLocalPref = p}
                     )
                     (parseWord32 $ px !! 0)

        "q" -> putStrLn "goodbye" >> exit                  

        _   -> putStrLn "couldn't parse a command - try Show / Path / nH / Nlri / Local preference / Update / Withdraw / Quit"

    console cstate

prompt = putStr ">>> " >> hFlush stdout

parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)
parsePrefixes s = readMaybe s :: Maybe [AddrRange IPv4]

parseAddress s  = readMaybe s :: Maybe IPv4
parsePath s  = readMaybe s :: Maybe [Word32]
parseWord32 s  = readMaybe s :: Maybe Word32
