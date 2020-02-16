{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.Redistributor where
import Control.Concurrent
import qualified System.IO.Streams as Streams
import Control.Monad(void,unless)
import Data.Maybe(fromMaybe)
import Data.IP
import Text.Read hiding (lift)
import System.IO(hFlush,stdout)
import Data.Char(toLower)
import Data.Word(Word32)
import System.Exit(exitSuccess)
import System.Console.Haskeline
import Control.Monad.Trans.Class

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

data CState = CState { push :: (ParsedUpdate -> InputT IO ())
                     , exit :: InputT IO ()
                     , csGlobal :: Global
                     , csPath :: [Word32]
                     , csNlri :: [AddrRange IPv4]
                     , csNextHop :: IPv4
                     , csLocalPref :: Word32 }

query :: CState -> [String] -> InputT IO ()
query cs s = do
    prefixTable <- lift $ getLocRib (rib $ csGlobal cs)
    if null $ head s then
        lift $ print prefixTable
    else
        maybe (outputStrLn "couldn't parse prefix")
              (\prefix -> outputStrLn $ "[" ++ show prefix ++ "] " ++ showRibAt prefixTable (fromAddrRange prefix))
              (parsePrefix $ head s)
    console cs

startConsole global = do
   let push v = lift $ ribPush (rib global) (localPeer $ gd global) v
       exit = lift $ putMVar (exitFlag global) ()
       consoleThread = runInputT defaultSettings $ console $ CState push exit global [] [] "0.0.0.0" 100
   void $ forkIO consoleThread

updateFrom CState{..} = mapM_ push $ iBGPUpdate csPath csNlri csNextHop csLocalPref
withdrawFrom CState{..} = mapM_ push ( bgpWithdraw csNlri )

console :: CState -> InputT IO ()
console cstate@CState{..} = do
    
    inputM <- getInputLine ">>> "
    let input = fromMaybe "" inputM
        (command:px) = words input ++ repeat ""
    case map toLower command of

        "" -> console cstate

        "q" -> query cstate px

        "s" -> do outputStrLn $ "Route: " ++ show csPath ++ " : " ++ show csNlri ++ " nh=" ++ show csNextHop ++ " lp=" ++ show csLocalPref
                  console cstate

        "u" -> do outputStrLn $ "Sending update: " ++ show csPath ++ " : " ++ show csNlri ++ " : " ++ show csNextHop
                  updateFrom cstate
                  console cstate

        "w" -> do outputStrLn $ "Sending withdraw: " ++ show csNlri
                  withdrawFrom cstate
                  console cstate

        "h" -> maybe (outputStrLn "couldn't parse nexthop")
                     (\p -> do outputStrLn $ "Nexthop: " ++ show p
                               console cstate {csNextHop = p}
                     )
                     (parseAddress $ px !! 0)

        "p" -> maybe (outputStrLn "couldn't parse a path")
                     (\p -> do outputStrLn $ "Path: " ++ show p
                               console cstate {csPath = p}
                     )
                     (parsePath $ px !! 0)
        
        "n" -> maybe (outputStrLn "couldn't parse prefixes")
                     (\p -> do outputStrLn $ "nlri: " ++ show p
                               console cstate {csNlri = p}
                     )
                     (parsePrefixes $ px !! 0)

        "l" -> maybe (outputStrLn "couldn't parse local preference")
                     (\p -> do outputStrLn $ "LocPref: " ++ show p
                               console cstate {csLocalPref = p}
                     )
                     (parseWord32 $ px !! 0)

        "x" -> outputStrLn "goodbye" >> exit                  

        _   -> outputStrLn "couldn't parse a command - try Show / Path / nH / Nlri / Local preference / Update / Withdraw / Quit"

    console cstate

prompt = putStr ">>> " >> hFlush stdout

parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)
parsePrefixes s = readMaybe s :: Maybe [AddrRange IPv4]

parseAddress s  = readMaybe s :: Maybe IPv4
parsePath s  = readMaybe s :: Maybe [Word32]
parseWord32 s  = readMaybe s :: Maybe Word32
