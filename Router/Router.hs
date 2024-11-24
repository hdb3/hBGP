{-# LANGUAGE DuplicateRecordFields,RecordWildCards,TemplateHaskell #-}
module Main where

import Development.GitRev
import Paths_hbgp(version)
import Data.Version(showVersion)
import System.Environment(getArgs)
import System.Exit
import Data.List(intersect)
import Network.Socket
import Control.Monad(unless)
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import System.FilePath.Posix(takeBaseName)

import qualified Session.Session as Session
import BGPRib.BGPRib
import Router.Config
import Router.BgpFSM
import Router.Collision
import Router.Global
import Router.Redistributor(redistribute)
import Router.Console(startConsole)
import Router.Monitor(startMonitor)
import Router.Log

main :: IO ()
main = do
    info banner
    say $ "log level is " ++ (show logMode)

    (rawConfig, fileBaseName) <- getConfig
    config <- checkCapabilities rawConfig >>= fixCapabilities

    global <- buildGlobal config fileBaseName


    _ <- forkIO (redistribute global)
    _ <- forkIO (startConsole global)
    _ <- forkIO (startMonitor global)

    let
        app = bgpFSM global

    debug $ "connecting to " ++ show (activePeers config)
    debug $ "activeOnly = " ++ show (activeOnly config)
    _ <- forkIO $ Session.session 179 app (configListenAddress config) (activePeers config) (not $ activeOnly config)
    info $ "Router " ++ fileBaseName ++ " ready"
    takeMVar (exitFlag global)
    -- gracefull cleanup would have to be called here
    -- currently, sessions just fall of a cliff edge (TCP reset)

banner = "hbgp " ++ showVersion version
         ++ if "master" == $(gitBranch) then "" else " (" ++ $(gitBranch)++ ")"

getConfig :: IO (Config, String)
getConfig = do
    args <- getArgs
    unless (null $ intersect args ["--version","-V","-v"]) exitSuccess

    let (configPath,configName) = if null args then ("bgp.conf","Router") else (head args, takeBaseName $ head args)
    
    configString <- readFile configPath
    let rawConfig = read configString :: Config
    return (buildPeerConfigs rawConfig,configName)

buildGlobal :: Config -> String -> IO Global
buildGlobal c@Config{..} configName = do
    let
        config = c
        gd = GlobalData { myAS = configAS , myBGPid = configBGPID }
        routerName = configName
        ld = localPeer gd
        delayOpenTimer = configDelayOpenTimer
        initialHoldTimer = configInitialHoldTimer

        -- TODO  - configure this in configuration file
        listenAddress = SockAddrInet 179 0 -- listen on all intefaces by default...

        -- TODO the map creation should be in Config...
        peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc,pc)) configConfiguredPeers

        logger = putStrLn

    exitFlag <- newEmptyMVar

    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- BGPRib.BGPRib.newRib ld
    monitorChannel <- newChan
    return Global {..}
