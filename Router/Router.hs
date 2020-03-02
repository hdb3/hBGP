{-# LANGUAGE DuplicateRecordFields,RecordWildCards,TemplateHaskell #-}
module Main where

import Development.GitRev
import Paths_hbgp(version)
import Data.Version(showVersion)
import System.Environment(getArgs)
import System.IO
import System.Exit
import Data.List(intersect)
import Network.Socket
import Control.Monad(unless)
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map

import qualified Session.Session as Session
import BGPRib.BGPRib
import Router.Config
import Router.BgpFSM
import Router.Collision
import Router.Global
import Router.Redistributor
import Router.Log

main :: IO ()
main = do
    info banner

    config <- getConfig

    global <- buildGlobal config

    _ <- forkIO (redistribute global)

    let
        app = bgpFSM global

    debug $ "connecting to " ++ show (activePeers config)
    debug $ "activeOnly = " ++ show (activeOnly config)
    forkIO $ Session.session 179 app (configListenAddress config) (activePeers config) (not $ activeOnly config)
    info "Router ready"
    takeMVar (exitFlag global)
    -- gracefull cleanup would have to be called here
    -- currently, sessions just fall of a cliff edge (TCP reset)

banner = "hbgp " ++ showVersion version
         ++ if "master" == $(gitBranch) then "" else " (" ++ $(gitBranch)++ ")"
         
getConfig :: IO Config
getConfig = do
    args <- getArgs
    unless (null $ intersect args ["--version","-V","-v"]) exitSuccess

    --let n = if 1 < length args then read (args !! 1) :: Int else 0
    let n = 0 -- kludge until this is patched to support ArgConfig style parameters.....
    let configPath = if null args then "bgp.conf" else head args

    configString <- readFile configPath
    let rawConfig = read configString :: Config
        rawConfig' = if 0 == n then rawConfig else rawConfig { configTestRouteCount = n }
    return $ buildPeerConfigs rawConfig'

buildGlobal :: Config -> IO Global
buildGlobal c@Config{..} = do
    let
        config = c
        gd = GlobalData { myAS = configAS , myBGPid = configBGPID }
        ld = localPeer gd
        delayOpenTimer = configDelayOpenTimer
        initialHoldTimer = configInitialHoldTimer

        -- TODO  - configure this in configuration file
        listenAddress = SockAddrInet 179 0 -- listen on all intefaces by default...

        -- TODO the map creation should be in Config...
        peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc,pc)) configConfiguredPeers

        logger = hPutStrLn stdout

    exitFlag <- newEmptyMVar

    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- BGPRib.BGPRib.newRib ld
    return Global {..}
