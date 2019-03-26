{-# LANGUAGE DuplicateRecordFields,RecordWildCards #-}
module Main where

import Paths_router(version)
import Data.Version(showVersion)
import System.Environment(getArgs)
import System.IO
import Network.Socket
import qualified Session
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map

import Config
import BgpFSM
import Collision
import BGPRib
import Global
import Redistributor
import Log

main :: IO ()
main = do
    info $ "Router " ++ showVersion version ++ " starting"

    config <- getConfig

    global <- buildGlobal config

    _ <- forkIO (redistribute global)
    
    let
        app = bgpFSM global

    info $ "connecting to " ++ show (activePeers config)
    info $ "activeOnly = " ++ show (activeOnly config)
    --print config
    Session.session 179 app (configListenAddress config) (activePeers config) (not $ activeOnly config)
    info "Router ready"
    idle where idle = do threadDelay 10000000
                         idle

getConfig = do
    args <- getArgs
    --let n = if 1 < length args then read (args !! 1) :: Int else 0
    let n = 0 -- kludge until this is patched to support ArgConfig style parameters.....
    let configPath = if null args then "bgp.conf" else head args

    configString <- readFile configPath
    let rawConfig = read configString :: Config
        rawConfig' = if 0 == n then rawConfig else rawConfig { configTestRouteCount = n }
    --print rawConfig
    return $ buildPeerConfigs rawConfig'

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

    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- BGPRib.newRib ld
    return Global {..}
