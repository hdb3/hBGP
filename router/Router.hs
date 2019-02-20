{-# LANGUAGE DuplicateRecordFields,RecordWildCards #-}
module Main where

import System.Environment(getArgs)
import Network.Socket
import Session
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map

import Config
import BgpFSM
import Collision
import BGPRib
import Global
import Redistributor

main :: IO ()
main = do
    config <- getConfig
    print config

    putStrLn "ZRouter starting"

    global <- buildGlobal config

    forkIO (redistribute global)
    
    let
        app = bgpFSM global

    putStrLn $ "connecting to " ++ show (activePeers config)
    session 179 app (activePeers config)
    putStrLn "Router ready"
    idle where idle = do threadDelay 10000000
                         idle

getConfig = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
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

    collisionDetector <- mkCollisionDetector
    sessions <- newMVar Data.Map.empty
    rib <- BGPRib.newRib ld
    return Global {..}
