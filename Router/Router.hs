{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import BGPRib.BGPRib
import Control.Concurrent
import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.List (intersect)
import qualified Data.Map.Strict as Data.Map
import Data.Version (showVersion)
import Data.Yaml
import Development.GitRev
import Network.Socket
import Paths_hbgp (version)
import Router.BgpFSM
import Router.Collision
import Router.Config
import Router.Console (startConsole)
import Router.Global
import Router.Log
import Router.Monitor (startMonitor)
import Router.Redistributor (redistribute)
import qualified Session.Session as Session
import System.Environment (getArgs)
import System.Exit
import System.FilePath.Posix (takeBaseName)
import Text.Read (readEither)

main :: IO ()
main = do
  info banner
  say $ "log level is " ++ (show logMode)

  getConfig
    >>= maybe
      exitFailure
      ( \(rawConfig, fileBaseName) -> do
          config <- checkCapabilities rawConfig >>= fixCapabilities
          global <- buildGlobal config fileBaseName
          _ <- forkIO (redistribute global)
          _ <- forkIO (startConsole global)
          _ <- forkIO (startMonitor global)

          let app = bgpFSM global

          debug $ "connecting to " ++ show (activePeers config)
          debug $ "activeOnly = " ++ show (activeOnly config)
          _ <- forkIO $ Session.session 179 app (configListenAddress config) (activePeers config) (not $ activeOnly config)
          info $ "Router " ++ fileBaseName ++ " ready"
          takeMVar (exitFlag global)
      )

-- graceful cleanup would have to be called here
-- currently, sessions just fall of a cliff edge (TCP reset)

banner =
  "hbgp "
    ++ showVersion version
    ++ if "master" == $(gitBranch) then "" else " (" ++ $(gitBranch) ++ ")"

getConfig :: IO (Maybe (Config, String))
getConfig = do
  args <- getArgs
  unless (null $ intersect args ["--version", "-V", "-v"]) exitSuccess

  let (configPath, configName) = if null args then ("bgp.conf", "Router") else (head args, takeBaseName $ head args)

  configString <- B.readFile configPath

  let config = decodeEither' configString :: Either ParseException Config

  either
    ( \errMsg -> do
        putStrLn $ "failed to read config from " ++ configPath ++ " " ++ show errMsg
        return Nothing
    )
    ( \config -> do
        putStrLn $ "starting config from " ++ configPath
        return $ Just (buildPeerConfigs config, configName)
    )
    config

buildGlobal :: Config -> String -> IO Global
buildGlobal c@Config {..} configName = do
  let config = c
      gd = GlobalData {myAS = configAS, myBGPid = configBGPID}
      routerName = configName
      ld = localPeer gd
      delayOpenTimer = configDelayOpenTimer
      initialHoldTimer = configInitialHoldTimer

      -- TODO  - configure this in configuration file
      listenAddress = SockAddrInet 179 0 -- listen on all interfaces by default...

      -- TODO the map creation should be in Config...
      peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc, pc)) configConfiguredPeers

      logger = putStrLn

  exitFlag <- newEmptyMVar

  collisionDetector <- mkCollisionDetector
  sessions <- newMVar Data.Map.empty
  rib <- BGPRib.BGPRib.newRib ld
  monitorChannel <- newChan
  return Global {..}
