{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import BGPRib.BGPRib
import Control.Concurrent
import Control.Logger.Simple
import Control.Monad (unless)
import Data.List (intersect)
import qualified Data.Map.Strict as Data.Map
import qualified Data.Text as T
import Data.Version (showVersion)
import Development.GitRev
import Network.Socket
import Paths_hbgp (version)
import Router.BgpFSM
import Router.Collision
import Router.Config
import Router.Console (startConsole)
import Router.Global
import Router.Monitor (startMonitor)
import Router.Redistributor (redistribute)
import qualified Session.Session as Session
import System.Environment (getArgs)
import System.Exit
import System.FilePath.Posix (takeBaseName)

main :: IO ()
-- main = withGlobalLogging (LogConfig (Just "hbgp.log") True) $ do
main = withGlobalLogging (LogConfig (Nothing) True) $ do
  setLogLevel LogTrace
  logNote banner

  (rawConfig, fileBaseName) <- getConfig
  config <- checkCapabilities rawConfig >>= fixCapabilities

  global <- buildGlobal config fileBaseName

  _ <- forkIO (redistribute global)
  _ <- forkIO (startConsole global)
  _ <- forkIO (startMonitor global)

  let app = bgpFSM global

  logInfo $ T.pack $ "connecting to " ++ show (activePeers config)
  logInfo $ T.pack $ "activeOnly = " ++ show (activeOnly config)
  _ <- forkIO $ Session.session 179 app (configListenAddress config) (activePeers config) (not $ activeOnly config)
  logInfo $ T.pack $ "Router " ++ fileBaseName ++ " ready"
  takeMVar (exitFlag global)

-- gracefull cleanup would have to be called here
-- currently, sessions just fall of a cliff edge (TCP reset)

banner :: T.Text
banner =
  T.pack $
    "hbgp " ++ showVersion version
      ++ if "master" == $(gitBranch) then "" else " (" ++ $(gitBranch) ++ ")"

getConfig :: IO (Config, String)
getConfig = do
  args <- getArgs
  unless (null $ intersect args ["--version", "-V", "-v"]) exitSuccess

  let (configPath, configName) = if null args then ("bgp.conf", "Router") else (head args, takeBaseName $ head args)

  configString <- readFile configPath
  let rawConfig = read configString :: Config
  return (buildPeerConfigs rawConfig, configName)

buildGlobal :: Config -> String -> IO Global
buildGlobal c@Config {..} configName = do
  let config = c
      gd = GlobalData {myAS = configAS, myBGPid = configBGPID}
      routerName = configName
      ld = localPeer gd
      delayOpenTimer = configDelayOpenTimer
      initialHoldTimer = configInitialHoldTimer

      -- TODO  - configure this in configuration file
      listenAddress = SockAddrInet 179 0 -- listen on all intefaces by default...

      -- TODO the map creation should be in Config...
      peerMap = Data.Map.fromList $ map (\pc -> (peerConfigIPv4 pc, pc)) configConfiguredPeers

      logger = putStrLn

  exitFlag <- newEmptyMVar

  collisionDetector <- mkCollisionDetector
  sessions <- newMVar Data.Map.empty
  rib <- BGPRib.BGPRib.newRib ld
  monitorChannel <- newChan
  return Global {..}
