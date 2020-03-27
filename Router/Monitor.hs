module Router.Monitor where

import Control.Concurrent (readChan)
import Router.Global
import Router.Log

startMonitor :: Global -> IO ()
startMonitor global = do
  item <- readChan (monitorChannel global)
  either reportDown reportUp item
  startMonitor global
  where
    reportUp peer = info $ "peer connected: " ++ show peer
    reportDown peer = info $ "peer disconnected: " ++ show peer
