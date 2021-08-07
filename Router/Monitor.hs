module Router.Monitor where

import Control.Concurrent (readChan)
import Control.Logger.Simple
import qualified Data.Text as T
import Router.Global

info = logInfo . T.pack

startMonitor :: Global -> IO ()
startMonitor global = do
  item <- readChan (monitorChannel global)
  either reportDown reportUp item
  startMonitor global
  where
    reportUp peer = info $ "peer connected: " ++ show peer
    reportDown peer = info $ "peer disconnected: " ++ show peer
