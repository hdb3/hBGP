module Main where

import Data.IP
import Data.Word
import Router.BGPConnect
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "bgpclient"
  local <- getArgIPv4 0
  peer <- getArgIPv4 1
  talker 179 peer local
  where
    getArgIPv4 n = do
      args <- getArgs
      if n > 1 + length args
        then die $ "can't read address in argument " ++ show n
        else
          maybe
            (die $ "can't read address in argument " ++ show n)
            return
            (readMaybe (args !! n))

talker :: Word16 -> IPv4 -> IPv4 -> IO ()
talker port peer local = do
  putStrLn $ "connecting to " ++ show peer ++ " from " ++ show local
  sock <- clientConnect port peer local
  putStrLn "connected"
  gracefulClose sock 100000
  putStrLn "disconnected"
