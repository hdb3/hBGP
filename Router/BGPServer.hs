{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.IP
import qualified Network.Socket as NS
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import Router.BGPConnect

main :: IO ()
main = do
  putStrLn "bgpserv"
  args <- getArgs
  if null args
    then die "can't read address in argument 1"
    else
      maybe
        (putStrLn "could not parse addresses")
        (listener 179)
        (readMaybe (head args))

listener :: NS.PortNumber -> IPv4 -> IO ()
listener port local = do
  serverSocket <- openServerSocket port local
  putStrLn $ "listener waiting on " ++ show local ++ ":" ++ show port
  sock1 <- getServerSession serverSocket
  putStr "got connection (1): "
  showSockAddresses sock1 >>= putStrLn
  sock2 <- getServerSession serverSocket
  putStr "got connection (2): "
  showSockAddresses sock2 >>= putStrLn
  NS.gracefulClose sock1 100000
  NS.gracefulClose sock2 100000
  putStrLn "disconnected"
