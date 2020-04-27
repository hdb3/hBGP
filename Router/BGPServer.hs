module Main where

import Data.IP
import Data.Word
import Router.BGPConnect
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

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

listener :: Word16 -> IPv4 -> IO ()
listener port local = do
  serverSocket <- openServerSocket port local
  putStrLn $ "listener waiting on " ++ show local ++ ":" ++ show port
  sock1 <- getServerSession serverSocket
  putStr "got connection (1): "
  getSockAddresses sock1 >>= print
  sock2 <- getServerSession serverSocket
  putStr "got connection (2): "
  getSockAddresses sock2 >>= print
  gracefulClose sock1 100000
  gracefulClose sock2 100000
  putStrLn "disconnected"
