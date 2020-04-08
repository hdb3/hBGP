module Main where

import Data.IP
import Data.Word
import Router.BGPConnect
import Router.BGPPingParser
import System.Environment (getArgs)
import System.Exit (die)
import System.IO(Handle, IOMode(ReadWriteMode))
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import Control.Concurrent

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
  h1 <- socketToHandle sock1 ReadWriteMode
  h2 <- socketToHandle sock2 ReadWriteMode
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar  
  thread1 <- forkIO $ tcpForwarder mv1 h1 h2
  thread2 <- forkIO $ tcpForwarder mv2 h2 h1
  takeMVar mv1
  takeMVar mv2
  gracefulClose sock1 100000
  gracefulClose sock2 100000
  putStrLn "disconnected"

tcpForwarder :: MVar () -> Handle -> Handle -> IO()
tcpForwarder mvar hIn hOut = do
  bs <- B.hGetSome hIn 64000000
  if B.null bs then do
                       putMVar mvar ()
                       return ()
               else do B.hPut hOut bs
                       tcpForwarder mvar hIn hOut 