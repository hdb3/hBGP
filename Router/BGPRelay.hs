module Main where

import Control.Concurrent
import qualified Data.ByteString as B
import Data.IP
import Data.Word
import Router.BGPConnect
import Router.BGPPingParser
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (Handle, IOMode (ReadWriteMode), hClose)
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
  h1 <- socketToHandle sock1 ReadWriteMode
  h2 <- socketToHandle sock2 ReadWriteMode
  c1 <- wrapper $ tcpForwarder h1 h2 "stream 1"
  c2 <- wrapper $ tcpForwarder h2 h1 "stream 2"
  r1 <- c1
  r2 <- c2
  putStrLn $ "stream1 yielded " ++ show r1
  putStrLn $ "stream2 yielded " ++ show r2
  hClose h1
  hClose h2
  putStrLn "disconnected"

tcpForwarder :: Handle -> Handle -> String -> IO ()
tcpForwarder hIn hOut name = do
  putStrLn $ "start " ++ name
  go
  putStrLn $ "normal end of stream " ++ name
  where
    go = do
      bs <- B.hGetSome hIn 64000000
      if B.null bs
        then return ()
        else do
          B.hPut hOut bs
          go

wrapper action = do
  mv <- newEmptyMVar
  forkFinally action (putMVar mv)
  return (takeMVar mv)
