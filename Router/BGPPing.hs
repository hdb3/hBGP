{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPlib.AttoBGP (bgpParser1)
import BGPlib.BGPlib (BGPMessage)
import Control.Monad (forever)
import Control.Monad.Extra (ifM)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra (byteStringCopy)
import Data.IP
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Word
import Router.BGPConnect
import Router.BGPPingParser
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO
import Text.Read (readMaybe)

retryOnBusy = False

main :: IO ()
main = do
  putStrLn "bgpping"
  args <- getArgs
  let (optargs, posargs) = partition (('-' ==) . head) args
      addresses = map getIPv4 posargs
      passive = elem "--listen" optargs
  if
    | (elem Nothing addresses) -> do
        putStrLn "could not parse addresses"
        usage
    | null args -> usage
    | (length args == 1) -> go passive (fromJust (addresses !! 0)) "0.0.0.0"
    | otherwise -> go passive (fromJust (addresses !! 0)) (fromJust (addresses !! 1))
  where
    go True = listener 179
    go False = talker 179
    getIPv4 = readMaybe :: String -> Maybe IPv4

usage :: IO ()
usage = do
  putStrLn "bgpping"
  putStrLn "bgpping 1.2.3.4 6.7.8.9          (active mode - first address is the remote peer)"
  putStrLn "bgpping --listen 1.2.3.4 6.7.8.9 (passive mode - first address is the remote peer)"

listener :: Word16 -> IPv4 -> IPv4 -> IO ()
listener port peer local = do
  serverSocket <- openServerSocket port local
  putStrLn $ "listener waiting on " ++ show local ++ " for connection from " ++ show peer
  forever
    ( do
        sock <- getServerSession serverSocket
        ifM (common sock peer local) exitSuccess (putStrLn "unexpected transport addresses in passive connect - continuing")
        gracefulClose sock 100000
    )

talker :: Word16 -> IPv4 -> IPv4 -> IO ()
talker port peer local = do
  putStrLn $ "connecting to " ++ show peer ++ " from " ++ show local
  sock <- clientConnect port peer local
  putStrLn "active: connected"
  ifM (common sock peer local) exitSuccess (die "unexpected transport addreses in active connect")
  gracefulClose sock 100000
  putStrLn "disconnected"

common :: Socket -> IPv4 -> IPv4 -> IO Bool
common sock expectedPeerAddress expectedLocalAddress = do
  ((_, receivedLocalAddress), (_, receivedPeerAddress)) <- getSockAddresses sock
  if (expectedPeerAddress, expectedLocalAddress) == (receivedPeerAddress, receivedLocalAddress)
    then do
      putStrLn $ "got expected connection with " ++ show receivedPeerAddress
      validateConnection sock
    else do
      putStrLn $ "got unwanted connection - got " ++ show (receivedPeerAddress, receivedLocalAddress) ++ " expecting " ++ show (expectedPeerAddress, expectedLocalAddress)
      close sock
      return False
  where
    validateConnection sock = do
      handle <- socketToHandle sock System.IO.ReadWriteMode
      hPutKeepalive handle
      hPutKeepalive handle
      gen <- getGenerator (ByteString.hGetSome handle 4096) bgpParser1
      msg <- gen
      maybe
        ( do
            putStrLn "immediate end of stream"
            return False
        )
        ( \msg -> do
            putStrLn $ "received " ++ show msg
            return True
        )
        msg

-- below and needed imports should be in a seprate module, which should be provided by BGPlib
hPutKeepalive :: Handle -> IO ()
hPutKeepalive handle = hPutBuilder handle keepaliveBuilder
  where
    keepaliveBuilder :: Builder
    keepaliveBuilder =
      marker
        <> word16BE 19
        <> word8 4
    marker :: Builder
    marker = byteStringCopy $ ByteString.replicate 16 0xff
