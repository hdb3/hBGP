{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPlib.AttoBGP (bgpParser1)
import BGPlib.BGPlib (BGPMessage)
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Extra (ifM)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra (byteStringCopy)
import Data.IP
import Data.List (partition)
import Data.Maybe (fromJust)
import Foreign.C.Error
import GHC.IO.Exception (ioe_description)
import qualified Network.Socket as NS
import Router.BGPPingParser
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO
import System.IO.Error
import Text.Read (readMaybe)

retryOnBusy = False

main :: IO ()
main = do
  putStrLn "bgpping"
  args <- getArgs
  let (optargs, posargs) = partition (('-' ==) . head) args
      addresses = map getIPv4 posargs
      passive = elem "--listen" optargs
  if  | (elem Nothing addresses) -> do
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

listener :: NS.PortNumber -> IPv4 -> IPv4 -> IO ()
listener port peer local = do
  listeningSocket <- bind port local
  putStrLn $ "listener waiting on " ++ show local ++ " for connection from " ++ show peer
  forever
    ( do
        (sock, _) <- NS.accept listeningSocket
        ifM (common sock peer local) exitSuccess (putStrLn "unexpected transport addresses in passive connect - continuing")
    )
  where
    bind :: NS.PortNumber -> IPv4 -> IO NS.Socket
    bind port address = catchIOError (unwrappedBind port address) bindErrorHandler
      where
        --
        unwrappedBind :: NS.PortNumber -> IPv4 -> IO NS.Socket
        unwrappedBind port ip = do
          sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
          NS.setSocketOption sock NS.ReuseAddr 1
          NS.setSocketOption sock NS.NoDelay 1
          NS.bind sock (NS.SockAddrInet port (toHostAddress ip))
          NS.listen sock 100
          return sock
        --
        bindErrorHandler e = do
          Errno errno <- getErrno
          if  | errno == 13 ->
                die
                  "permission error binding port (are you su?) (or try: sysctl net.ipv4.ip_unprivileged_port_start=179?)"
              | errno == 99 ->
                die "address error binding port - host configuration mismatch?"
              | errno `elem` [98] ->
                if retryOnBusy
                  then do
                    hPutStrLn stderr "waiting to bind port"
                    threadDelay 10000000 -- 10 seconds
                    bind port address
                  else die "port already in use"
              | otherwise ->
                die $
                  unlines
                    [ "*** UNKNOWN exception, please record this",
                      "error " ++ ioeGetErrorString e,
                      "errno " ++ show errno,
                      "description " ++ ioe_description e
                    ]

talker :: NS.PortNumber -> IPv4 -> IPv4 -> IO ()
talker port peer local = do
  putStrLn $ "connecting to " ++ show peer ++ " from " ++ show local
  sock <- newSock
  setNoDelay sock
  bind sock local
  connect sock port peer
  putStrLn "active: connected"
  ifM (common sock peer local) exitSuccess (die "unexpected transport addreses in active connect")
  where
    connect :: NS.Socket -> NS.PortNumber -> IPv4 -> IO ()
    connect sock port peer =
      catchIOError
        (NS.connect sock (NS.SockAddrInet port (toHostAddress peer)))
        ( \e -> do
            Errno errno <- getErrno
            if  | elem errno [2, 103] -> do
                  putStrLn $ ioe_description e ++ "(" ++ show errno ++ ") retrying in 3 seconds"
                  threadDelay 3000000 -- 3 seconds
                  connect sock port peer
                | otherwise -> do
                  putStrLn "connect error"
                  putStrLn $ "errorString: " ++ ioeGetErrorString e
                  putStrLn $ "errno " ++ show errno
                  putStrLn $ "description " ++ ioe_description e
                  putStrLn "retrying in 3 seconds"
                  threadDelay 3000000 -- 3 seconds
                  connect sock port peer
        )
    bind :: NS.Socket -> IPv4 -> IO ()
    bind sock addr =
      catchIOError
        (NS.bind sock (NS.SockAddrInet 0 $ toHostAddress addr))
        ( \e -> do
            Errno errno <- getErrno
            if  | errno == 99 ->
                  error "address error binding port - host configuration mismatch?"
                | otherwise -> do
                  putStrLn "biind error"
                  putStrLn $ "errorString: " ++ ioeGetErrorString e
                  putStrLn $ "errno " ++ show errno
                  putStrLn $ "description " ++ ioe_description e
            die ("fatal error can't continue")
        )

common :: NS.Socket -> IPv4 -> IPv4 -> IO Bool
common sock expectedPeerAddress expectedLocalAddress = do
  receivedPeerAddress <- getPeerAddress sock
  receivedLocalAddress <- getLocalAddress sock
  if (expectedPeerAddress, expectedLocalAddress) == (receivedPeerAddress, receivedLocalAddress)
    then do
      putStrLn $ "got expected connection with " ++ show receivedPeerAddress
      validateConnection sock
    else do
      putStrLn $ "got unwanted connection - got " ++ show (receivedPeerAddress, receivedLocalAddress) ++ " expecting " ++ show (expectedPeerAddress, expectedLocalAddress)
      NS.close sock
      return False
  where
    validateConnection sock = do
      handle <- NS.socketToHandle sock System.IO.ReadWriteMode
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

-- if this is a real BGP peer session then we should expect some input
-- we can safely send any valid BGP message (keepalive? open?) - or wait for an OPEN?
-- if the response comes to our message and can be parsed then unless we want to verify remote identity and configuration that is sufficient....

sockAddr :: NS.SockAddr -> IPv4
sockAddr (NS.SockAddrInet _ hostAddress) = fromHostAddress hostAddress

getPeerAddress :: NS.Socket -> IO IPv4
getPeerAddress sock = fmap sockAddr (NS.getPeerName sock)

getLocalAddress :: NS.Socket -> IO IPv4
getLocalAddress sock = fmap sockAddr (NS.getSocketName sock)

newSock = NS.socket NS.AF_INET NS.Stream NS.defaultProtocol

setNoDelay s = NS.setSocketOption s NS.NoDelay 1 >> return s

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
