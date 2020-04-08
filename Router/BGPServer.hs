{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Data.IP
import Foreign.C.Error
import GHC.IO.Exception (ioe_description)
import qualified Network.Socket as NS
import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import System.IO.Error
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

retryOnBusy = False

listener :: NS.PortNumber -> IPv4 -> IO ()
listener port local = do
  serverSocket <- openServerSocket port local
  putStrLn $ "listener waiting on " ++ show local ++ ":" ++ show port
  sock1 <- getSession serverSocket
  putStr "got connection (1): "
  showSockAddresses sock1 >>= putStrLn
  sock2 <- getSession serverSocket
  putStr "got connection (2): "
  showSockAddresses sock2 >>= putStrLn
  NS.gracefulClose sock1 100000
  NS.gracefulClose sock2 100000
  putStrLn "disconnected"

getSession :: NS.Socket -> IO NS.Socket
getSession listeningSocket = fst <$> NS.accept listeningSocket

openServerSocket :: NS.PortNumber -> IPv4 -> IO NS.Socket
openServerSocket port address = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.setSocketOption sock NS.NoDelay 1
  serverBind sock port address
  NS.listen sock 100
  return sock
  where
    serverBind :: NS.Socket -> NS.PortNumber -> IPv4 -> IO ()
    serverBind sock port ip =
      catchIOError
        (NS.bind sock (NS.SockAddrInet port (toHostAddress ip)))
        ( \e -> do
            Errno errno <- getErrno
            if  | errno == 13 ->
                  die
                    "permission error binding port (are you su?) (or try: sysctl net.ipv4.ip_unprivileged_port_start=179?)"
                | errno == 99 ->
                  die "address error binding port - host configuration mismatch?"
                | errno == 98 ->
                  if retryOnBusy
                    then do
                      hPutStrLn stderr "waiting to bind port"
                      threadDelay 10000000 -- 10 seconds
                      serverBind sock port address
                    else die "port already in use"
                | otherwise -> unknownSocketErrorHandler e errno
        )

unknownSocketErrorHandler e errno =
  die $
    unlines
      [ "*** UNKNOWN exception, please record this",
        "error " ++ ioeGetErrorString e,
        "errno " ++ show errno,
        "description " ++ ioe_description e
      ]

showSockAddresses :: NS.Socket -> IO String
showSockAddresses sock = do
  peer <- NS.getPeerName sock
  local <- NS.getSocketName sock
  return $ show (local, peer)
