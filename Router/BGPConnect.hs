{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.BGPConnect(clientConnect, getServerSession, openServerSocket, showSockAddresses) where

import Control.Concurrent
import Data.IP
import Foreign.C.Error
import GHC.IO.Exception (ioe_description)
import qualified Network.Socket as NS
import System.Exit (die)
import System.IO
import System.IO.Error

retryOnBusy = False

clientConnect :: NS.PortNumber -> IPv4 -> IPv4 -> IO NS.Socket
clientConnect port peer local = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.NoDelay 1
  bind sock local
  connect' sock port peer
  return sock
  where
    connect' :: NS.Socket -> NS.PortNumber -> IPv4 -> IO ()
    connect' sock port peer =
      catchIOError
        (NS.connect sock (NS.SockAddrInet port (toHostAddress peer)))
        ( \e -> do
            Errno errno <- getErrno
            if  | elem errno [2, 103] -> do
                  putStrLn $ ioe_description e ++ "(" ++ show errno ++ ") retrying in 3 seconds"
                  threadDelay 3000000 -- 3 seconds
                  connect' sock port peer
                | otherwise -> unknownSocketErrorHandler e errno
        )
    bind :: NS.Socket -> IPv4 -> IO ()
    bind sock addr =
      catchIOError
        (NS.bind sock (NS.SockAddrInet 0 $ toHostAddress addr))
        ( \e -> do
            Errno errno <- getErrno
            if  | errno == 99 -> die "address error binding port - host configuration mismatch?"
                | otherwise -> unknownSocketErrorHandler e errno
        )

getServerSession :: NS.Socket -> IO NS.Socket
getServerSession listeningSocket = fst <$> NS.accept listeningSocket

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
