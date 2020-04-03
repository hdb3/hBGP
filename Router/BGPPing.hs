{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Extra (ifM)
import Data.IP
import Data.List (partition)
import Data.Maybe (fromJust)
-- import Data.Bool.Extras(bool)
import Foreign.C.Error
import GHC.IO.Exception (ioe_description)
import qualified Network.Socket as NS
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
  putStrLn $ "talker connecting from " ++ show local ++ " to " ++ show peer
  --sock <- unwrappedConnect port peer local
  sock <- wrappedConnect port peer local
  putStrLn "active: connected"
  ifM (common sock peer local) exitSuccess (die "unexpected transport addreses in active connect")
  where
    unwrappedConnect :: NS.PortNumber -> IPv4 -> IPv4 -> IO NS.Socket
    unwrappedConnect port peer local = do
      sock <- newSock
      setNoDelay sock
      bind sock local
      connect sock port peer
      return sock
    wrappedConnect :: NS.PortNumber -> IPv4 -> IPv4 -> IO NS.Socket
    wrappedConnect port peer local = catchIOError (unwrappedConnect port peer local) connectErrorHandler
      where
        connectErrorHandler e = do
          Errno errno <- getErrno
          if  | errno == 99 ->
                error "address error binding port - host configuration mismatch?"
              | otherwise -> do
                putStrLn $ "errorString: " ++ ioeGetErrorString e
                putStrLn $ "errno " ++ show errno
                putStrLn $ "description " ++ ioe_description e
                threadDelay 3000000 -- 3 seconds
                wrappedConnect port peer local

common :: NS.Socket -> IPv4 -> IPv4 -> IO Bool
common sock expectedPeerAddress expectedLocalAddress = do
  receivedPeerAddress <- getPeerAddress sock
  receivedLocalAddress <- getLocalAddress sock
  if (expectedPeerAddress, expectedLocalAddress) == (receivedPeerAddress, receivedLocalAddress)
    then do
      putStrLn $ "got expected connection with " ++ show receivedPeerAddress
      return True
    else do
      putStrLn $ "got unwanted connection - got " ++ show (receivedPeerAddress, receivedLocalAddress) ++ " expecting " ++ show (expectedPeerAddress, expectedLocalAddress)
      NS.close sock
      return False

sockAddr :: NS.SockAddr -> IPv4
sockAddr (NS.SockAddrInet _ hostAddress) = fromHostAddress hostAddress

getPeerAddress :: NS.Socket -> IO IPv4
getPeerAddress sock = fmap sockAddr (NS.getPeerName sock)

getLocalAddress :: NS.Socket -> IO IPv4
getLocalAddress sock = fmap sockAddr (NS.getSocketName sock)

newSock = NS.socket NS.AF_INET NS.Stream NS.defaultProtocol

bind s addr = catchIOError (NS.bind s (NS.SockAddrInet 0 $ toHostAddress addr)) (genericErrorHandler "bind")

connect s port addr = catchIOError (NS.connect s (NS.SockAddrInet port (toHostAddress addr))) (genericErrorHandler "connect")

setNoDelay s = NS.setSocketOption s NS.NoDelay 1 >> return s

genericErrorHandler s e = do
  putStrLn $ "error in " ++ s
  Errno errno <- getErrno
  putStrLn $ "errorString: " ++ ioeGetErrorString e
  putStrLn $ "errno " ++ show errno
  putStrLn $ "description " ++ ioe_description e

-- experimenatal stuff #########################################

connectTo'' port local remote = newSock'' >>= setNoDelay'' >>= bind'' local >>= connect'' port remote
  where
    newSock'' = NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    bind'' addr s = catchIOError (NS.bind s (NS.SockAddrInet 0 $ toHostAddress addr) >> return s) (genericErrorHandler'' "bind")
    connect'' port addr s = catchIOError (NS.connect s (NS.SockAddrInet port (toHostAddress addr)) >> return s) (genericErrorHandler'' "connect")
    setNoDelay'' s = NS.setSocketOption s NS.NoDelay 1 >> return s
    genericErrorHandler'' s e = do
      putStrLn $ "error in " ++ s
      Errno errno <- getErrno
      putStrLn $ "errorString: " ++ ioeGetErrorString e
      putStrLn $ "errno " ++ show errno
      putStrLn $ "description " ++ ioe_description e
      return $ error "can't help with this one...."

connectTo' port local remote = newSock' >>= setNoDelay' >>= bind' local >>= connect' port remote
  where
    newSock' = Just <$> NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    bind' addr (Just sock) = catchIOError (NS.bind sock (NS.SockAddrInet 0 $ toHostAddress addr) >> return (Just sock)) (genericErrorHandler' sock "bind")
    bind' _ Nothing = return Nothing
    connect' port addr (Just sock) = catchIOError (NS.connect sock (NS.SockAddrInet port (toHostAddress addr)) >> return (Just sock)) (genericErrorHandler' sock "connect")
    connect' _ _ Nothing = return Nothing
    setNoDelay' (Just s) = NS.setSocketOption s NS.NoDelay 1 >> return (Just s)
    setNoDelay' Nothing = return Nothing
    genericErrorHandler' sock s e = do
      putStrLn $ "error in " ++ s
      Errno errno <- getErrno
      putStrLn $ "errorString: " ++ ioeGetErrorString e
      putStrLn $ "errno " ++ show errno
      putStrLn $ "description " ++ ioe_description e
      NS.close sock
      return Nothing
