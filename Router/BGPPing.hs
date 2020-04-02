{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import Data.IP
import Data.List (partition)
import Data.Maybe (fromJust)
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
  putStrLn "bgpping"
  args <- getArgs
  let (optargs, posargs) = partition (('-' ==) . head) args
      addresses = map getIPv4 posargs
      passive = elem "--listen" optargs
  -- print (args,optargs, posargs,passive,addresses)
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
  eSock <- tryIOError (bindSock' port (toHostAddress local))
  either
    ( \e -> do
        Errno errno <- getErrno
        if  | errno == 13 ->
              die
                "permission error binding port (are you su?) (or try: sysctl net.ipv4.ip_unprivileged_port_start=179?)"
            | errno == 99 ->
              die "address error binding port - host configuration mismatch?"
            | errno `elem` [98] ->
              do
                hPutStrLn stderr "waiting to bind port"
                threadDelay 10000000
                listener port peer local
            | otherwise ->
              error $ errReport' errno e
    )
    ( \(listeningSocket, _) -> do
        putStrLn "listener waiting"
        forever
          ( do
              s <- NS.accept listeningSocket
              forkIO $ listenClient s
          )
    )
    eSock
  where
    bindSock' port ip = do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.setSocketOption sock NS.NoDelay 1
      NS.bind sock (NS.SockAddrInet port ip)
      NS.listen sock 100
      return (sock, addr)
    listenClient (sock, NS.SockAddrInet _ remoteHostAddress) = do
      (NS.SockAddrInet _ localHostAddress) <- NS.getSocketName sock
      let addressPair = (fromHostAddress localHostAddress, fromHostAddress remoteHostAddress)
      putStrLn $ "received connect from " ++ show (fromHostAddress remoteHostAddress)
      wrap sock

wrap :: NS.Socket -> IO ()
wrap sock = do
  peerAddress <- NS.getPeerName sock
  let ip = fromPeerAddress peerAddress
      fromPeerAddress (NS.SockAddrInet _ ip) = fromHostAddress ip
  catchIOError
    ( do
        putStrLn "connected"
        NS.close sock
        putStrLn $ "app terminated for : " ++ show ip
    )
    ( \e -> do
        Errno errno <- getErrno
        putStrLn $
          "Exception in session with "
            ++ show ip
            ++ " - "
            ++ errReport errno e
    )

talker :: NS.PortNumber -> IPv4 -> IPv4 -> IO ()
talker port peer local = do
  putStrLn $ "run: " ++ show (peer, local) ++ " start"
  sock <- connectTo port peer local
  putStrLn "run: connected"
  where
    connectTo port peer local =
      catchIOError
        ( do
            sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
            NS.setSocketOption sock NS.NoDelay 1
            NS.bind sock (NS.SockAddrInet 0 $ toHostAddress local)
            NS.connect sock $ NS.SockAddrInet port $ toHostAddress peer
            return $ Just sock
        )
        ( \e -> do
            Errno errno <- getErrno
            -- most errors are timeouts or connection rejections from unattended ports
            -- a better way to report would be handy - repeated console messages are not useful!
            putStrLn $
              "Exception connecting to "
                ++ show peer
                ++ " - "
                ++ errReport errno e
            return Nothing
        )

errReport errno e
  | errno `elem` [2, 32, 99, 104, 107, 115] =
    ioe_description e ++ " (" ++ show errno ++ ")"
  | otherwise =
    errReport' errno e

errReport' errno e =
  unlines
    [ "*** UNKNOWN exception, please record this",
      "error " ++ ioeGetErrorString e,
      "errno " ++ show errno,
      "description " ++ ioe_description e
    ]
