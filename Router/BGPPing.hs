{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad                  ( forever )
import           Data.IP
import           Foreign.C.Error
import           GHC.IO.Exception               ( ioe_description )
import qualified Network.Socket                as NS
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )
import           System.IO
import           System.IO.Error

main :: IO ()
main = do
  putStrLn "bgpping"
  args <- getArgs
  if null args then listener 179 ("0.0.0.0", "0.0.0.0") else die "no work to do"

listener :: NS.PortNumber -> (IPv4, IPv4) -> IO ()
listener port (local, peer) = do
  eSock <- tryIOError (bindSock' port (toHostAddress local))
  either
    (\e -> do
      Errno errno <- getErrno
      if
        | errno == 13
        -> die
          "permission error binding port (are you su?) (or try: sysctl net.ipv4.ip_unprivileged_port_start=179?)"
        | errno == 99
        -> die "address error binding port - host configuration mismatch?"
        | errno `elem` [98]
        -> do
          hPutStrLn stderr "waiting to bind port"
          threadDelay 10000000
          listener port (local, peer)
        | otherwise
        -> error $ errReport' errno e
    )
    (\(listeningSocket, _) -> forever
      (do
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
    let addressPair =
          (fromHostAddress localHostAddress, fromHostAddress remoteHostAddress)
    wrap sock

wrap :: NS.Socket -> IO ()
wrap sock = do
  peerAddress <- NS.getPeerName sock
  let ip = fromPeerAddress peerAddress
      fromPeerAddress (NS.SockAddrInet _ ip) = fromHostAddress ip
  catchIOError
    (do
      putStrLn "connected"
      NS.close sock
      putStrLn $ "app terminated for : " ++ show ip
    )
    (\e -> do
      Errno errno <- getErrno
      putStrLn
        $  "Exception in session with "
        ++ show ip
        ++ " - "
        ++ errReport errno e
    )

run :: NS.PortNumber -> (IPv4, IPv4) -> IO ()
run port (src, dst) = do
  putStrLn $ "run: " ++ show (src, dst) ++ " start"
  sock <- connectTo port (src, dst)
  putStrLn $ "run: " ++ show (src, dst) ++ " connected"
 where
  connectTo port (src, dst) = catchIOError
    (do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption sock NS.NoDelay 1
      NS.bind sock (NS.SockAddrInet 0 $ toHostAddress src)
      NS.connect sock $ NS.SockAddrInet port $ toHostAddress dst
      return $ Just sock
    )
    (\e -> do
      Errno errno <- getErrno
      -- most errors are timeouts or connection rejections from unattended ports
      -- a better way to report would be handy - repeated console messages are not useful!
      putStrLn
        $  "Exception connecting to "
        ++ show dst
        ++ " - "
        ++ errReport errno e
      return Nothing
    )

errReport errno e
  | errno `elem` [2, 32, 99, 104, 107, 115]
  = ioe_description e ++ " (" ++ show errno ++ ")"
  | otherwise
  = errReport' errno e

errReport' errno e = unlines
  [ "*** UNKNOWN exception, please record this"
  , "error " ++ ioeGetErrorString e
  , "errno " ++ show errno
  , "description " ++ ioe_description e
  ]
