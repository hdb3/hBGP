{-# LANGUAGE OverloadedStrings #-}

{-

   A simple network server application, showcasing the forking of multiple threads

-}
module Main where

import qualified Data.ByteString as BS
import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString(recv)
import Data.IP
import Control.Concurrent

main :: IO ()
main = do
    let port = 5000
        localIP = toHostAddress "169.254.99.98"
        app = devNull (10^8) 0 ; startDelay = 0
        --app = recvLoop (10^6) 0 ; startDelay = 10^8
    listeningSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port localIP )
    listen listeningSocket 1
    forever $ do
                (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
                putStrLn $ "Server - connect request from " ++ show ( fromHostAddress remoteIPv4 ) ++ ":" ++ show remotePort
                serve sock app startDelay
                putStrLn $ "Server - session close from " ++ show ( fromHostAddress remoteIPv4 ) ++ ":" ++ show remotePort

    where

    serve sock app sd = do
        putStrLn "Server process starting"
        peerAddress  <- getPeerName sock
        localAddress <- getSocketName sock
        putStrLn $ "Server process - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
        threadDelay sd
        putStrLn "Server loop starting"
        n <- app sock
        putStrLn $ "Server loop exit: " ++ show n
        close sock
        --return ()

    -- recvLoop slowly drains the queue, with a configurable pause
    recvLoop pause n sock = do
        threadDelay pause
        reply <- recv sock 4096
        putStrLn $ "received " ++ show (BS.length reply) ++ "/" ++ show (n + BS.length reply)
        if BS.null reply
        then
            return n
        else
            recvLoop pause (n + BS.length reply) sock

    devNull bs n sock = do
        reply <- recv sock bs
        if BS.null reply
        then
            return n
        else
            devNull bs (n + BS.length reply) sock
