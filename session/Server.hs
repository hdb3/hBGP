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
        localIP = "169.254.99.98"
    listeningSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port $ toHostAddress localIP )
    listen listeningSocket 1
    forever $ do
                (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
                putStrLn $ "listener - connect request from " ++ show remoteIPv4 ++ ":" ++ show remotePort
                serve sock

    where

    serve sock = do
        putStrLn "serve starting"
        peerAddress  <- getPeerName sock
        localAddress <- getSocketName sock
        putStrLn $ "serve - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
        recvLoop sock
        close sock
        return ()

    recvLoop sock = do
        threadDelay $ 10^6
        reply <- recv sock 4096
        putStrLn $ "received " ++ show (BS.length reply)
        if BS.null reply
        then
            return ()
        else
            recvLoop sock
