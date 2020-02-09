{-# LANGUAGE OverloadedStrings #-}
{-

   BGPwaiter - wait for an incoming connection on port 179
   and report the addreses it was received on.

-}
module Main where
import Network.Socket
import Data.IP

main :: IO ()
main = do
    listeningSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet 179 ( toHostAddress "192.168.122.1") )
    --bind listeningSocket ( SockAddrInet 8179 ( toHostAddress "0.0.0.0") )
    listen listeningSocket 1
    (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
    putStrLn $ "BGPwaiter - connect request from " ++ show ( fromHostAddress remoteIPv4 ) ++ ":" ++ show remotePort
    peerAddress  <- getPeerName sock
    localAddress <- getSocketName sock
    putStrLn $ "BGPwaiter process - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    close sock
    (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
    putStrLn $ "BGPwaiter process - Done"
