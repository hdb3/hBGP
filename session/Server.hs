{-# LANGUAGE OverloadedStrings #-}

{-

   A simple network server application, showcasing the forking of multiple threads

-}
module Main where

import Data.Char(toUpper)
import qualified Data.ByteString.Char8 as C
--import Control.Concurrent
import Control.Monad (void,forever)
import Control.Applicative((<$>))
import Network.Socket
import Network.Socket.ByteString(send,recv)
--import System.IO
import Data.IP

main :: IO ()
main = do
    let port = 179
        localIP = "169.254.99.98"
    listeningSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port $ toHostAddress localIP )
    listen listeningSocket 1
    forever $ do
                (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
                putStrLn $ "listener - connect request from " ++ show remoteIPv4 ++ ":" ++ show remotePort
                echo sock

    where

    echo sock = do
        putStrLn "echo starting"
        peerAddress  <- getPeerName sock
        localAddress <- getSocketName sock
        putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
        void $ send sock $ C.pack "hello friend\n"
        reply <- C.unpack <$> recv sock 4096
        putStrLn $ "my friend said: \"" ++ reply ++ "\"\n"
        void $ send sock $ C.pack $ "you said " ++ map toUpper reply ++ "\n"
        void $ send sock $ C.pack "Goodbye!\n"
        close sock
        return ()
