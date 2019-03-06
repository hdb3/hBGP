{-# LANGUAGE OverloadedStrings #-}

{-

   A simple network server application, showcasing the forking of multiple threads

-}
module Main where

import Data.Char(toUpper)
import qualified Data.ByteString.Char8 as C
import System.Environment(getArgs)
import Control.Concurrent
import Control.Monad (void,forever)
import Control.Applicative((<$>))
import Network.Socket hiding (send,recv)
import Network.Socket.ByteString(send,recv)
import System.IO

main :: IO ()
main = do
    fork <- inArgs "fork"
    putStrLn $ "Fork: " ++ show fork
    let port = 179
        app = echo
    listeningSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port 0 )
    listen listeningSocket 100
    forever $ do
        (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
        logger $ "listener - connect request from " ++ show remoteIPv4 ++ ":" ++ show remotePort
        if fork then
            void $ forkIO $ app sock
        else
            app sock

    where

    logger = hPutStrLn stderr

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

    inArgs :: String -> IO Bool
    inArgs s = do
        args <- getArgs
        return ( map toUpper s `elem` map (map toUpper) args)
