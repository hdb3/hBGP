{-# LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char(toUpper)
import System.Environment(getArgs)
import Data.Maybe
import Control.Concurrent
import Control.Monad (void,unless,when,forever)
import Network.Socket(SockAddr(..))
import System.IO
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error
import Network.Simple.TCP

logger = hPutStrLn stderr
main = do 
    fork <- inArgs "fork"
    putStrLn $ "Fork: " ++ show fork
    let port = 5000
        app = echo
    listeningSocket <- socket AF_INET Stream defaultProtocol 
    setSocketOption listeningSocket ReuseAddr 1
    bind listeningSocket ( SockAddrInet port 0 )
    listen listeningSocket 100
    forever $ do
        (sock, SockAddrInet remotePort remoteIPv4) <- accept listeningSocket
        SockAddrInet _ addr <- getPeerName' sock
        let ip = fromHostAddress addr
        let ip' = fromHostAddress remoteIPv4
        logger $ "listener - connect request from " ++ show ip
        if fork then
            void $ forkIO $ app sock 
        else
            app sock 

        close sock

wrap app sock = do
    SockAddrInet _ addr <- getPeerName' sock
    let ip = fromHostAddress addr
    catchIOError
        ( do logger $ "connected to : " ++ show ip
             app sock
             logger $ "app terminated for : " ++ show ip )
        (\e -> do Errno errno <- getErrno
                  logger $ "Exception in session with " ++ show ip ++ " - " ++ errReport errno e )
    close sock


getPeerName' sock = 
    catchIOError
        ( getPeerName sock )
        (\e -> do Errno errno <- getErrno
                  hPutStrLn stderr $ "Exception in getPeerName - " ++ errReport errno e 
                  return $ SockAddrInet 0 0 )


errReport 2 e = ioe_description e
errReport errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]

echo sock = do
    putStrLn "echo starting"
    peerAddress  <- getPeerName' sock
    -- let peerAddress = SockAddrInet 0 0
    localAddress <- getSocketName sock
    putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    send sock "hello friend\n"
    reply <- recv sock 4096
    putStrLn $ "my friend said: \"" ++ reply ++ "\"\n"
    send sock $ "you said " ++ (map toUpper reply) ++ "\n"
    send sock "Goodbye!\n"
    return ()

inArgs :: String -> IO Bool
inArgs s = do
    args <- getArgs
    return ( map toUpper s `elem` map (map toUpper) args)
