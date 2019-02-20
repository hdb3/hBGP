{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char(toUpper)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as C
import System.Environment(getArgs)
import Data.Maybe
import Control.Concurrent
import Control.Monad (void,unless,when,forever)
import Network.Socket(getSocketName,getPeerName,SockAddr(..))
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
    serve HostIPv4 "5000" echo

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

echo (sock,addr) = do
    putStrLn "echo starting"
    peerAddress  <- getPeerName' sock
    localAddress <- getSocketName sock
    putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    send sock $ C.pack $ "hello friend\n"
    reply' <- recv sock 4096
    maybe (return ())
          ( \reply -> do putStrLn $ "my friend said: \"" ++ reply ++ "\"\n"
                         send sock $ C.pack $ "you said " ++ (map toUpper reply) ++ "\n"
                         send sock $ C.pack $ "Goodbye!\n"
                         return () )
          (fmap C.unpack reply')
    return ()

inArgs :: String -> IO Bool
inArgs s = do
    args <- getArgs
    return ( map toUpper s `elem` map (map toUpper) args)
