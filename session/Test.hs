{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad (forever)
import qualified Network.Socket as NS
import Data.IP
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C8


main :: IO ()    
main = do
    sock <- connectTo "169.254.99.99" 5000 "169.254.99.98"
    forever (do sendAll sock $ C8.pack "Hello"
                threadDelay $ 10^7
            )
                
connectTo :: IPv4 -> NS.PortNumber -> IPv4 -> IO NS.Socket
connectTo localIP port remoteIP = do
    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    catchIOError
        ( do NS.setSocketOption sock NS.ReuseAddr 1
             NS.setSocketOption sock NS.NoDelay 1
             NS.bind sock (NS.SockAddrInet NS.defaultPort $ toHostAddress localIP)
             NS.connect sock $ NS.SockAddrInet port $ toHostAddress remoteIP
             return sock )

        (\e -> do
            Errno errno <- getErrno
            putStrLn $ "Exception connecting to " ++ show remoteIP ++ " from " ++ show localIP ++ " - " ++ errReport errno e
            return sock )

    where

    errReport errno e | errno `elem` [2,107,115] = ioe_description e ++ " (" ++ show errno ++ ")"
                      | otherwise = errReport' errno e
    
    errReport' errno e = unlines
        [ "*** UNKNOWN exception, please record this"
        -- , ioeGetErrorString e
        , "error " ++ ioeGetErrorString e
        , "errno " ++ show errno
        , "description " ++ ioe_description e
        ]
