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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.IO(IOMode( ReadWriteMode ))
import qualified System.Posix.Types as SPT
import Data.Time.Clock

import Poll

main :: IO ()    
main = do
    --let app = loop ; blockSize = 1024
    let app = nullLoop (10^6) (10^3)
    --let app = nullLoop (10^3) (10^6)
    sock <- connectTo "169.254.99.99" 5000 "169.254.99.98"
    fd  <- NS.fdSocket sock
    handle <- NS.socketToHandle sock ReadWriteMode
    app handle (SPT.Fd fd)

    where

    loop h fd bs n = do 
        BS.hPut h $ BS.replicate bs 0
        fdWaitOnQEmpty fd
        putStrLn $ "Client: put " ++ show bs ++ "/" ++ show (n + bs)
        --sendAll sock $ BS.replicate bs 0
        --waitOnQEmpty sock
        --threadDelay $ 10^7
        loop h fd bs (n + bs)

    nullLoop burstSize count h fd = do 
        t0 <- getCurrentTime
        go ( BS.replicate burstSize 0 ) count
        t1 <- getCurrentTime
        fdWaitOnQEmpty fd
        t2 <- getCurrentTime
        putStrLn $ "Client: put " ++ show count ++ " blocks of size " ++ show burstSize
        putStrLn $ "Client: elapsed times = " ++ show ( diffUTCTime t2 t0 ) ++ " total " ++ show ( diffUTCTime t1 t0 ) ++ " before ACK "++ show ( diffUTCTime t2 t1 ) ++ " after ACK "
        where
            go bs n = if n == 0 then return () else do BS.hPut h bs ; go bs (n-1)

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

    errReport errno e | errno `elem` [2,107,115] = ioe_description e ++ " (" ++ show errno ++ ")"
                      | otherwise = errReport' errno e
    
    errReport' errno e = unlines
        [ "*** UNKNOWN exception, please record this"
        -- , ioeGetErrorString e
        , "error " ++ ioeGetErrorString e
        , "errno " ++ show errno
        , "description " ++ ioe_description e
        ]
