{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Data.IP
import Foreign.C.Error
import GHC.IO.Exception (ioe_description)
import qualified Network.Socket as NS
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Error
import Text.Read (readMaybe)

retryOnBusy = False

main :: IO ()
main = do
  putStrLn "bgpclient"
  local <- getArgIPv4 0
  peer <- getArgIPv4 1
  talker 179 peer local
  where
    getArgIPv4 n = do
      args <- getArgs
      if n > 1 + length args
        then die $ "can't read address in argument " ++ show n
        else
          maybe
            (die $ "can't read address in argument " ++ show n)
            return
            (readMaybe (args !! n))

talker :: NS.PortNumber -> IPv4 -> IPv4 -> IO ()
talker port peer local = do
  putStrLn $ "connecting to " ++ show peer ++ " from " ++ show local
  sock <- connect port peer local
  putStrLn "connected"
  NS.gracefulClose sock 100000
  putStrLn "disconnected"

connect :: NS.PortNumber -> IPv4 -> IPv4 -> IO NS.Socket
connect port peer local = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.NoDelay 1
  bind sock local
  connect' sock port peer
  return sock
  where
    connect' :: NS.Socket -> NS.PortNumber -> IPv4 -> IO ()
    connect' sock port peer =
      catchIOError
        (NS.connect sock (NS.SockAddrInet port (toHostAddress peer)))
        ( \e -> do
            Errno errno <- getErrno
            if  | elem errno [2, 103] -> do
                  putStrLn $ ioe_description e ++ "(" ++ show errno ++ ") retrying in 3 seconds"
                  threadDelay 3000000 -- 3 seconds
                  connect' sock port peer
                | otherwise -> do
                  putStrLn "connect error"
                  putStrLn $ "errorString: " ++ ioeGetErrorString e
                  putStrLn $ "errno " ++ show errno
                  die $ "description " ++ ioe_description e
        )
    bind :: NS.Socket -> IPv4 -> IO ()
    bind sock addr =
      catchIOError
        (NS.bind sock (NS.SockAddrInet 0 $ toHostAddress addr))
        ( \e -> do
            Errno errno <- getErrno
            if  | errno == 99 ->
                  error "address error binding port - host configuration mismatch?"
                | otherwise -> do
                  putStrLn "bind error"
                  putStrLn $ "errorString: " ++ ioeGetErrorString e
                  putStrLn $ "errno " ++ show errno
                  putStrLn $ "description " ++ ioe_description e
            die "fatal error can't continue"
        )
