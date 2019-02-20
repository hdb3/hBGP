{-#LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Data.IP
import qualified System.IO.Streams as Streams
import Text.Read

import ZServ

main :: IO ()
main = do
    args <- getArgs
    let s = args !! 0
    (inputStream,outputStream) <- 
        maybe ( getZStreamUnix s )
              getZStreamInet
              ( readMaybe s :: Maybe IPv4)

    zservRegister outputStream _ZEBRA_ROUTE_BGP
    zservRequestRouterId outputStream
    loop inputStream
    where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              print zMsg
                              loop stream )
              msg
