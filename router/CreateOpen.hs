{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO(stdout)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Data.IP
import Data.Word

import BGPlib

main :: IO ()
main = writeOpen defaultOpen

writeOpen :: BGPMessage -> IO ()
writeOpen = L.hPut stdout . wireFormat . encode

defaultOpen :: BGPMessage
defaultOpen = simpleOpen 65534 "0.0.0.0"

simpleOpen :: Word32 -> IPv4 -> BGPMessage
simpleOpen as routerID | as < 0x10000 = BGPOpen (fromIntegral as) 0 routerID [CapAS4 as]
                       | otherwise = BGPOpen 23456 0 routerID [CapAS4 as]
