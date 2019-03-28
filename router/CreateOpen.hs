{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO(stdout)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Data.IP
import Data.Word

import BGPlib
import ArgConfig

-- data BGPMessage = BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: IPv4, caps :: [ Capability ] }

-- simple fixed version...
--main = writeOpen defaultOpen

-- command line configured version
main :: IO ()
main = do
    dict <- buildDictionary
    --writeOpen $ defaultOpen { myAutonomousSystem = getVal dict 65534 "as" , holdTime = getVal dict 0 "holdTime" , bgpID = getVal dict "0.0.0.0" "bgpID" }
    writeOpen $ ( simpleOpen (getVal dict 65534 "as") (getVal dict "0.0.0.0" "bgpID") ){ holdTime = getVal dict 0 "holdTime" }

writeOpen :: BGPMessage -> IO ()
writeOpen = L.hPut stdout . wireFormat . encode

defaultOpen :: BGPMessage
defaultOpen = simpleOpen 65534 "0.0.0.0"

simpleOpen :: Word32 -> IPv4 -> BGPMessage
simpleOpen as routerID | as < 0x10000 = BGPOpen (fromIntegral as) 0 routerID [CapAS4 as]
                       | otherwise = BGPOpen 23456 0 routerID [CapAS4 as]
