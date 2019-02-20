{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Word
import Data.IP
import Data.Binary
import qualified Data.ByteString.Lazy as L

import Common
import Prefixes
import Numeric(showHex)
hex x = showHex x ""

readPfx :: String -> Prefix
readPfx = fromAddrRange.read

readIPfx :: String -> IPrefix
readIPfx = fromPrefix.fromAddrRange.read

iPrefixToInt (IPrefix i) = i

main' = do
-- test read and overloadedStrings
    let ipfx = "192.168.1.99/24" :: IPrefix
    print ipfx
    putStrLn $ hex $ iPrefixToInt ipfx 
    print $ toPrefix ipfx
    print $ toAddrRange $ toPrefix ipfx

main = do
    let prefixes = map fromAddrRange ranges
        iprefixes = map fromPrefix prefixes
        iprefixes' = map iPrefixToInt iprefixes
        prefixes' = map toPrefix iprefixes
        ranges'   = map toAddrRange prefixes'
    print ranges
    print prefixes
    print iprefixes
    print prefixes'
    print ranges'

ranges = 
        ["1.2.3.4/32"
        ,"1.2.3.4/24"
        ,"1.2.3.4/16"
        ,"1.2.3.4/8"
        , "0.0.0.0/0"
        , "192.168.1.99/24"
        , "129.129.0.0/16"
        , "172.16.0.77/12"
        , "169.254.108.17/32"
        , "10.1.2.3/8"
        ] :: [AddrRange IPv4]
