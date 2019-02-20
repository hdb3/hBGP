{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Word
import Data.IP
import Data.Binary
import qualified Data.ByteString.Lazy as L

import Common
import Prefixes

readPfx :: String -> Prefix
readPfx = fromAddrRange.read
decodePrefixes :: L.ByteString -> [Prefix]
decodePrefixes = decode
encodePrefixes :: [Prefix] -> L.ByteString
encodePrefixes = encode
decodePrefix :: L.ByteString -> Prefix
decodePrefix = decode
encodePrefix :: Prefix -> L.ByteString
encodePrefix = encode
p = do putStrLn "------------------" ; putStrLn ""

main = do
    -- testDecode
    -- testEncode
    testEncodes
    -- testDecodes

testDecode = 
    mapM_ (test1 . fromHex)
        ["00"
        ,"0801"
        ,"100102"
        ,"18010203"
        ,"1c01020340"
        ,"2001020304"
        ]
    where
    test1 :: L.ByteString -> IO ()
    test1 bs = do
        putStrLn $ toHex bs
        let pfx = decode bs :: Prefix
        print pfx
        putStrLn "------------------"
        putStrLn ""


encodeList = 
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

testEncodes = test1 encodeList where
-- testEncodes = test1 ["1.2.3.4/32"] where
    test1 ars = do
        let pfxs = map fromAddrRange ars
            enc = encodePrefixes pfxs
        putStrLn $ "encoded: " ++ simpleHex' enc
        let dec = decodePrefixes enc
        -- putStrLn $ "decoded: " ++ show dec
        if dec == pfxs then putStrLn "OK"
                       else do putStrLn "*** FAIL ***"
                               putStrLn $ "encoded: " ++ simpleHex' enc
                               putStrLn $ "decoded: " ++ show dec
 
        p

testEncode = do
    let test :: Prefix -> IO ()
        test = test3
    mapM_ (test . fromAddrRange) encodeList
    where
    test1 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let enc = encodePrefix pfx
        putStrLn $ "encoded: " ++ simpleHex' enc
        p
    
    test2 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let ar = toAddrRange pfx
        putStrLn $ "AddrRange': " ++ show ar
    
        let arMasked' = makeAddrRange (masked ip' (intToMask subnet')) subnet'
            (ip',subnet') = addrRangePair ar
        putStrLn $ "AddrRange (masked): " ++ show ar
    
        -- let pfxMasked = canonicalPrefix pfx
        -- putStrLn $ "prefix (masked): " ++ show pfxMasked
        p

    test3 pfx = do
        putStrLn $ "prefix: " ++ show pfx
        let enc = encodePrefix pfx
        let dec = decodePrefix enc
        putStrLn $ "encoded: " ++ simpleHex' enc
        putStrLn $ "decoded: " ++ show dec
        if dec == pfx then putStrLn "OK"
                      else do putStrLn "*** FAIL ***"
                              putStrLn $ "encoded: " ++ simpleHex' enc
                              putStrLn $ "decoded: " ++ show dec

        p
