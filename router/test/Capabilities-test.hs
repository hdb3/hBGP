{-# LANGUAGE MultiWayIf #-}
module Main where
import Data.Binary
import Data.ByteString.Lazy(toStrict)

import Capabilities
import Common

capmp = CapMultiprotocol 0 0
capAS4 = CapAS4 0x00010f0f
capGrR = CapGracefulRestart False 0
testList = [
             ("empty list",[]),
             ("singleton list",[capGrR]),
             ("long list",[capmp, capAS4, capGrR])
           ] :: [(String,[Capability])]

main = do
    putStrLn "capability test"
    runTests "testEncodings" testEncodings testList
    putStrLn ""
    runTests "testOptionalParameters" testOptionalParameters testList

testEncodings :: (String,[Capability]) -> IO ()
testEncodings (d,ps) = do
    putStrLn "testEncodings"
    putStrLn $ d ++ ":" ++ show ps
    let t p = do
        let encodedCapability = encode p
            decodedCapability = decode encodedCapability
        putStrLn $ "encoded: " ++ simpleHex (toStrict encodedCapability)
        putStrLn $ "decoded: " ++ show decodedCapability
        if decodedCapability == p then
            putStrLn "conversion OK" else
            putStrLn "*** conversion FAIL!!!"
        putStrLn ""
    mapM_ t ps
    putStrLn "done"

runTests desc f tests = do
    putStrLn desc
    mapM_  f tests
    putStrLn "done"

testOptionalParameters (d,ps) = do
        let params = buildOptionalParameters ps
            recodedParams = parseOptionalParameters params
        putStrLn $ d ++ ":" ++ show ps
        putStrLn $ "encoded: " ++ simpleHex params
        putStrLn $ "decoded: " ++ show recodedParams
        if recodedParams == ps then
            putStrLn "conversion OK" else
            putStrLn "*** conversion FAIL!!!"
        putStrLn ""
