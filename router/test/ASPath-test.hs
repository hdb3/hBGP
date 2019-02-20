{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Binary
import qualified Data.ByteString.Lazy as L

import Common
import ASPath (asPrePend,ASPath42(..),ASPath32,ASPath16,decodeAS4,decodeAsASPath4,decodeAsASPath2,ASPath(..),ASSegment(..),as4list,as2list)

asSequence2 = ASSequence . as2list
asSet2 = ASSet . as2list
asSequence4 = ASSequence . as4list
asSet4 = ASSet . as4list

path0 = ASPath [] :: ASPath16
path1 = ASPath [seg0] :: ASPath16
path2 = ASPath [seg1] :: ASPath16
path3 = ASPath [seg1,seg2] :: ASPath16
seg0  = asSequence2 []
seg1  = asSet2 [1,2,3]
seg2  = asSequence2 [6,5,4]
pathAttribute3 = ASPath2 path3 :: ASPath42
path40 = ASPath [] :: ASPath32
path41 = ASPath [seg4a] :: ASPath32
path42 = ASPath [seg4b] :: ASPath32
path43 = ASPath [seg4c] :: ASPath32
path44 = ASPath [seg4b,seg4c] :: ASPath32
seg4a  = asSequence4 []
seg4b  = asSet4 [1,2,3]
seg4c  = asSequence4 [6,5,4]
pathAttribute44 = ASPath4 path44 :: ASPath42

main = do
    mapM_ test2 [path0,path1,path2,path3]
    test42 False pathAttribute3
    test42 True pathAttribute44
    mapM_ test4 [path40,path41,path42,path43,path44]
    prependTest

prependTest = do
    let path42' = asPrePend 42 path42
        path2'  = asPrePend 42 path2
        path43' = asPrePend 42 path43
        path3'  = asPrePend 42 path3
    print path42'
    print path2'
    print path43'
    print path3'

test42 :: Bool -> ASPath42 -> IO()
test42 isAS4 path = do
               putStrLn "test42"
               let enc = encode path
                   -- dec = decode enc :: ASPath2
                   dec = if isAS4 then decodeAsASPath4 enc else decodeAsASPath2 enc
                   dec' = decodeAS4 enc
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn $ "atto decoded: " ++ show dec'
               putStrLn " ------------------"

test2 :: ASPath16 -> IO()
test2 path = do
               putStrLn ""
               let enc = encode path
                   dec = decode enc :: ASPath16
                   dec' = decodeAS4 enc
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn $ "atto decoded: " ++ show dec'
               putStrLn " ------------------"

test4 :: ASPath32 -> IO()
test4 path = do
               putStrLn ""
               let enc = encode path
                   dec = decode enc :: ASPath32
                   dec' = decodeAS4 enc
               print path
               putStrLn ""
               putStrLn $ "encoded: " ++ simpleHex' enc
               putStrLn $ "decoded: " ++ show dec
               putStrLn $ "atto decoded: " ++ show dec'
               putStrLn " ------------------"
