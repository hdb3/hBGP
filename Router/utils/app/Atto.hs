{-# LANGUAGE OverloadedStrings #-}
module Main where
import BGPlib.AttoBGP
import BGPlib.BGPlib
import BGPRib.BGPRib
import qualified Data.ByteString as B
import Data.ByteString.Lazy(toStrict,fromStrict)
import Data.Attoparsec.ByteString
import Data.Binary
import System.Environment(getArgs)
import Stopwatch

main' = do
    bs <- B.getContents
    parseCheck wireParser bs

parseCheck p bs = do
    either (\s -> putStrLn $ "parse failed: " ++ s)
           (\msgs -> putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes" )
           ( parseOnly p bs )

parse_ p bs = either fail
                     id
                     ( parseOnly p bs )

keepAlive = toStrict $ wireFormat $ encode $ BGPKeepalive
mkUpdate a b c = toStrict $ wireFormat $ encode $ ungetUpdate $ makeUpdateSimple a b c
update1 = mkUpdate [] ["192.168.0.0/16"] []
update2 = mkUpdate [] ["192.168.0.0/16"] ["192.168.1.0/24"]
update3 = mkUpdate [] [] []
update4 = mkUpdate [PathAttributeOrigin 0] [] []
update5 = mkUpdate [PathAttributeNextHop "127.0.0.1"] [] []
update6 = mkUpdate [PathAttributeOrigin 0, PathAttributeNextHop "127.0.0.1"] [] []
update7 = mkUpdate [PathAttributeOrigin 0, PathAttributeNextHop "127.0.0.1"] ["192.168.0.0/16"] ["192.168.0.0/24","192.168.1.0/24","192.168.2.0/24"]
eor = mkUpdate [] [] []
eors = B.append eor eor
msg = B.concat [ keepAlive, eor, keepAlive ]

main = do
    args <- getArgs
    if null args then do
        test "custom" $ mkUpdate [] [] [ "192.168.0.0/16"
                                     --,"0.0.0.0/0"
                                     --,"255.255.255.255/1"
                                     --,"255.255.255.255/8"
                                     --,"255.0.255.255/9"
                                     --,"255.0.255.255/16"
                                     --,"255.0.0.255/17"
                                     --,"255.0.0.255/23"
                                     --,"255.255.0.255/24"
                                     --,"255.255.0.255/31"
                                     --,"0.255.0.255/32"
                                     --,"0.0.0.0/33"
                                     ]
        --test "eor" eor
        --test "msg" msg
        --test "update1" update1
        --test "update2" update2
        --test "update3" update3
        --test "update4" update4
        --test "update5" update4
        --test "update7" update4
        --test "update7" update4
    else if 1 == length args then do
        putStrLn $ "\n*** " ++ (head args) ++ " ***\n"
        bs <- B.readFile (head args)
        timeIO "parseCheck wireParser" $ parseCheck wireParser bs
        timeIO "parseCheck bgpParser" $ parseCheck bgpParser bs
    else do
        let n = read (args !! 1) :: Int
        putStrLn $ "\n*** " ++ (head args) ++ " " ++ show n ++ " ***\n"
        bs <- B.readFile (head args)
        let msgs = parse_ wireParser bs
            msg = msgs !! n
            wireMsg = toStrict $ wireFormat $ fromStrict msg
        putStrLn $ toHex wireMsg
        parseCheck wireParser wireMsg
        parseCheck bgpParser wireMsg
        parseTest bgpParser wireMsg
        print $ parse_ bgpParser wireMsg

    where test s bs = do
              putStrLn $ "\n*** " ++ s ++ " ***\n"
              putStrLn $ toHex bs
              parseCheck wireParser bs
              parseCheck bgpParser bs
              --timeIO "parseCheck wireParser" $ parseCheck wireParser bs
              --timeIO "parseCheck bgpParser" $ parseCheck bgpParser bs
