{-# LANGUAGE OverloadedStrings #-}
module Main where
import BGPlib.AttoBGP
--import BGPlib.BGPlib
--import BGPRib.BGPRib
import qualified Data.ByteString as B
--import Data.ByteString.Lazy(toStrict,fromStrict)
import Data.Attoparsec.ByteString
--import Data.Binary
import System.Environment(getArgs)
import Control.Exception(evaluate)
import Control.DeepSeq(force)
import System.IO

import Stopwatch

parseCheck p bs = do
    result <- evaluate $ force $ parseOnly p bs
    either (\s -> putStrLn $ "parse failed: " ++ s)
           (\msgs -> putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes" )
           result

main = do
    args <- getArgs
    if null args then putStrLn "?"
    else do
        putStrLn $ "\n*** " ++ (head args) ++ " ***\n"
        h <- openFile ReadMode (head args)

        timeIO "parseCheck wireParser" $ parseCheck wireParser bs
        timeIO "parseCheck bgpParser" $ parseCheck bgpParser bs
