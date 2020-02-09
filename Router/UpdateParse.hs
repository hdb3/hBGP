{-#LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import Data.IP
import Control.Monad(unless)
import Control.Concurrent
import Text.Read
import Data.Char(toLower)

main :: IO ()
main = console [] [] "0.0.0.0"

console ::  [Int] -> [AddrRange IPv4] ->  IPv4 -> IO ()
console path pfxs nh = do
    prompt
    input <- getLine
    let (command:px) = words input ++ repeat ""
    case map toLower command of

        "s" -> do putStrLn $ "Route: " ++ show path ++ " : " ++ show pfxs ++ " : " ++ show nh
                  console path pfxs nh

        "u" -> do putStrLn $ "Sending update: " ++ show path ++ " : " ++ show pfxs ++ " : " ++ show nh
                  console path pfxs nh

        "w" -> do putStrLn $ "Sending withdraw: " ++ show pfxs
                  console path pfxs nh

        "h" -> maybe (putStrLn "couldn't parse nexthop")
                     (\p -> do putStrLn $ "Nexthop: " ++ show p
                               console path pfxs p
                     )
                     (parseAddress $ px !! 0)

        "p" -> maybe (putStrLn "couldn't parse a path")
                     (\p -> do putStrLn $ "Path: " ++ show p
                               console p pfxs nh
                     )
                     (parsePath $ px !! 0)
        
        "n" -> maybe (putStrLn "couldn't parse prefixes")
                     (\p -> do putStrLn $ "NLRI: " ++ show p
                               console path p nh
                     )
                     (parsePrefixes $ px !! 0)

        "q" -> putStrLn "goodbye"

        _   -> do putStrLn "couldn't parse a command - try Show / Path / nH / Nlri / Update / Withdraw / Quit"
                  console path pfxs nh

prompt = putStr ">>> " >> hFlush stdout

parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)
parsePrefixes s = readMaybe s :: Maybe [AddrRange IPv4]

parseAddress s  = readMaybe s :: Maybe IPv4
parsePath s  = readMaybe s :: Maybe [Int]