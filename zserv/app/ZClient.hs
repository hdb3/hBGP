{-#LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import System.IO
import Data.IP
import Control.Monad(unless)
import Control.Concurrent
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

    putStrLn "connected"
    zservRegister outputStream _ZEBRA_ROUTE_BGP
    zservRequestRouterId outputStream
    zservRequestInterface outputStream
    forkIO (loop inputStream)
    console outputStream
    where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              print zMsg
                              loop stream )
              msg

console outputStream = do
    prompt
    input <- getLine
    let (command,p1,p2) = parseInput input
    case command of
        'a' -> maybe (putStrLn "couldn't parse a route")
                     (\prefix -> maybe ( putStrLn "couldn't parse a next-hop")
                                      ( \nextHop  -> do putStrLn $ "add " ++ show prefix ++ " via " ++ show nextHop
                                                        addRoute outputStream prefix nextHop )
                                      (parseAddress p2))
                     (parsePrefix p1)

        'd' -> maybe (putStrLn "couldn't parse a route")
                     (\prefix -> do putStrLn $ "del " ++ show prefix
                                    delRoute outputStream prefix )
                     (parsePrefix p1)

        'q' -> putStrLn "goodbye"
        'z' -> putStrLn "say hello, wave goodbye"
        _ -> putStrLn "couldn't parse a command"

    unless (command == 'q') (console outputStream)

    where prompt = putStr ">>> " >> hFlush stdout
          parseInput = parseInput' . words
          parseInput' wx | null wx = (' ',"","")
                         | otherwise  = (head (wx !! 0) , wx !! 1, wx !! 2)
          parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)
          parseAddress s  = readMaybe s :: Maybe IPv4


