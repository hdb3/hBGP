{-# LANGUAGE OverloadedStrings #-}
module Main where
import BGPlib.AttoBGP
import BGPlib.BGPparse
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import System.Environment(getArgs)
import Control.Exception(evaluate)
import Control.DeepSeq(force,NFData)
import System.IO
import Data.IORef

import Stopwatch

data BGPHandle = BGPHandle Handle (IORef B.ByteString)

main = do
    args <- getArgs
    if null args then putStrLn "?"
    else do
        let fname = head args
        putStrLn $ "\n*** " ++ fname ++ " ***\n"
        timeIO "parseCheck terminatingWireParser" $ openFile fname ReadMode >>= bgpHandle >>= readMsgs

readMsgs h = do
    msgs <- collect []
    msgs' <- evaluate $ force msgs
    putStrLn $ "read " ++ show (length msgs') ++ " messages"

    where

    collect l = do
        next <- getNext h
        case next of
           BGPEndOfStream -> return l
           (BGPError _) -> return l
           BGPTimeout -> return l
           _          -> collect (next:l)
           
bgpHandle :: Handle -> IO BGPHandle
bgpHandle h = do
        ior <- newIORef B.empty
        return $ BGPHandle h ior

getNext :: BGPHandle -> IO BGPMessage
getNext (BGPHandle stream ioref) = do

    oldBuf <- readIORef ioref
    newBuf <- if B.null oldBuf then getBuf else return oldBuf
    result <- complete ( parse terminatingBGPParser newBuf )
    case result of
        (Done i r) -> do if B.null newBuf then do writeIORef ioref undefined
                                                  return BGPEndOfStream
                                          else do writeIORef ioref i
                                                  evaluate $ force r
        (Partial cont) -> error "Partial has been removed already by `g`"
        (Fail _ s sx) -> error $ "parse fail in getNext" ++ show (s,sx)
        -- 'production' version in the event that parse fails actually occur
        -- (Fail _ s sx) -> do hPutStrln stderr $ "parse fail in getNext" ++ show (s,sx)
        --                     return $ BGPError $ show (s,sx)

    where

    getBuf = B.hGetNonBlocking stream 4096

    complete :: Result BGPMessage -> IO (Result BGPMessage)
    complete (Partial cont) = do
        bs <- getBuf
        complete (cont bs)
    
    complete result = return result
