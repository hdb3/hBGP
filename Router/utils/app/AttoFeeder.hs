{-# LANGUAGE OverloadedStrings #-}
module Main where
import BGPlib.AttoBGP
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import System.Environment(getArgs)
import Control.Exception(evaluate)
import Control.DeepSeq(force,NFData)
import System.IO
import Data.IORef

import Stopwatch

parseCheck eos p stream ioref = do
    (status, msgs) <- collect eos p stream ioref
    msgs' <- evaluate $ force msgs
    putStrLn $ "read " ++ show (length msgs') ++ " messages"
    putStrLn $ "exit status: " ++ show status

main = do
    args <- getArgs
    if null args then putStrLn "?"
    else do
        putStrLn $ "\n*** " ++ (head args) ++ " ***\n"

        h <- openFile (head args) ReadMode -- (head args)
        ior <- newIORef B.empty :: IO (IORef B.ByteString)

        timeIO "parseCheck terminatingWireParser" $ parseCheck eosWireParser terminatingWireParser h ior

        h <- openFile (head args) ReadMode -- (head args)
        ior <- newIORef B.empty :: IO (IORef B.ByteString)

        timeIO "parseCheck terminatingBGPParser" $ parseCheck eosBGPParser terminatingBGPParser h ior

--getBuf h = B.hGetNonBlocking h 1024
getBuf h = B.hGetNonBlocking h (1024*1024)

nullStatus :: [a] -> IO (([String], String),[a])
nullStatus ax = return (([],""),ax)

collect :: (Control.DeepSeq.NFData a) => (a -> Bool) -> Parser a -> Handle -> IORef B.ByteString -> IO (([String], String),[a])
collect eos p stream ioref = go []
    where
    go l = do
        next <- getNext p stream ioref
        either (\left -> return $ (left,l))
               (\right -> if eos right then nullStatus l else go (right:l))
               next

getNext :: (Control.DeepSeq.NFData a) => Parser a -> Handle -> IORef B.ByteString -> IO ( Either ([String], String)  a)
getNext p stream ioref = do

    oldBuf <- readIORef ioref
    newBuf <- if B.null oldBuf then getBuf stream else return oldBuf
    let result = parse p newBuf
    result' <- g (getBuf stream) result
    case result' of
        (Done i r) -> do if B.null newBuf then writeIORef ioref undefined else writeIORef ioref i
                         evaluate $ force $ Right r
                         --return $ Right r
        (Partial cont) -> error "Partial has been removed already by `g`"
        (Fail _ s sx) -> return $ Left (s,sx)

    where

    g :: IO B.ByteString -> Result a -> IO (Result a)
    g getStream (Partial cont) = do
        bs <- getStream
        let result = cont bs
        g getStream result
    
    g _ result = return result
