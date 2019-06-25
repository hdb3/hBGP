{-# LANGUAGE OverloadedStrings #-}
module Main where
import BGPlib.AttoBGP
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import System.Environment(getArgs)
import Control.Exception(evaluate)
import Control.DeepSeq(force)
import System.IO
import Data.IORef

import Stopwatch

--main = return ()

parseCheck p stream ioref = do
    -- result <- evaluate $ force $ parseOnly p bs
    (status, msgs) <- collect p stream ioref
    putStrLn $ "read " ++ show (length msgs) ++ " messages"
    putStrLn $ "exit status: " ++ show status

main = do
    args <- getArgs
    if null args then putStrLn "?"
    else do
        putStrLn $ "\n*** " ++ (head args) ++ " ***\n"

        h <- openFile (head args) ReadMode -- (head args)
        ior <- newIORef B.empty :: IO (IORef B.ByteString)

        timeIO "parseCheck terminatingWireParser" $ parseCheck terminatingWireParser h ior
        --timeIO "parseCheck wireParser" $ parseCheck wireParser h ior

        h <- openFile (head args) ReadMode -- (head args)
        ior <- newIORef B.empty :: IO (IORef B.ByteString)

        timeIO "parseCheck terminatingBGPParser" $ parseCheck terminatingBGPParser h ior
        --timeIO "parseCheck bgpParser" $ parseCheck bgpParser h ior

{-
-}

getBuf' h = do buf <- B.hGetNonBlocking h 4096 -- 32768
               putStrLn $ show (B.length buf)
               return buf
getBuf h = B.hGetNonBlocking h 32768

collect :: Parser a -> Handle -> IORef B.ByteString -> IO (([String], String),[a])
collect p stream ioref = go []
    where
    go l = do
        next <- getNext p stream ioref
        either (\left -> return $ (left,l))
               (\right -> go (right:l))
               next

getNext :: Parser a -> Handle -> IORef B.ByteString -> IO ( Either ([String], String)  a)
getNext p stream ioref = do

    oldBuf <- readIORef ioref
    newBuf <- if B.null oldBuf then getBuf stream else return oldBuf
    let result = parse p newBuf
    result' <- g (getBuf stream) result
    case result' of
        (Done i r) -> do if B.null newBuf then writeIORef ioref undefined else writeIORef ioref i
                         return $ Right r
        (Partial cont) -> error "Partial has been removed already by `g`"
        (Fail _ s sx) -> return $ Left (s,sx)



g :: IO B.ByteString -> Result a -> IO (Result a)
g getStream (Partial cont) = do
    bs <- getStream
    let result = cont bs
    g getStream result

g _ result = return result
