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

main = return ()

{-
parseCheck p stream ioref = do
    -- result <- evaluate $ force $ parseOnly p bs
    Left (status, msgs) <- collect p stream ioref
    putStrLn $ "read " ++ show (length msgs) ++ " messages"
    putStrLn $ "exit status: " ++ show status

main = do
    args <- getArgs
    if null args then putStrLn "?"
    else do
        putStrLn $ "\n*** " ++ (head args) ++ " ***\n"
        h <- openFile (head args) ReadMode -- (head args)
        ior <- newIORef B.empty :: IO (IORef B.ByteString)

        timeIO "parseCheck wireParser" $ parseCheck wireParser h ior
        timeIO "parseCheck bgpParser" $ parseCheck bgpParser h ior

-}

getBuf h = B.hGetNonBlocking h 4096 -- 32768

--collect :: Parser a -> Handle -> IORef B.ByteString -> Either [a]
collect p stream ioref = go []
    where
    go :: [a] -> IO (([String], String),[a])
    go l = either (\left -> return $ (left,l))
                  (\right -> go (right:l))
                  <$> f
    f :: IO ( Either ([String], String) a)
    f = do
        oldBuf <- readIORef ioref
        newBuf <- if B.null oldBuf then getBuf stream else return oldBuf
        return $ parse p newBuf >>= g
        where
        g :: Result a -> IO ( Either ([String], String) a)
        g (Done i r) = do
            writeIORef ioref i
            return $ Right r
        --g (Partial cont) = getBuf stream >>= cont >>= g
        g (Fail _ s sx) = return $ Left (s,sx)

