{-# LANGUAGE OverloadedStrings #-}
module App1 where

import qualified Data.ByteString as BS
import qualified System.Timeout
import qualified System.IO
import qualified System.Posix.Types
import Control.Concurrent(threadDelay)
import Data.Word
import Data.Maybe(isNothing)
import Poll

app :: Int -> Int -> Int -> Int -> System.IO.Handle -> System.Posix.Types.Fd -> IO ()
--app :: System.IO.Handle -> System.Posix.Types.Fd -> Int -> Int -> Int -> Int -> IO ()
app bs t0 t1 t2 h fd = do

    putStrLn $ "loop start, tpoll=" ++ show t0 ++ " tcheck=" ++ show t1 ++ " tblock=" ++ show t2 ++ " block size=" ++ show bs
    phase1Count <- phase1 opPut opChk t0 0
    --ql <- opPeek
    --putStrLn $ "Phase 1 : count = " ++ show phase1Count ++ " queue length " ++ show ql
    phase1QLength <- phase1a opPeek t1
    phase2Count <- phase2 opPut t2 phase1Count
    phase2QLength <- phase1a opPeek t1
    putStrLn $ "loop exit"
    putStrLn $ "Phase 1 : count = " ++ show phase1Count ++ " queue length " ++ show phase1QLength
    putStrLn $ "Phase 2 : count = " ++ show phase2Count ++ " queue length " ++ show phase2QLength
    putStrLn $ "Implied remote queue depth: " ++ show (( bs*phase1Count) - fromIntegral phase1QLength)
    putStrLn $ "Implied local queue depth: " ++ show ( phase2QLength - phase1QLength ) ++ " or about " ++ show (bs*(phase2Count-phase1Count))

    where

    opPut = BS.hPut h ( BS.replicate bs 0 )
    opPeek = fdUnsent fd
    opChk = (0 ==) <$> opPeek

    phase1 :: IO() -> IO Bool -> Int -> Int -> IO Int
    phase1 opPut opChk delay count = do
        opPut
        threadDelay delay
        opChk >>= \ p -> if p then phase1 opPut opChk delay (count+1) else return $ count+1

    phase1a :: IO Word32 -> Int -> IO Word32
    phase1a opPeek delay = do
        v0 <- opPeek
        threadDelay delay
        v1 <- opPeek
        --if (v0 == v1 ) then return v0 else return undefined
        if v0 == v1 then return v0 else do print (v0,v1) ; return undefined

    phase2 :: IO() -> Int -> Int -> IO Int
    phase2 opPut delay count = do
        v <- System.Timeout.timeout delay opPut
        if isNothing v then return count else phase2 opPut delay (count+1)
