module Main where

import BGPlib.AttoBGP
import BGPlib.BGPparse
import Control.Exception (evaluate)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.IORef
import Stopwatch
import System.Environment (getArgs)
import System.IO

data BGPHandle = BGPHandle Handle (IORef B.ByteString)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "?"
    else do
      let fname = head args
      putStrLn $ "\n*** " ++ fname ++ " ***\n"
      timeIO "parseCheck terminatingBGPParser" $ openFile fname ReadMode >>= bgpHandle >>= readMsgs

readMsgs :: BGPHandle -> IO ()
readMsgs h = do
  msgs <- collect []
  msgs' <- evaluate msgs
  putStrLn $ "read " ++ show (length msgs') ++ " messages"
  where
    collect l = do
      next <- getNext h
      if endOfStream next
        then return l
        else collect (next : l)
    endOfStream msg =
      case msg of
        BGPEndOfStream -> True
        (BGPError _) -> True
        BGPTimeout -> True
        _ -> False

bgpHandle :: Handle -> IO BGPHandle
bgpHandle h = do
  ior <- newIORef B.empty
  return $ BGPHandle h ior

getNext :: BGPHandle -> IO BGPMessage
getNext (BGPHandle stream ioref) = do
  oldBuf <- readIORef ioref
  newBuf <- if B.null oldBuf then getBuf else return oldBuf
  -- result <- complete ( parse terminatingBGPParser newBuf )
  result <- complete (parse bgpParser1 newBuf)
  case result of
    (Done i r) ->
      if B.null newBuf
        then do
          writeIORef ioref undefined
          return BGPEndOfStream
        else do
          writeIORef ioref i
          evaluate r
    (Partial _) -> error "Partial has been removed already by `g`"
    (Fail _ s sx) -> error $ "parse fail in getNext" ++ show (s, sx)
  where
    -- 'production' version in the event that parse fails actually occur
    -- (Fail _ s sx) -> do hPutStrln stderr $ "parse fail in getNext" ++ show (s,sx)
    --                     return $ BGPError $ show (s,sx)

    getBuf = B.hGetNonBlocking stream 4096

    complete :: Result BGPMessage -> IO (Result BGPMessage)
    complete (Partial cont) = do
      bs <- getBuf
      complete (cont bs)
    complete result = return result
