{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BGPlib.AttoBGP (bgpParser, bgpParser1)
import BGPlib.BGPlib (BGPMessage (..))
import qualified Data.ByteString as ByteString
import Router.BGPPingParser
import System.IO

main = example1

example2 = do
  putStrLn "start of function"
  gen <- getGenerator getter parser
  let go = do
        next <- gen
        maybe
          (putStrLn "end of stream")
          ( \val -> do
              action val
              go
          )
          next
  go
  putStrLn "end of function"

example1 =
  let go bs = do
        next <- getNext getter parser bs
        maybe
          (return ())
          (\(bs', val) -> action val >> go bs')
          next
   in go ByteString.empty

getterNoisy = do
  putStrLn "enter getter for 64K"
  bs <- ByteString.hGetSome stdin _64K
  putStrLn $ "exit getter with " ++ show (ByteString.length bs) ++ " bytes"
  return bs

_64K = 65536 :: Int

_1M = 1024 * 1024 :: Int

_16M = 16 * _1M

getterQuiet = ByteString.hGetSome stdin _16M

getter = getterQuiet

-- type Val = BGPMessage
-- action = display
-- parser = bgpParser1

type Val = [BGPMessage]

action :: Val -> IO ()
action = mapM_ print

parser = bgpParser

display m = putChar $ case m of
  BGPUpdate {..} ->
    if  | ByteString.null attributes -> 'E'
        | null nlri -> 'A'
        | null withdrawn -> 'u'
        | otherwise -> 'U'
  BGPNotify {} -> 'N'
  BGPKeepalive -> 'K'
  BGPTimeout -> 'T'
  BGPEndOfStream -> 'X'
  (BGPError _) -> '?'
