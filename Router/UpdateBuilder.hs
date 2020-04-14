{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPRib.Update (deparseBGPOutputs, endOfRib, originateUpdate)
import BGPlib.BGPlib (ASPath (..), ASSegment (..), PathAttribute (..), _BGP_ORIGIN_IGP, fromAddrRange)
import Data.ByteString (hPut)
import Data.ByteString.Char8 (pack)
import Data.IP
-- import Data.Word
import System.IO (stderr, stdout)

main :: IO ()
main = update
  where
    eor = do
      hPut stderr "Building an EndOfRib Update\n"
      hPut stdout $ deparseBGPOutputs [endOfRib]
    update = do
      let msg = originateUpdate _BGP_ORIGIN_IGP [1, 2, 3] "127.0.0.1" ["169.254.0.123/32"]
      hPut stderr $ pack $ "Building a iBGP Update: " ++ show msg ++ "\n"
      hPut stdout $ deparseBGPOutputs [msg]
