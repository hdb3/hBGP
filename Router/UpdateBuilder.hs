{-# LANGUAGE OverloadedStrings #-}

module Main where

import BGPRib.BGPRib (ParsedUpdate, encodeUpdates, encodeUpdates, makeUpdate, makeUpdate)
import BGPlib.BGPlib (ASPath (..), ASSegment (..), PathAttribute (..), _BGP_ORIGIN_IGP, fromAddrRange, wireFormat)
import Data.ByteString.Lazy (hPut)
import Data.ByteString.Lazy.Char8 (pack)
import Data.IP
import Data.Word
import Router.CreateUpdate
import System.IO (stderr, stdout)

main :: IO ()
main = eor
  where
    eor = do
      let update = eorBGPUpdate
      hPut stderr "Building an EndOfRib Update\n"
      hPut stdout $ wireFormat $ encode $ head $ encodeUpdates update
    igp = do
      let update = iBGPUpdate [1, 2, 3] ["169.254.0.123/32"] "127.0.0.1"
      hPut stderr $ pack $ "Building a iBGP Update: " ++ show update ++ "\n"
      hPut stdout $ wireFormat $ encode $ head $ encodeUpdates update
    --hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"

    eorBGPUpdate = makeUpdate [] [] []
