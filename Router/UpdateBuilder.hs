{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO(stdout,stderr)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (hPut)
import Data.Binary(encode,Word32)
import Data.IP

import BGPlib.BGPlib (wireFormat , fromAddrRange, _BGP_ORIGIN_IGP, PathAttribute(..),  ASSegment(..), ASPath(..) )
import BGPRib.BGPRib (ParsedUpdate, encodeUpdates , makeUpdate , makeUpdate , encodeUpdates)
import Router.CreateUpdate

main :: IO ()
main = eor
    where
    eor = do
        let update = eorBGPUpdate
        hPut stderr "Building an EndOfRib Update\n"
        hPut stdout $ wireFormat $ encode $ head $ encodeUpdates update

    igp = do
        let update = iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"
        hPut stderr $ pack $ "Building a iBGP Update: " ++ show update ++ "\n"
        hPut stdout $ wireFormat $ encode $ head $ encodeUpdates update
        --hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"

    eorBGPUpdate = makeUpdate [] [] []
