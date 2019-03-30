{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO(stdout,stderr)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (hPut)
import Data.Binary(encode)

import BGPlib (wireFormat , fromAddrRange, _BGP_ORIGIN_IGP, PathAttribute(..),  ASSegment(..), ASPath(..) )
import BGPRib (encodeUpdates , makeUpdate , makeUpdate , encodeUpdates)

main :: IO ()
main = eor
    where
    eor = do
        let update = eorBGPUpdate
        hPut stderr "Building an EndOfRib Update"
        hPut stdout $ wireFormat $ encode $ head $ encodeUpdates update

    igp = do
        let update = iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"
        hPut stderr "Building a iBGP Update:"
        hPut stderr $ pack $ show update
        hPut stdout $ wireFormat $ encode $ head $ encodeUpdates  update
        --hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"

    iBGPUpdate = xBGPUpdate False

    eBGPUpdate = xBGPUpdate True

    xBGPUpdate isExternal aspath prefixes nextHop = makeUpdate
                       ( map fromAddrRange prefixes )
                       []
                       [ PathAttributeOrigin _BGP_ORIGIN_IGP
                       , PathAttributeASPath $ ASPath4 [ASSequence aspath]
                       , PathAttributeNextHop nextHop
                       , if isExternal then PathAttributeMultiExitDisc 0 else PathAttributeLocalPref 0
                       ]

    eorBGPUpdate = makeUpdate [] [] []
