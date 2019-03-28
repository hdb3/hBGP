{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(IOMode( ReadWriteMode ),Handle, hClose,stdout)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Data.Binary(encode)
import Data.IP

import BGPlib
import BGPRib hiding ( group,update )

main = do
    L.hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ eorBGPUpdate
    --L.hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ iBGPUpdate [1,2,3] ["169.254.0.123/32"] "127.0.0.1"
    hClose stdout

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
