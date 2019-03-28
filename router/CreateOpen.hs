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

main = do
    L.hPut stdout $ wireFormat $ encode $ BGPOpen { myAutonomousSystem = 65000
                     , holdTime = 180
                     , bgpID = "192.168.122.122"
                     , caps = [CapAS4 65000]}
    hClose stdout
