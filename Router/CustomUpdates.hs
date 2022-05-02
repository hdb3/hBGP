{-# LANGUAGE OverloadedStrings #-}


-- module CustomRib(ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribPush, RibHandle ) where
module Main where

import ArgConfig
import BGPRib hiding (addPeer, group, ribPush, update)
import BGPlib
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import System.IO (stderr, stdout)
import UpdateSource hiding (main)
import Prelude hiding (print, putStrLn)

putStrLn :: String -> IO ()
putStrLn = BS.hPutStrLn stderr . BS.pack

print :: Show a => a -> IO ()
print = putStrLn . show

data TestMode = OneShot | Continuous | Passive deriving (Read, Show, Eq)

main = do
  dict <- buildDictionary
  let testMode = getVal dict OneShot "testMode"
      startPrefix = getVal dict "172.16.0.0/30" "startPrefix"
      tableSize = getVal dict 100 "tableSize"
      groupSize = getVal dict 4 "groupSize"
      burstSize = getVal dict 10 "burstSize"
      burstDelay = getVal dict 0 "burstDelay"
      repeatDelay = getVal dict 0 "repeatDelay"
      idleDetect = fromRational $ getVal dict 5.0 "idleDetect"
      oneShotMode = testMode == OneShot
      peer = dummyPeerData {localIPv4 = getVal dict "192.168.122.1" "nextHop"}

  updateSource <- initSource peer startPrefix tableSize groupSize burstSize burstDelay oneShotMode repeatDelay

  go updateSource
  where
    go s = do
      updates <- s
      if null updates
        then return ()
        else do
          -- print updates
          let wireMessages = map (wireFormat . encode) updates
          mapM_ (L.hPut stdout) wireMessages
          go s

main' = do
  L.hPut stdout $ wireFormat $ encode $ head $ encodeUpdates $ iBGPUpdate [1, 2, 3] ["169.254.0.123/32"] "127.0.0.1"
  where
    iBGPUpdate = xBGPUpdate False
    eBGPUpdate = xBGPUpdate True
    xBGPUpdate isExternal aspath prefixes nextHop =
      makeUpdate
        (map fromAddrRange prefixes)
        []
        [ PathAttributeOrigin _BGP_ORIGIN_IGP,
          PathAttributeASPath $ ASPath4 [ASSequence aspath],
          PathAttributeNextHop nextHop,
          if isExternal then PathAttributeMultiExitDisc 0 else PathAttributeLocalPref 0
        ]
