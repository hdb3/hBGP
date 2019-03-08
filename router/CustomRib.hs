{-# LANGUAGE OverloadedStrings , RecordWildCards, TupleSections #-}
module CustomRib(ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribPush, RibHandle ) where
import System.Timeout(timeout)
import Data.Time.Clock
import Data.Maybe(fromMaybe)
import Data.Word
import Control.Concurrent

import BGPlib
import BGPRib hiding ( ribPush, addPeer)
import UpdateSource
import Log

data CRib = CRib { msgCount :: Int }

bumpMsgCount :: MVar CRib -> IO Int
bumpMsgCount mCRib = do
    cRib <- takeMVar mCRib
    let c = msgCount cRib
    putMVar mCRib ( cRib {msgCount = c + 1})
    return c

data RibHandle = RibHandle {cRib :: MVar CRib, peer :: PeerData, start :: UTCTime, updateSource :: UpdateSource}

showDeltaTime :: UTCTime -> IO String
showDeltaTime start = do
    now <- getCurrentTime
    let deltaTime = diffUTCTime now start
    tid <- myThreadId
    let thread = drop (length ( "ThreadId " :: String)) (show tid)
    return $ thread ++ " - " ++ init (show deltaTime)

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress _ port ip =
    trace $ "delPeerByAddress " ++ show ip ++ ":" ++ show port

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer _ peer = do
    trace $ "addPeer " ++ show peer
    updateSource <- initSource peer "172.16.0.0/30" 16 4 2 0 -- table size / group size / burst size / repeat count
    cRib <- newMVar $ CRib 0
    start <- getCurrentTime
    return RibHandle{..}

ribPush :: RibHandle -> ParsedUpdate -> IO()
ribPush RibHandle{..} update = do
    deltaTime <- showDeltaTime start
    count <- bumpMsgCount cRib
    trace $ deltaTime ++ " push " ++ " : " ++ show peer ++ ": (" ++ show count ++ ") " ++ show update

ribPull :: RibHandle -> IO [ParsedUpdate]
ribPull RibHandle{..} =  do
    deltaTime <- showDeltaTime start
    --trace $ deltaTime ++ " pull " ++ show peer
    threadDelay (100 * 1000)
    updateSource
    --threadDelay 10000000000
    --return []

msgTimeout :: Int -> IO [a] -> IO [a]
msgTimeout t f = fromMaybe [] <$> timeout (1000000 * t) f

addRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IPv4 -> IO()
addRouteRib _ peer prefix nextHop =
    trace $ "addRouteRib " ++ show peer ++ ":" ++ show prefix ++ ":" ++ show nextHop

delRouteRib :: Rib -> PeerData -> AddrRange IPv4 -> IO()
delRouteRib rib peer prefix =
    trace $ "delRouteRib " ++ show peer ++ ":" ++ show prefix

updateFromAdjRibEntrys :: Rib -> PeerData -> [AdjRIBEntry] -> IO [ParsedUpdate]
updateFromAdjRibEntrys _ target ares = do
    trace $ "updateFromAdjRibEntrys " ++ show target ++ ":" ++ show ares
    return []

routesFromAdjRibEntrys :: Rib -> [AdjRIBEntry] -> IO [(IPrefix,IPv4)]
routesFromAdjRibEntrys _ ares = do
    trace $ "routesFromAdjRibEntrys " ++ show ares
    return []
