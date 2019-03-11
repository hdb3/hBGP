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

data CRib = CRib { msgCount :: Int, active :: Bool, lastUpdate :: UTCTime }

bumpMsgCount :: MVar CRib -> IO Int
bumpMsgCount mCRib = do
    now <- getCurrentTime
    cRib <- takeMVar mCRib
    let c = msgCount cRib
        lastUpdate = now
        active = True
    putMVar mCRib ( cRib {msgCount = c + 1})
    return c

data RibHandle = RibHandle {thread :: Int, mvCRib :: MVar CRib, peer :: PeerData, start :: UTCTime, updateSource :: UpdateSource}

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
    updateSource <- initSourceDefault peer
    -- updateSource <- initSource peer "172.16.0.0/30" 1000000 4 1000 -- table size / group size / burst size / repeat count
    start <- getCurrentTime
    tid <- myThreadId
    let thread = read $ drop (length ( "ThreadId " :: String)) (show tid)
    mvCRib <- newMVar $ CRib 0 False start
    return RibHandle{..}

ribPush :: RibHandle -> ParsedUpdate -> IO()
--ribPush RibHandle{..} NullUpdate = return ()
ribPush RibHandle{..} NullUpdate = do
    cRib <- takeMVar mvCRib
    if active cRib then do
        putMVar mvCRib ( cRib {active = False})
        report cRib
    else
        putMVar mvCRib cRib
    where
    report CRib{..} = do
        let deltaTime = diffUTCTime lastUpdate start
        putStrLn $ show thread ++ " : " ++ show peer ++ " " ++ show msgCount ++ " " ++ show deltaTime

ribPush RibHandle{..} update = do
    deltaTime <- showDeltaTime start
    count <- bumpMsgCount mvCRib
    trace $ deltaTime ++ " push " ++ " : " ++ show peer ++ ": (" ++ show count ++ ") " ++ show update

ribPull :: RibHandle -> IO [ParsedUpdate]
ribPull RibHandle{..} =  do
    deltaTime <- showDeltaTime start
    --trace $ deltaTime ++ " pull " ++ show peer
    updates <- updateSource
    threadDelay (100 * 1000)
    return updates
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
