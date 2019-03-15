{-# LANGUAGE OverloadedStrings , RecordWildCards #-}
module CustomRib(ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribPush, RibHandle ) where
import System.Timeout(timeout)
import Data.Time.Clock
import Data.Maybe(fromMaybe)
import Data.Word
import Control.Concurrent
import Control.Monad(when)

import BGPlib
import BGPRib hiding ( ribPush, addPeer)
import UpdateSource
import Log
import ArgConfig

data TestMode = OneShot | Continuous | Passive deriving (Read,Show,Eq)
data CRib = CRib { msgCount :: Int, active :: Bool, firstUpdate,lastUpdate :: UTCTime }

data RibHandle = RibHandle {testMode :: TestMode, thread :: Int, mvCRib :: MVar CRib, peer :: PeerData, start :: UTCTime, updateSource :: UpdateSource, idleDetect :: NominalDiffTime}

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress _ port ip =
    trace $ "delPeerByAddress " ++ show ip ++ ":" ++ show port

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer _ peer = do
    trace $ "addPeer " ++ show peer
    start <- getCurrentTime
    tid <- myThreadId
    dict <- buildDictionary
    let
        testMode = getVal dict Passive "testMode"
        startPrefix = getVal dict "172.16.0.0/30" "startPrefix"
        tableSize = getVal dict 100 "tableSize"
        groupSize = getVal dict 4 "groupSize"
        burstSize = getVal dict 10 "burstSize"
        burstDelay = getVal dict 0 "burstDelay"
        repeatDelay = getVal dict 0 "repeatDelay"
        idleDetect = fromRational $ getVal dict 5.0 "idleDetect"
        oneShotMode = testMode == OneShot
        thread = read $ drop (length ( "ThreadId " :: String)) (show tid)

    updateSource <- if testMode == Passive then nullInitSource else initSource peer startPrefix tableSize groupSize burstSize burstDelay oneShotMode repeatDelay
    info $ show thread ++ " - customRib operating in mode: " ++ show testMode
    --updateSource <- initSourceDefault peer
    -- updateSource <- initSource peer "172.16.0.0/30" 1000000 4 1000 -- table size / group size / burst size / repeat count
    mvCRib <- newMVar $ CRib 0 False undefined undefined
    return RibHandle{..}

ribPush :: RibHandle -> BGPMessage -> IO Bool
ribPush RibHandle{..} BGPKeepalive = do
    cRib <- takeMVar mvCRib
    if active cRib then do
        now <- getCurrentTime
        let idleTime = diffUTCTime now (lastUpdate cRib)
        if idleTime < idleDetect then putMVar mvCRib cRib else do
            putMVar mvCRib ( cRib {active = False})
            report cRib
    else do
        trace "ribPush (keepalive)"
        putMVar mvCRib $ CRib 0 False undefined undefined
        --putMVar mvCRib cRib
    return True
    where
    report CRib{..} = do
        let deltaTime = init $ show $ diffUTCTime lastUpdate firstUpdate
        info $ show thread ++ " :report: " ++ show peer ++ " " ++ show msgCount ++ " " ++ deltaTime

ribPush RibHandle{..} update = do
    now <- getCurrentTime
    cRib <- takeMVar mvCRib
    let mc = msgCount cRib + 1
    if active cRib then
        putMVar mvCRib ( cRib {msgCount = mc, lastUpdate = now})
    else
        putMVar mvCRib ( cRib {msgCount = 1, active = True, firstUpdate=now, lastUpdate = now})

    let deltaTime = init $ show $ diffUTCTime now start
    trace $ show thread ++ " : " ++ deltaTime ++ " :push: " ++ " : " ++ show peer ++ ": (" ++ show mc ++ ") " ++ show update
    return True

ribPull :: RibHandle -> IO [BGPMessage]
ribPull RibHandle{..} =  do
    --now <- getCurrentTime
    --let deltaTime = diffUTCTime now start
    --trace $ deltaTime ++ " pull " ++ show peer
    updates <- updateSource
    when (null updates)
         (threadDelay $ 10^12)
    return updates
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
