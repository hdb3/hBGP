{-# LANGUAGE RecordWildCards, TupleSections #-}
module CustomRib(buildUpdates,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribUpdater, RibHandle ) where
import System.Timeout(timeout)
import Data.Time.Clock
import Data.Maybe(fromMaybe)
import Data.Word
import Control.Concurrent(threadDelay,MVar,newMVar,myThreadId)

import BGPlib
import BGPRib hiding ( ribUpdater, addPeer)
import Log

data CRib = CRib { msgCount :: Int }

data RibHandle = RibHandle {cRib :: MVar CRib, peer :: PeerData, start :: UTCTime}

showDeltaTime :: UTCTime -> IO String
showDeltaTime start = do
    now <- getCurrentTime
    let deltaTime = diffUTCTime now start
    tid <- myThreadId
    let thread = drop (length "ThreadId ") (show tid)
    return $ thread ++ " - " ++ init (show deltaTime)

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress _ port ip =
    trace $ "delPeerByAddress " ++ show ip ++ ":" ++ show port

addPeer :: Rib -> PeerData -> IO RibHandle
addPeer _ peer = do
    trace $ "addPeer " ++ show peer
    cRib <- newMVar $ CRib 0
    start <- getCurrentTime
    return RibHandle{..}

ribUpdater :: RibHandle -> ParsedUpdate -> IO()
ribUpdater RibHandle{..} update = do
    deltaTime <- showDeltaTime start
    trace $ deltaTime ++ " ribUpdater " ++ " : " ++ show peer ++ ":" ++ show update

buildUpdates :: RibHandle -> IO [ParsedUpdate]
buildUpdates RibHandle{..} =  do
    deltaTime <- showDeltaTime start
    trace $ deltaTime ++ " buildUpdates " ++ show peer
    threadDelay 10000000000
    return []

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
