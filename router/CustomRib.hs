{-# LANGUAGE RecordWildCards, TupleSections #-}
module CustomRib(buildUpdates,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribUpdater ) where
import System.Timeout(timeout)
import Data.Maybe(fromMaybe)
import Data.Word
import Control.Concurrent(threadDelay)

import BGPlib
import BGPRib hiding ( ribUpdater, addPeer)
import Log

delPeerByAddress :: Rib -> Word16 -> IPv4 -> IO ()
delPeerByAddress _ port ip =
    trace $ "delPeerByAddress " ++ show ip ++ ":" ++ show port

addPeer :: Rib -> PeerData -> IO ()
addPeer _ peer =
    trace $ "addPeer " ++ show peer

ribUpdater :: Rib -> PeerData -> ParsedUpdate -> IO()
ribUpdater _ peer update =
    trace $ "ribUpdater " ++ show peer ++ ":" ++ show update

buildUpdates :: Rib -> PeerData -> IO [ParsedUpdate]
buildUpdates _ peer =  do
    trace $ "buildUpdates " ++ show peer
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
