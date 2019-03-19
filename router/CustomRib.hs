{-# LANGUAGE OverloadedStrings , RecordWildCards #-}
module CustomRib(ribPull,msgTimeout,addRouteRib,delRouteRib,updateFromAdjRibEntrys,routesFromAdjRibEntrys,delPeerByAddress, addPeer, ribPush, RibHandle ) where
import System.Timeout(timeout)
import Data.Time.Clock
import Data.Time.Calendar(Day(ModifiedJulianDay))
import Data.Maybe(fromMaybe)
import Data.Word
import Control.Concurrent
import Control.Monad(when)

import BGPlib
import BGPRib hiding ( ribPush, addPeer)
import UpdateSource
import Log
import ArgConfig

zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) 0.0

data TestMode = OneShot | Continuous | Passive deriving (Read,Show,Eq)
data CRib = CRib { msgCount :: Int
                 , pullActive
                 , active :: Bool
                 , firstPull
                 , lastPull
                 , firstUpdate
                 , lastUpdate :: UTCTime }

testAndClearPullActive :: RibHandle -> IO Bool
testAndClearPullActive rh = modifyMVar (mvCRib rh) (\crib -> return (crib{pullActive=False},pullActive crib))

setPullActive :: RibHandle -> IO ()
setPullActive rh = modifyMVar_ (mvCRib rh) (\crib -> return crib{pullActive=True})

getDeltaFirstPull :: RibHandle -> IO NominalDiffTime
getDeltaFirstPull rh = modifyMVar (mvCRib rh) (\crib -> if zeroDay == firstPull crib
                                                    then getCurrentTime >>= \ t -> return (crib{firstPull = t},fromRational 0.0)
                                                    else getCurrentTime >>= \ t -> return (crib,diffUTCTime t (firstPull crib))
                                          )

getFirstPull :: RibHandle -> IO UTCTime
getFirstPull = gfp

setCRibTime :: (CRib -> UTCTime -> CRib) -> RibHandle -> IO ()
setCRibTime updater rh = modifyMVar_ (mvCRib rh) ( \ crib -> updater crib <$> getCurrentTime )

getCRibTime :: (CRib -> UTCTime) -> RibHandle -> IO UTCTime
getCRibTime getter rh = getter <$> readMVar (mvCRib rh)

setLastPull :: RibHandle -> IO ()
setLastPull rh = modifyMVar_ (mvCRib rh) setLastPullCRib
    where
    setLastPullCRib :: CRib -> IO CRib
    setLastPullCRib crib = do
        now <- getCurrentTime
        return crib {lastPull=now}

slp = setCRibTime (\rh a -> rh {lastPull=a})
sfu = setCRibTime (\rh a -> rh {firstUpdate=a})
slu = setCRibTime (\rh a -> rh {lastUpdate=a})
gfp = getCRibTime firstPull
glp = getCRibTime lastPull
gfu = getCRibTime firstUpdate
glu = getCRibTime lastUpdate

getLastPull :: RibHandle -> IO UTCTime
getLastPull = getCRibTime lastPull

data RibHandle = RibHandle {testMode :: TestMode
                           , thread :: Int
                           , mvCRib :: MVar CRib
                           , peer :: PeerData
                           , start :: UTCTime
                           , updateSource :: UpdateSource
                           , idleDetect :: NominalDiffTime}

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
    mvCRib <- newMVar $ CRib { msgCount = 0
                             , pullActive = False
                             , active = False
                             , firstPull = zeroDay
                             , lastPull = start
                             , firstUpdate = undefined
                             , lastUpdate = undefined  }

    return RibHandle{..}

ribPush :: RibHandle -> BGPMessage -> IO Bool
ribPush RibHandle{..} BGPKeepalive = do
    cRib <- takeMVar mvCRib
    if active cRib then do
        now <- getCurrentTime
        let idleTime = diffUTCTime now (lastUpdate cRib)
        if idleTime < idleDetect then do
            --putStrLn "((lastUpdate cRib),now,idleTime,idleDetect)"
            --print ((lastUpdate cRib),now,idleTime,idleDetect)
            putMVar mvCRib cRib
        else do
        --if idleTime < idleDetect then return () else do
            putMVar mvCRib ( cRib {active = False})
            report cRib
    else do
        trace "ribPush (keepalive)"
        putMVar mvCRib $ cRib {msgCount = 0, active = False}
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
--ribPull RibHandle{..} =  do
ribPull rh =  do
    pullActive <- testAndClearPullActive rh
    deltaFirstPull <- show <$> getDeltaFirstPull rh
    trace $ "ribPull: pullActive=" ++ show pullActive
    -- this is quite chatty if the bursts are small
    when False -- pullActive
         ( do t0 <- glp rh
              tfp <- gfp rh
              now <- getCurrentTime
              let deltaTime = show ( diffUTCTime now t0 )
              info $ deltaFirstPull ++ " (" ++ deltaTime ++ ") pull " ++ show ( peer rh)
         )
    updates <- updateSource rh
    if null updates then do
        when pullActive ( do info $ "ribPull: update stream ended"
                             info $ deltaFirstPull ++ " pull " ++ show ( peer rh)
                        )
        threadDelay $ 10^12 -- should just return if the stream REALYY is empty forever.....
    else do
        slp rh
        setPullActive rh
        trace $ "ribPull: stream active"
    return updates

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
