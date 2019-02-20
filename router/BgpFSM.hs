{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM) where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(IOMode( ReadWriteMode ),Handle, hClose)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Control.Concurrent
import Control.Exception
import Control.Monad(when,unless)
import Data.Maybe(fromJust,isJust,fromMaybe)
import Data.Either(either)
import qualified Data.Map.Strict as Data.Map
import System.Posix.Temp(mkstemp)

import BGPRib
import BGPlib
import Open
import Collision
import Route
import Global
import Config

-- TODO - modify the putStrLn's to at least report the connected peer. but..
-- better: implement a logger

data FSMState = St { handle :: Handle
                   , sock :: Socket
                   , osm :: OpenStateMachine 
                   , peerConfig :: PeerConfig
                   , maybePD :: Maybe PeerData
                   , rcvdOpen :: MVar BGPMessage }

type F = FSMState -> IO (State, FSMState)

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
    deriving Show

instance Exception FSMException

bgpFSM :: Global -> ( Socket , SockAddr) -> IO ()
bgpFSM global@Global{..} ( sock , peerName ) =
                          do threadId <- myThreadId
                             putStrLn $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName

                             SockAddrInet _ remoteIP <- getPeerName sock
                             let maybePeer = maybe 
                                                 ( if configAllowDynamicPeers config then Just (fillConfig config (fromHostAddress remoteIP))
                                                   else Nothing)
                                                 Just
                                                 ( Data.Map.lookup (fromHostAddress remoteIP) peerMap )
                             fsmExitStatus <-
                                         catch
                                             (runFSM global sock maybePeer )
                                             (\(FSMException s) -> do
                                                 return $ Left s)
                             -- TDOD throuuigh testing around delPeer
                             -- TODO REAL SOON - FIX....
                             -- maybe (return()) (delPeer rib) maybePeer
                             peers <- getPeersInRib rib
                             let myPD = filter (\pd -> peerIPv4 pd == fromHostAddress remoteIP) peers
                             unless (null myPD) (do print myPD
                                                    delPeer rib (head myPD)) 
                             print myPD
                             close sock
                             deregister collisionDetector
                             either
                                 (\s -> putStrLn $ "BGPfsm exception exit" ++ s)
                                 (\s -> putStrLn $ "BGPfsm normal exit" ++ s)
                                 fsmExitStatus


initialiseOSM :: Global -> PeerConfig -> OpenStateMachine
initialiseOSM Global{..} PeerConfig{..} =
    makeOpenStateMachine ( BGPOpen { myAutonomousSystem = toAS2 $ myAS gd
                                   , holdTime = (configOfferedHoldTime config)
                                   , bgpID = myBGPid gd
                                   , caps = peerConfigOfferedCapabilities} )
                         ( BGPOpen { myAutonomousSystem = toAS2 $ fromMaybe 0 peerConfigAS
                                   , holdTime = 0
                                   , bgpID = fromMaybe (fromHostAddress 0) peerConfigBGPID
                                   , caps = peerConfigRequiredCapabilities} )

bgpSnd :: Handle -> BGPMessage -> IO()
bgpSnd h msg | 4079 > L.length (encode msg) = catchIOError ( sndRawMessage h (encode msg)) (\e -> throw $ FSMException (show (e :: IOError)))
             | otherwise = do (n,h) <- mkstemp "bgpSnd"
                              L.hPut h (encode msg)
                              hClose h
                              putStrLn $ "encoded message too long in bgpSnd, encoded message was written to: " ++ n 

get :: Handle -> Int -> IO BGPMessage
get b t = getRawMsg b t >>= return . decodeBGPByteString

runFSM :: Global -> Socket -> Maybe PeerConfig -> IO (Either String String)
runFSM g@Global{..} sock maybePeerConfig  = do
-- The 'Maybe PeerData' allows the FSM to handle unwanted connections, i.e. send BGP Notification
-- thereby absolving the caller from having and BGP protocol awareness
    handle <- socketToHandle sock ReadWriteMode
    maybe (do bgpSnd handle $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionRejected L.empty
              return  $ Left "connection rejected for unconfigured peer" )
          ( \peerConfig -> do ro <- newEmptyMVar
                              fsm (StateConnected, St{sock = sock
                                                     , handle = handle
                                                     , osm = initialiseOSM g peerConfig
                                                     , peerConfig = peerConfig
                                                     , maybePD = Nothing
                                                     , rcvdOpen = ro}))
          maybePeerConfig
    where

    fsm :: (State,FSMState) -> IO (Either String String)
    fsm (s,st) | s == Idle = return $ Right "FSM normal exit"
               | otherwise = do
        (s',st') <- f s st
        fsm (s',st') where
            f StateConnected = stateConnected
            f StateOpenSent = stateOpenSent
            f StateOpenConfirm = stateOpenConfirm
            f ToEstablished = toEstablished
            f Established = established

    idle s = do putStrLn $ "IDLE - reason: " ++ s
                return (Idle, undefined )

    stateConnected :: F
    stateConnected st@St{..} = do
        msg <- get handle delayOpenTimer
        case msg of 
            BGPTimeout -> do
                putStrLn "stateConnected - event: delay open expiry"
                bgpSnd handle (localOffer osm)
                putStrLn "stateConnected -> stateOpenSent"
                return (StateOpenSent,st )
            open@BGPOpen{} -> do
                let osm' = updateOpenStateMachine osm open
                    resp = getResponse osm'
                putStrLn "stateConnected - event: rcv open"
                collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
                if isJust collision then do
                    bgpSnd handle $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionCollisionResolution L.empty
                    idle (fromJust collision)
                else if isKeepalive resp then do 
                    putStrLn "stateConnected -> stateOpenConfirm"
                    bgpSnd handle (localOffer osm)
                    bgpSnd handle resp
                    return (StateOpenConfirm , st {osm=osm'} )
                else do
                    bgpSnd handle resp
                    idle "stateConnected - event: open rejected error"
            BGPNotify{} -> do
               -- TODO - improve Notify analysis and display
               idle "stateConnected -> exit rcv notify"
            BGPUpdate{} -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateConnected - recvd Update - FSM error"
            z -> do
                idle $ "stateConnected - network exception - " ++ show z

    stateOpenSent :: F
    stateOpenSent st@St{..} = do
        msg <- get handle initialHoldTimer
        case msg of 
          BGPTimeout -> do
              bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
              idle "stateOpenSent - error initial Hold Timer expiry"
          open@BGPOpen{} -> do
              let osm' = updateOpenStateMachine osm open
                  resp =  getResponse osm'
              putStrLn "stateOpenSent - rcv open"
              collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
              if isJust collision then do
                  bgpSnd handle $ BGPNotify NotificationCease 0 L.empty
                  idle (fromJust collision)
              else if isKeepalive resp then do 
                  bgpSnd handle resp
                  putStrLn "stateOpenSent -> stateOpenConfirm"
                  return (StateOpenConfirm,st{osm=osm'})
              else do
                    bgpSnd handle resp
                    idle "stateOpenSent - event: open rejected error"
          BGPNotify{} -> do
             idle "stateOpenSent - rcv notify"
          _ -> do
              bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
              idle "stateOpenSent - FSM error"

    stateOpenConfirm :: F
    stateOpenConfirm st@St{..} = do
        msg <- get handle (getNegotiatedHoldTime osm)
        case msg of 
            BGPTimeout -> do
                bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "stateOpenConfirm - error initial Hold Timer expiry"
            BGPKeepalive -> do
                putStrLn "stateOpenConfirm - rcv keepalive"
                return (ToEstablished,st)
            BGPNotify{} -> do
                idle "stateOpenConfirm - rcv notify"
            _ -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateOpenConfirm - FSM error"

    toEstablished :: F
    toEstablished st@St{..} = do
        putStrLn "transition -> established"
        putStrLn $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        -- only now can we create the peer data record becasue we have the remote AS/BGPID available and confirmed
        SockAddrInet _ localIP <- getSocketName sock
        peerName @(SockAddrInet _ remoteIP) <- getPeerName sock

        let globalData = gd
            peerAS = fromIntegral $ myAutonomousSystem $ fromJust $ remoteOffer osm
            peerBGPid = bgpID $ fromJust $ remoteOffer osm
            peerIPv4 = fromHostAddress remoteIP
            localIPv4 = fromHostAddress localIP
            localPref = 0 -- TODO - source this somewhere sensible - config?
            isExternal = peerAS /= myAS gd
            peerData = BGPRib.PeerData { .. }
        registerEstablished collisionDetector peerBGPid peerName
        -- VERY IMPORTANT TO USE THE NEW VALUE peerData' AS THIS IS THE ONLY ONE WHICH CONTAINS ACCURATE REMOTE IDENTITY FOR DYNAMIC PEERS!!!!
        -- it would be much better to remove the temptation to use conficured data by forcing a new type for relevant purposes, and dscarding the
        -- preconfigured values as soon as possible
        addPeer rib peerData
        forkIO $ sendLoop handle rib peerData  (getKeepAliveTimer osm)
        return (Established,st{maybePD=Just peerData})

    established :: F
    established st@St{..} = do
        let peerData = fromJust maybePD
        msg <- get handle (getNegotiatedHoldTime osm)
        case msg of 
            BGPKeepalive -> do
                return (Established,st)
            update@BGPUpdate{} ->
                maybe
                    ( do
                         bgpSnd handle $ BGPNotify NotificationUPDATEMessageError 0 L.empty
                         idle "established - Update parse error"
                    )
                    (\parsedUpdate -> do
                      BGPRib.ribUpdater rib peerData parsedUpdate
                      return (Established,st)
                    )
                    ( processUpdate update )

            BGPNotify{} -> do
                idle "established - rcv notify"
            BGPEndOfStream -> idle "established: BGPEndOfStream"
            BGPTimeout -> do
                bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "established - HoldTimerExpired error"
            _ -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "established - FSM error"

    -- collisionCheck
    -- manage cases where there is an established connection (always reject)
    -- and where another connection is in openSent state (use tiebreaker)
    -- and of couse where there is no other connection for this BGPID
    collisionCheck :: CollisionDetector -> IPv4 -> IPv4 -> IO (Maybe String)
    collisionCheck c self peer = do
        -- TODO - work out whether keeping the socket info is valuable, since we never use it
        --        for now fake it up since it is no longer in visibility
        let peerName = SockAddrInet 0 0
        rc <- raceCheck c peer peerName
        maybe
            (return Nothing)
            (\session -> return $
                       if sessionEstablished session then
                           Just $ "collisionCheck - event: collision with established session - open rejected error for peer " ++ show session
                       -- TODO - check this logic
                       -- does it consider whether we initiated the connection or not?
                       -- this requires to look at the port numbers
                       else if peer < self then
                           Just $ "collisionCheck - event: collision with tie-break - open rejected error for peer "  ++ show session
                       else
                           Nothing
                )
            rc

-- TODO rename 'send loop'
    sendLoop handle rib peer timer = do
        running <- catch
            ( do sendQueuedUpdates handle rib peer timer
                 return True
            )
            (\(FSMException _) -> do
                -- this is perfectly normal event when the fsm closes down as it doesn't stop the keepAliveLoop explicitly
                return False
            )
        when running
            ( sendLoop handle rib peer timer )

    sendQueuedUpdates handle rib peer timeout = do
        updates <- pullAllUpdates (1000000 * timeout) peer rib
        if null updates then
            bgpSnd handle BGPKeepalive
        else do routes <- lookupRoutes rib peer updates
                -- putStr $ "Ready to send routes to " ++ show (peerIPv4 peer)
                if 11 > length updates then do
                    putStr $ "sending routes to " ++ show (peerIPv4 peer)
                    print $ map fst updates
                else do
                    putStr $ "sending routes to " ++ show (peerIPv4 peer)
                    print $ map fst (take 10 updates)
                    putStrLn $ "and " ++ show (length updates - 10) ++ " more"
                mapM_ (bgpSnd handle) routes
