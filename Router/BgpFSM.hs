{-# LANGUAGE RecordWildCards #-}
module Router.BgpFSM(bgpFSM) where
import Network.Socket
import System.IO.Error(catchIOError)
import qualified Data.ByteString.Lazy as L
import Data.IP
import Control.Concurrent
import Control.Exception
import Data.Maybe(fromJust,isJust,fromMaybe)
import Data.Either(either)
import qualified Data.Map.Strict as Data.Map
import Control.Applicative ((<|>))
import Control.Monad(void)

import BGPlib.BGPlib

import BGPRib.BGPRib(ParsedUpdate(NullUpdate),parseUpdate,PeerData(..),myBGPid,myAS)
-- TODO = move Update.hs, and ppssibly some or all of BGPData, from bgprib to bgplib, so that bgprib does not need to be imported here.....
--        the needed thing in BGPData is PeerData, but what else should move to is less obvious
--        there are some things which any BGP application needs.....

--import qualified Router.CustomRib as Rib
import qualified Router.StdRib as Rib

import Router.Open
import Router.Collision
import Router.Global
import Router.Config
import Router.Log
import Router.BGPHandle

data FSMState = St { handle :: BGPHandle
                   , peerName :: SockAddr
                   , socketName :: SockAddr
                   , osm :: OpenStateMachine
                   , peerConfig :: PeerConfig
                   , maybePD :: Maybe PeerData
                   , rcvdOpen :: MVar BGPMessage
                   , ribHandle :: Maybe Rib.RibHandle }

type F = FSMState -> IO (State, FSMState)

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

bgpFSM :: Global -> ( Socket , SockAddr) -> IO ()
bgpFSM global@Global{..} ( sock , peerName ) =
                          do threadId <- myThreadId
                             trace $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName

                             socketName <- getSocketName sock
                             let (SockAddrInet remotePort remoteIP) = peerName
                                 (SockAddrInet _ localIP)   = socketName
                             handle <- getBGPHandle sock

                             -- lookup explicit local IP then failover to widlcard adn eventually, if allowed, a dynamic peer
                             let maybePeer =  Data.Map.lookup (fromHostAddress localIP , fromHostAddress remoteIP) peerMap  <|>
                                              Data.Map.lookup (fromHostAddress 0 , fromHostAddress remoteIP) peerMap  <|>
                                              if configAllowDynamicPeers config
                                               then Just (fillConfig config (fromHostAddress remoteIP))
                                               else Nothing
                             fsmExitStatus <-
                                         catch
                                             (runFSM global socketName peerName handle maybePeer )
                                             (\(BGPIOException s) ->
                                                 return $ Left s)
                             -- TDOD throuuigh testing around delPeer
                             -- TODO REAL SOON - FIX....
                             bgpClose handle
                             deregister collisionDetector
                             Rib.delPeerByAddress rib (fromIntegral remotePort) (fromHostAddress remoteIP)
                             either
                                 (\s -> warn $ "BGPfsm exception exit" ++ s)
                                 (\s -> trace $ "BGPfsm normal exit" ++ s)
                                 fsmExitStatus


initialiseOSM :: Global -> PeerConfig -> OpenStateMachine
initialiseOSM Global{..} PeerConfig{..} =
    makeOpenStateMachine BGPOpen { myAutonomousSystem = toAS2 $ fromMaybe ( myAS gd ) peerConfigLocalAS
                                   , holdTime = configOfferedHoldTime config
                                   , bgpID = fromMaybe ( myBGPid gd) peerConfigLocalBGPID
                                   , caps = peerConfigOfferedCapabilities}
                         BGPOpen { myAutonomousSystem = toAS2 $ fromMaybe 0 peerConfigAS
                                   , holdTime = 0
                                   , bgpID = fromMaybe (fromHostAddress 0) peerConfigBGPID
                                   , caps = peerConfigRequiredCapabilities}

runFSM :: Global -> SockAddr -> SockAddr -> BGPHandle -> Maybe PeerConfig -> IO (Either String String)
runFSM g@Global{..} socketName peerName handle =
-- The 'Maybe PeerData' allows the FSM to handle unwanted connections, i.e. send BGP Notification
-- thereby absolving the caller from having and BGP protocol awareness
    maybe (do bgpSnd handle $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionRejected L.empty
              return $ Left "connection rejected for unconfigured peer" )
          ( \peerConfig -> do ro <- newEmptyMVar
                              fsm (StateConnected, St{
                                                       peerName = peerName
                                                     , socketName = socketName
                                                     , handle = handle
                                                     , osm = initialiseOSM g peerConfig
                                                     , peerConfig = peerConfig
                                                     , maybePD = Nothing
                                                     , rcvdOpen = ro
                                                     , ribHandle = Nothing
                                                     }))
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

    idle s = do info $ "IDLE - reason: " ++ s
                return (Idle, undefined )

    stateConnected :: F
    stateConnected st@St{..} = do
        msg <- bgpRcv handle delayOpenTimer
        case msg of
            BGPTimeout -> do
                trace "stateConnected - event: delay open expiry"
                bgpSnd handle (localOffer osm)
                trace "stateConnected -> stateOpenSent"
                return (StateOpenSent,st )

            open@BGPOpen{} -> do
                let osm' = updateOpenStateMachine osm open
                    resp = getResponse osm'
                trace "stateConnected - event: rcv open"
                collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
                if isJust collision then do
                    bgpSnd handle $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionCollisionResolution L.empty
                    idle (fromJust collision)
                else if isKeepalive resp then do
                    trace "stateConnected -> stateOpenConfirm"
                    bgpSnd handle (localOffer osm)
                    bgpSnd handle resp
                    return (StateOpenConfirm , st {osm=osm'} )
                else do
                    bgpSnd handle resp
                    idle "stateConnected - event: open rejected error"

            BGPNotify{} ->
               -- TODO - improve Notify analysis and display
               idle "stateConnected -> exit rcv notify"

            BGPUpdate{} -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateConnected - recvd Update - FSM error"

            z -> idle $ "stateConnected - network exception - " ++ show z

    stateOpenSent :: F
    stateOpenSent st@St{..} = do
        msg <- bgpRcv handle initialHoldTimer
        case msg of

          BGPTimeout -> do
              bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
              idle "stateOpenSent - error initial Hold Timer expiry"

          open@BGPOpen{} -> do
              let osm' = updateOpenStateMachine osm open
                  resp = getResponse osm'
              trace "stateOpenSent - rcv open"
              collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
              if isJust collision then do
                  bgpSnd handle $ BGPNotify NotificationCease 0 L.empty
                  idle (fromJust collision)
              else if isKeepalive resp then do
                  bgpSnd handle resp
                  trace "stateOpenSent -> stateOpenConfirm"
                  return (StateOpenConfirm,st{osm=osm'})
              else do
                    bgpSnd handle resp
                    idle "stateOpenSent - event: open rejected error"

          BGPNotify{} -> idle "stateOpenSent - rcv notify"

          msg -> do
              bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
              idle $ "stateOpenSent - FSM error" ++ show msg

    stateOpenConfirm :: F
    stateOpenConfirm st@St{..} = do
        msg <- bgpRcv handle (getNegotiatedHoldTime osm)
        case msg of

            BGPTimeout -> do
                bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "stateOpenConfirm - error Hold Timer expiry"

            BGPKeepalive -> do
                trace "stateOpenConfirm - rcv keepalive"
                return (ToEstablished,st)

            BGPNotify{} -> idle "stateOpenConfirm - rcv notify"

            _ -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle "stateOpenConfirm - FSM error"

    toEstablished :: F
    toEstablished st@St{..} = do
        trace "transition -> established"
        trace $ "hold timer: " ++ show (getNegotiatedHoldTime osm) ++ " keep alive timer: " ++ show (getKeepAliveTimer osm)
        -- only now can we create the peer data record becasue we have the remote AS/BGPID available and confirmed

        let globalData = gd
            peerAS = fromIntegral $ myAutonomousSystem $ fromJust $ remoteOffer osm
            peerBGPid = bgpID $ fromJust $ remoteOffer osm
            (SockAddrInet pp remoteIP) = peerName
            (SockAddrInet lp localIP) = socketName
            peerIPv4 = fromHostAddress remoteIP
            peerPort = fromIntegral pp
            localIPv4 = fromHostAddress localIP
            localPort = fromIntegral lp
            peerLocalPref = 0 -- TODO - source this somewhere sensible - config?
            isExternal = peerAS /= myAS gd
            peerData = PeerData { .. }
        registerEstablished collisionDetector peerBGPid peerName
        -- VERY IMPORTANT TO USE THE NEW VALUE peerData' AS THIS IS THE ONLY ONE WHICH CONTAINS ACCURATE REMOTE IDENTITY FOR DYNAMIC PEERS!!!!
        -- it would be much better to remove the temptation to use conficured data by forcing a new type for relevant purposes, and dscarding the
        -- preconfigured values as soon as possible
        -- TODO - make the addPeer function return a handle so that rib and peer data are not exposed
        ribHandle <- Rib.addPeer rib peerData
        _ <- forkIO $ sendLoop handle ribHandle
        _ <- forkIO $ keepaliveLoop handle (getKeepAliveTimer osm)
        return (Established,st{maybePD=Just peerData , ribHandle = Just ribHandle})

    established :: F
    established st@St{..} = do
        msg <- bgpRcv handle (getNegotiatedHoldTime osm)
        case msg of

            BGPKeepalive -> do void $ Rib.ribPush (fromJust ribHandle) NullUpdate
                               return (Established,st)

            update@BGPUpdate{} -> do

                trace "established: BGPUpdate"
                Rib.ribPush (fromJust ribHandle) (parseUpdate update)
                return (Established,st)

            BGPNotify{} -> idle "established - rcv notify"

            BGPEndOfStream -> idle "established: BGPEndOfStream"

            BGPTimeout -> do
                bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "established - HoldTimerExpired error"
            _ -> do
                bgpSnd handle $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
                idle $ "established - FSM error (" ++ show msg ++ ")"

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
                           Just $ "collisionCheck - event: collision with tie-break - open rejected error for peer " ++ show session
                       else
                           Nothing
                )
            rc

    sendLoop handle rh = catch
        ( do
             updates <- Rib.ribPull rh
             trace $ "sendloop: updates (" ++ show (length updates) ++ ")"
             bgpSndAll handle updates
             sendLoop handle rh
        )
        (\(BGPIOException _) -> return ()
            -- this is the standard way to close down this thread
        )

    keepaliveLoop handle timer | timer == 0 = return ()
                               | otherwise  = catch
        ( do
             bgpSnd handle BGPKeepalive
             threadDelay ( 1000000 * timer)
             keepaliveLoop handle timer
        )
        (\(BGPIOException _) -> return ()
            -- this is the standard way to close down this thread
        )
