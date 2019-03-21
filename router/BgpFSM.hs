{-# LANGUAGE RecordWildCards #-}
module BgpFSM(bgpFSM) where
import Network.Socket
import System.IO.Error(catchIOError)
import System.IO(IOMode( ReadWriteMode ),Handle, hClose)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Data.Binary(encode)
import Data.IP
import Control.Concurrent
import Control.Exception
import Data.Maybe(fromJust,isJust,fromMaybe)
import Data.Either(either)
import qualified Data.Map.Strict as Data.Map
import System.Posix.Temp(mkstemp)
import qualified System.Posix.Types as SPT
import Control.Applicative ((<|>))

import BGPRib(endOfRib,processUpdate,encodeUpdates,GlobalData(..),PeerData(..),ParsedUpdate(..))
-- TODO = move Update.hs, and ppssibly some or all of BGPData, from bgprib to bgplib, so that bgprib does not need to be imported here.....
--        the needed thing in BGPData is PeerData, but what else should move to is less obvious
--        there are some things which any BGP application needs.....

import BGPlib
import Open
import Collision
import qualified CustomRib as Rib
--import qualified StdRib as Rib
import Global
import Config
import Log
import Session(fdWaitOnQEmpty)

data FSMState = St { handle :: Handle
                   , fd :: SPT.Fd
                   , peerName :: SockAddr
                   , socketName :: SockAddr
                   , osm :: OpenStateMachine
                   , peerConfig :: PeerConfig
                   , maybePD :: Maybe PeerData
                   , rcvdOpen :: MVar BGPMessage
                   , ribHandle :: Maybe (Rib.RibHandle) }

type F = FSMState -> IO (State, FSMState)

data State = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show,Eq)

newtype FSMException = FSMException String
    deriving Show

instance Exception FSMException

bgpFSM :: Global -> ( Socket , SockAddr) -> IO ()
bgpFSM global@Global{..} ( sock , peerName ) =
                          do threadId <- myThreadId
                             trace $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName

                             socketName <- getSocketName sock
                             let (SockAddrInet remotePort remoteIP) = peerName
                                 (SockAddrInet localPort localIP)   = socketName
                             fd <- SPT.Fd <$> fdSocket sock
                             handle <- socketToHandle sock ReadWriteMode

                             -- lookup explicit local IP then failover to widlcard adn eventually, if allowed, a dynamic peer
                             let maybePeer = ( Data.Map.lookup (fromHostAddress localIP , fromHostAddress remoteIP) peerMap ) <|>
                                             ( Data.Map.lookup (fromHostAddress 0 , fromHostAddress remoteIP) peerMap ) <|>
                                             ( if configAllowDynamicPeers config
                                               then Just (fillConfig config (fromHostAddress remoteIP))
                                               else Nothing)
                             fsmExitStatus <-
                                         catch
                                             (runFSM global socketName peerName handle fd maybePeer )
                                             (\(FSMException s) ->
                                                 return $ Left s)
                             -- TDOD throuuigh testing around delPeer
                             -- TODO REAL SOON - FIX....
                             hClose handle
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

bgpSnd :: Handle -> BGPMessage -> IO()
bgpSnd h msg | 4079 > L.length (encode msg) = catchIOError ( sndRawMessage h (encode msg)) (\e -> throw $ FSMException (show (e :: IOError)))
             | otherwise = do (n,h) <- mkstemp "bgpSnd"
                              L.hPut h (encode msg)
                              hClose h
                              warn $ "encoded message too long in bgpSnd, encoded message was written to: " ++ n

bgpSndAll :: Handle -> [BGPMessage] -> IO()
bgpSndAll h msgs = do
    let encodedMessages = map encode msgs
    catchIOError ( sndRawMessages h encodedMessages )
                 (\e -> throw $ FSMException (show (e :: IOError)))

get :: Handle -> Int -> IO BGPMessage
get b t = fmap decodeBGPByteString (getRawMsg b t)

runFSM :: Global -> SockAddr -> SockAddr -> Handle -> SPT.Fd -> Maybe PeerConfig -> IO (Either String String)
runFSM g@Global{..} socketName peerName handle fd =
-- The 'Maybe PeerData' allows the FSM to handle unwanted connections, i.e. send BGP Notification
-- thereby absolving the caller from having and BGP protocol awareness
    maybe (do bgpSnd handle $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionRejected L.empty
              return $ Left "connection rejected for unconfigured peer" )
          ( \peerConfig -> do ro <- newEmptyMVar
                              fsm (StateConnected, St{
                                                       peerName = peerName
                                                     , socketName = socketName
                                                     , handle = handle
                                                     , fd = fd
                                                     , osm = initialiseOSM g peerConfig
                                                     , peerConfig = peerConfig
                                                     , maybePD = Nothing
                                                     , rcvdOpen = ro
                                                     , ribHandle = Nothing
                                                     }))
    where

    rawPut bs = catchIOError ( BS.hPut handle bs )
                             (\e -> throw $ FSMException (show (e :: IOError)))

    framedPut msgs = rawPut ( L.toStrict $ L.concat $ map wireFormat msgs)

    bgpMessagesPut bgpMsgs = framedPut ( map encode bgpMsgs )

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
        msg <- get handle delayOpenTimer
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
        msg <- get handle initialHoldTimer
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
        msg <- get handle (getNegotiatedHoldTime osm)
        case msg of

            BGPTimeout -> do
                bgpSnd handle $ BGPNotify NotificationHoldTimerExpired 0 L.empty
                idle "stateOpenConfirm - error initial Hold Timer expiry"

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
            localPref = 0 -- TODO - source this somewhere sensible - config?
            isExternal = peerAS /= myAS gd
            peerData = PeerData { .. }
        registerEstablished collisionDetector peerBGPid peerName
        -- VERY IMPORTANT TO USE THE NEW VALUE peerData' AS THIS IS THE ONLY ONE WHICH CONTAINS ACCURATE REMOTE IDENTITY FOR DYNAMIC PEERS!!!!
        -- it would be much better to remove the temptation to use conficured data by forcing a new type for relevant purposes, and dscarding the
        -- preconfigured values as soon as possible
        -- TODO - make the addPeer function return a handle so that rib and peer data are not exposed
        ribHandle <- Rib.addPeer rib peerData
        _ <- forkIO $ sendLoop handle (getKeepAliveTimer osm) ribHandle
        return (Established,st{maybePD=Just peerData , ribHandle = Just ribHandle})

    established :: F
    established st@St{..} = do
        msg <- get handle (getNegotiatedHoldTime osm)
        case msg of

            BGPKeepalive -> do _ <- Rib.ribPush (fromJust ribHandle) msg
                               return (Established,st)

            update@BGPUpdate{} -> do
                pushResponse <- Rib.ribPush (fromJust ribHandle) update
                if pushResponse then
                    return (Established,st)
                else do
                    bgpSnd handle $ BGPNotify NotificationUPDATEMessageError 0 L.empty
                    idle "established - Update parse error"
--                maybe
--                    ( do
--                         bgpSnd handle $ BGPNotify NotificationUPDATEMessageError 0 L.empty
--                         idle "established - Update parse error"
--                    )
--                    (\parsedUpdate -> do
--                      Rib.ribPush (fromJust ribHandle) parsedUpdate
--                      return (Established,st)
--                    )
--                    ( processUpdate update )

            BGPNotify{} -> idle "established - rcv notify"

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
                           Just $ "collisionCheck - event: collision with tie-break - open rejected error for peer " ++ show session
                       else
                           Nothing
                )
            rc

-- loop runs until it catches the FSMException
        -- wrapper to allow catching the entry and doing some a bit special, like sending EoR and Keepalive
    sendLoop handle timer rh = bgpMessagesPut [endOfRib,BGPKeepalive] >>
                               sendLoop' handle timer rh

    sendLoop' handle timer rh = catch
        ( do
             -- this forces a delay until the TCP ACK for all sent messages
             -- however there could still be a lot (500kb?) of data in the receiver's queue.
             -- fdWaitOnQEmpty fd
             updates <- Rib.msgTimeout timer (Rib.ribPull rh)
             if null updates then
                 bgpSnd handle BGPKeepalive
             else do
                 bgpMessagesPut updates
                 --bgpSndAll handle updates
                 --mapM_ (bgpSnd handle ) updates
             sendLoop' handle timer rh
        )
        (\(FSMException _) -> return ()
            -- this is perfectly normal event when the fsm closes down as it doesn't stop the keepAliveLoop explicitly
        )
