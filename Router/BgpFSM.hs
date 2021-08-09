module Router.BgpFSM (bgpFSM) where

import BGPRib.BGPRib (PeerData (..), myAS, myBGPid)
import BGPlib.BGPlib
import ByteString.StrictBuilder
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception
-- TODO = move some or all of BGPData, from bgprib to bgplib, so that bgprib does not need to be imported here.....
--import qualified Router.CustomRib as Rib

import Control.Logger.Simple
import Control.Monad (void)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Data.Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import Data.Word
import Network.Socket
import Router.Collision
import Router.Config
import Router.Global
import Router.Open
import qualified Router.StdRib as Rib

data BGPState = St
  { handle :: BGPHandle,
    peerName :: SockAddr,
    socketName :: SockAddr,
    osm :: OpenStateMachine,
    peerConfig :: PeerConfig,
    maybePD :: Maybe PeerData,
    rcvdOpen :: MVar BGPMessage,
    ribHandle :: Maybe Rib.RibHandle
  }

type F = BGPState -> IO (FSMState, BGPState)

trace = logTrace . T.pack

warn = logWarn . T.pack

data FSMState = StateConnected | StateOpenSent | StateOpenConfirm | ToEstablished | Established | Idle deriving (Show, Eq)

bgpFSM :: Global -> (Socket, SockAddr) -> IO ()
bgpFSM global@Global {..} (sock, peerName) =
  do
    threadId <- myThreadId
    logTrace $ T.pack $ "Thread " ++ show threadId ++ " starting: peer is " ++ show peerName

    socketName <- getSocketName sock
    let (SockAddrInet remotePort remoteIP) = peerName
        (SockAddrInet _ localIP) = socketName
    handle <- getBGPHandle sock

    -- lookup explicit local IP then failover to widlcard adn eventually, if allowed, a dynamic peer
    let maybePeer =
          Data.Map.lookup (fromHostAddress localIP, fromHostAddress remoteIP) peerMap
            <|> Data.Map.lookup (fromHostAddress 0, fromHostAddress remoteIP) peerMap
            <|> if configAllowDynamicPeers config
              then Just (fillConfig config (fromHostAddress remoteIP))
              else Nothing
    fsmExitStatus <-
      catch
        (startFSM global socketName peerName handle maybePeer)
        ( \(BGPIOException s) ->
            return $ Left s
        )
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
initialiseOSM Global {..} PeerConfig {..} =
  let toAS2 :: Word32 -> Word16
      toAS2 as
        | as < 0x10000 = fromIntegral as
        | otherwise = 23456
   in makeOpenStateMachine
        BGPOpen
          { myAutonomousSystem = toAS2 $ fromMaybe (myAS gd) peerConfigLocalAS,
            holdTime = configOfferedHoldTime config,
            bgpID = fromMaybe (myBGPid gd) peerConfigLocalBGPID,
            caps = peerConfigOfferedCapabilities
          }
        BGPOpen
          { myAutonomousSystem = toAS2 $ fromMaybe 0 peerConfigAS,
            holdTime = 0,
            bgpID = fromMaybe (fromHostAddress 0) peerConfigBGPID,
            caps = peerConfigRequiredCapabilities
          }

-- bgpSnd :: BGPHandle -> BGPMessage -> IO ()
-- bgpSnd bgph bgpmsg = bgpSendHandle bgph (builderBytes $ builder bgpmsg)

-- bgpSndOutput :: BGPHandle -> BGPOutput -> IO ()
-- bgpSndOutput h m = bgpSndAll h [m]

-- bgpSndAll :: BGPHandle -> [BGPOutput] -> IO ()
-- bgpSndAll bgph msgs = bgpSendHandle bgph (deparseBGPOutputs msgs)

startFSM :: Global -> SockAddr -> SockAddr -> BGPHandle -> Maybe PeerConfig -> IO (Either String String)
startFSM g@Global {..} socketName peerName handle =
  -- The 'Maybe PeerData' allows the FSM to handle unwanted connections, i.e. send BGP Notification
  -- thereby absolving the caller from having any BGP protocol awareness
  maybe
    ( do
        bgpSnd $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionRejected L.empty
        return $ Left "connection rejected for unconfigured peer"
    )
    ( \peerConfig -> do
        ro <- newEmptyMVar
        let initBGPState =
              St
                { peerName = peerName,
                  socketName = socketName,
                  handle = handle,
                  osm = initialiseOSM g peerConfig,
                  peerConfig = peerConfig,
                  maybePD = Nothing,
                  rcvdOpen = ro,
                  ribHandle = Nothing
                }
        exitState <- actions (StateConnected, initBGPState)
        maybe
          (warn "FSM exit without defined peer")
          (writeChan monitorChannel . Left)
          (maybePD exitState)
        return $ Right "FSM normal exit"
    )
  where
    bgpSnd bgpmsg = bgpSendHandle handle (builderBytes $ builder bgpmsg)
    bgpSndOutput m = bgpSndAll [m]
    bgpSndAll msgs = bgpSendHandle handle (deparseBGPOutputs msgs)

    actions :: (FSMState, BGPState) -> IO BGPState
    actions (s, st) = do
      (s', st') <- action (s, st)
      if s' == Idle then return st' else actions (s', st')

    action :: (FSMState, BGPState) -> IO (FSMState, BGPState)
    action (Idle, st) = error "unreachable"
    --
    -- StateConnected
    --
    action (StateConnected, st@St {..}) = do
      msg <- bgpRcv handle delayOpenTimer
      case msg of
        BGPTimeout -> do
          trace "stateConnected - event: delay open expiry"
          bgpSnd (localOffer osm)
          trace "stateConnected -> stateOpenSent"
          return (StateOpenSent, st)
        open@BGPOpen {} -> do
          let osm' = updateOpenStateMachine osm open
              resp = getResponse osm'
          trace "stateConnected - event: rcv open"
          collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
          if isJust collision
            then do
              bgpSnd $ BGPNotify NotificationCease _NotificationCeaseSubcodeConnectionCollisionResolution L.empty
              idle st (fromJust collision)
            else
              if isKeepalive resp
                then do
                  trace "stateConnected -> stateOpenConfirm"
                  bgpSnd (localOffer osm)
                  bgpSnd resp
                  return (StateOpenConfirm, st {osm = osm'})
                else do
                  bgpSnd resp
                  idle st "stateConnected - event: open rejected error"
        BGPNotify {} ->
          -- TODO - improve Notify analysis and display
          idle st "stateConnected -> exit rcv notify"
        BGPUpdate {} -> do
          bgpSnd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
          idle st "stateConnected - recvd Update - FSM error"
        z -> idle st $ "stateConnected - network exception - " ++ show z
    --
    -- StateOpenSent
    --
    action (StateOpenSent, st@St {..}) = do
      msg <- bgpRcv handle initialHoldTimer
      case msg of
        BGPTimeout -> do
          bgpSnd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
          idle st "stateOpenSent - error initial Hold Timer expiry"
        open@BGPOpen {} -> do
          let osm' = updateOpenStateMachine osm open
              resp = getResponse osm'
          trace "stateOpenSent - rcv open"
          collision <- collisionCheck collisionDetector (myBGPid gd) (bgpID open)
          if isJust collision
            then do
              bgpSnd $ BGPNotify NotificationCease 0 L.empty
              idle st (fromJust collision)
            else
              if isKeepalive resp
                then do
                  bgpSnd resp
                  trace "stateOpenSent -> stateOpenConfirm"
                  return (StateOpenConfirm, st {osm = osm'})
                else do
                  bgpSnd resp
                  idle st "stateOpenSent - event: open rejected error"
        BGPNotify {} -> idle st "stateOpenSent - rcv notify"
        msg -> do
          bgpSnd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
          idle st $ "stateOpenSent - FSM error" ++ show msg
    --
    -- StateOpenConfirm
    --
    action (StateOpenConfirm, st@St {..}) = do
      msg <- bgpRcv handle (getNegotiatedHoldTime osm)
      case msg of
        BGPTimeout -> do
          bgpSnd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
          idle st "stateOpenConfirm - error Hold Timer expiry"
        BGPKeepalive -> do
          trace "stateOpenConfirm - rcv keepalive"
          return (ToEstablished, st)
        BGPNotify {} -> idle st "stateOpenConfirm - rcv notify"
        _ -> do
          bgpSnd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
          idle st "stateOpenConfirm - FSM error"
    --
    -- ToEstablished
    --
    action (ToEstablished, st@St {..}) = do
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
          peerLocalPref = peerConfigLocalPref peerConfig
          isExternal = peerAS /= myAS gd
          peerData = PeerData {..}
      registerEstablished collisionDetector peerBGPid peerName
      -- VERY IMPORTANT TO USE THE NEW VALUE peerData' AS THIS IS THE ONLY ONE WHICH CONTAINS ACCURATE REMOTE IDENTITY FOR DYNAMIC PEERS!!!!
      -- it would be much better to remove the temptation to use conficured data by forcing a new type for relevant purposes, and dscarding the
      -- preconfigured values as soon as possible
      ribHandle <- Rib.addPeer rib peerData
      _ <- forkIO $ sendLoop ribHandle
      _ <- forkIO $ keepaliveLoop handle (getKeepAliveTimer osm)
      writeChan monitorChannel (Right peerData)
      return (Established, st {maybePD = Just peerData, ribHandle = Just ribHandle})
    --
    -- Established
    --
    action (Established, st@St {..}) = do
      msg <- bgpRcv handle (getNegotiatedHoldTime osm)
      case msg of
        BGPKeepalive -> do
          void $ Rib.ribPush (fromJust ribHandle) NullUpdate
          return (Established, st)
        update@BGPUpdate {} -> do
          trace "established: BGPUpdate"
          Rib.ribPush (fromJust ribHandle) (parseUpdate update)
          return (Established, st)
        BGPNotify {} -> idle st "established - rcv notify"
        BGPEndOfStream -> idle st "established: BGPEndOfStream"
        BGPTimeout -> do
          bgpSnd $ BGPNotify NotificationHoldTimerExpired 0 L.empty
          idle st "established - HoldTimerExpired error"
        _ -> do
          bgpSnd $ BGPNotify NotificationFiniteStateMachineError 0 L.empty
          idle st $ "established - FSM error (" ++ show msg ++ ")"

    idle st s = do
      trace $ "IDLE - reason: " ++ s
      return (Idle, st)
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
        ( \session ->
            return $
              if sessionEstablished session
                then Just $ "collisionCheck - event: collision with established session - open rejected error for peer " ++ show session
                else -- TODO - check this logic
                -- does it consider whether we initiated the connection or not?
                -- this requires to look at the port numbers

                  if peer < self
                    then Just $ "collisionCheck - event: collision with tie-break - open rejected error for peer " ++ show session
                    else Nothing
        )
        rc

    sendLoop rh =
      catch
        ( do
            updates <- Rib.ribPull rh
            case updates of
              [] ->
                return ()
              -- Exit on an empty list because the enqueue/dequeue logoc never signals on an empty FIFO;
              -- The empty list represents a failure to find the peer in the list of current peers
              -- Which transient condition arises when the parent process has exited.
              _ -> do
                trace $ "sendloop: updates (" ++ show (length updates) ++ ")"
                bgpSndAll updates
                sendLoop rh
        )
        ( \(BGPIOException _) -> return ()
        -- this is the standard way to close down this thread
        )

    keepaliveLoop handle timer
      | timer == 0 = return ()
      | otherwise =
        catch
          ( do
              threadDelay (1000000 * timer)
              trace $ "keepaliveLoop send"
              bgpSnd BGPKeepalive
              keepaliveLoop handle timer
          )
          ( \(BGPIOException _) -> do
              -- this is the standard way to close down this thread
              trace $ "keepaliveLoop exit"
              return ()
          )
