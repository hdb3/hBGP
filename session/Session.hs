{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Session where

import Data.Maybe
import Control.Concurrent
import Control.Monad (void,forever)
import Network.Socket(getPeerName,PortNumber,SockAddr(..))
import qualified Network.Socket as NS
import Network.Simple.TCP
import System.IO
import System.Exit(die)
import Data.IP
import qualified Data.Map.Strict as Data.Map
import System.IO.Error
import GHC.IO.Exception(ioe_description)
import Foreign.C.Error


type App = ((Socket,SockAddr) -> IO ())
type RaceCheck = (IPv4 -> IO Bool)
type RaceCheckUnblock = (IPv4 -> IO ())
data State = State { port :: PortNumber
                   , raceCheckBlock :: RaceCheck
                   , raceCheckNonBlock :: RaceCheck
                   , raceCheckUnblock :: RaceCheckUnblock
                   , defaultApp :: App
                   , peers :: [IPv4]
                   }

logger _ = return ()
seconds = 1000000
respawnDelay = 10 * seconds
idleDelay = 100 * seconds

session :: PortNumber -> App -> [IPv4] -> IO ()
session port defaultApp peers = do
    state <- mkState port defaultApp peers
    listener state
    mapM_ ( forkIO . run state ) peers

mkState port defaultApp peers = do
    mapMVar <- newMVar Data.Map.empty
    let raceCheckNonBlock port = do result <- raceCheck False mapMVar port
                                    -- debug -- putStrLn $ "raceCheckNonBlock: " ++ show port ++ " : " ++ show result
                                    return result
        raceCheckBlock port    = do result <- raceCheck True mapMVar port
                                    -- debug -- putStrLn $ "raceCheckBlock: " ++ show port ++ " : " ++ show result
                                    return result
        raceCheckUnblock port  = do -- debug -- putStrLn $ "raceCheckUnblocker: " ++ show port
                                    raceCheckUnblocker mapMVar port
    return State {..}

-- RACE CHECK
-- before calling the application perform a race check
-- if there is a race then don't call the application
-- optionally, block waiting for the other session to complete
-- the race check has three entry points - blocking and non block request, and unblock.
-- the race check uses a map stored in MVar, and another MVar to support the blocking request
-- the blocked request returns an error condition even when unblocked, to prevent an
-- overeager talker from sharing the limelight too easily

raceCheckUnblocker :: MVar (Data.Map.Map IPv4 (MVar ())) -> IPv4 -> IO ()
raceCheckUnblocker mapMVar address = do
    map <- readMVar mapMVar
    let Just peerMVar = Data.Map.lookup address map
    putMVar peerMVar ()

raceCheck :: Bool -> MVar (Data.Map.Map IPv4 (MVar ())) -> IPv4 -> IO Bool
raceCheck blocking mapMVar address = do
-- get the specific MVar out of the Map
-- if it doesn't exist then insert it and take it
-- this is non-blocking so if it does exist but is empty then just exit 
    -- threadId <- myThreadId
    map <- takeMVar mapMVar
    let maybePeerMVar = Data.Map.lookup address map
    maybe (do peerMVar <- newEmptyMVar :: IO (MVar ())
              putMVar mapMVar (Data.Map.insert address peerMVar map)
              return True )
          (\peerMVar -> do
              putMVar mapMVar map
              maybeFree <- tryTakeMVar peerMVar
              if isJust maybeFree
              then return True
              else if not blocking
              then return False else
                  do
                  _ <- readMVar peerMVar
                  return False
          )
          maybePeerMVar

listener :: State -> IO ()
listener state@State{..} = do
    -- eSock <- tryIOError ( bindSock HostIPv4 (show port ) )
    -- NOTE - the reason for using the lower level functions rather than network-simple
    --        is that bindSock from network-simple masks the errno by attemptin to close the socket
    --        This makes it impossible to provide accurate feedback (although the descriptive text in the
    --        exception IS correct).
    eSock <- tryIOError ( bindSock' port 0 )
    either
        ( \e -> do Errno errno <- getErrno
                   if | errno == 13 -> die "permission error binding port (are you su?)"
                      | errno `elem` [98] -> do hPutStrLn stderr "waiting to bind port"
                                                threadDelay (10 * seconds)
                                                listener state
                      | otherwise -> error $ errReport' errno e )
        ( \(listeningSocket,_) -> void $ forkIO $ do listenSock listeningSocket 100
                                                     forever  ( accept listeningSocket ( listenClient state )))
        eSock
    where
    bindSock' port ip = let addr = NS.SockAddrInet port ip in do
        sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock addr
        return ( sock , addr )


listenClient state@State{..} (sock, SockAddrInet _ remoteIPv4) = forkIO $ do
        let ip = fromHostAddress remoteIPv4
        logger $ "listener - connect request from " ++ show ip
        unblocked <- raceCheckNonBlock ip
        if unblocked then do
            wrap state defaultApp sock 
            raceCheckUnblock ip
        else do
            logger $ "listener - connect reject due to race"
            closeSock sock

fromPeerAddress (SockAddrInet _ ip) = fromHostAddress ip

wrap state@State{..} app sock = do
    peerAddress <- getPeerName sock
    let ip = fromPeerAddress peerAddress
    catchIOError
        ( do logger $ "connected to : " ++ show ip
             app ( sock , peerAddress )
             closeSock sock
             logger $ "app terminated for : " ++ show ip )
        (\e -> do Errno errno <- getErrno
                  logger $ "Exception in session with " ++ show ip ++ " - " ++ errReport errno e )

run :: State -> IPv4 -> IO ()
run state@State{..} ip = do
    unblocked <- raceCheckBlock ip
    if unblocked then
         do sock <- connectTo port ip
            maybe ( return () )
                  (wrap state defaultApp)
                  sock
            raceCheckUnblock ip
    else logger $ "run blocked for " ++ show ip
    threadDelay respawnDelay
    run state ip
    where
    connectTo port ip =
        catchIOError
        ( do (sock,_) <- connectSock (show ip) (show port)
             return $ Just sock )
        (\e -> do
            Errno errno <- getErrno
            -- most errors are timeouts or connection rejections from unattended ports
            -- a better way to report would be handy - repeated console messages are not useful!
            logger $ "Exception connecting to " ++ show ip ++ " - " ++ errReport errno e
            return Nothing )

errReport errno e | errno `elem` [2,107] = ioe_description e ++ " (" ++ show errno ++ ")"
                  | otherwise = errReport' errno e

errReport' errno e = unlines
    [ "*** UNKNOWN exception, please record this"
    -- , ioeGetErrorString e
    , "error " ++ ioeGetErrorString e
    , "errno " ++ show errno
    , "description " ++ ioe_description e
    ]
