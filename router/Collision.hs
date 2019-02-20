{-# LANGUAGE Strict #-}
module Collision where
import Network.Socket
import Data.IP
import Control.Concurrent

type CollisionDetector = MVar [ Session ]
data Session = Session { sessionBgpid :: IPv4, sessionAddr :: SockAddr, sessionTid :: ThreadId, sessionEstablished :: Bool }  deriving (Eq,Show)

purge :: ThreadId -> [ Session ] -> [ Session ]
purge tid sx = reverse $ purge' tid [] sx  where
    purge' tid ret [] = ret
    purge' tid ret (s:sx) | tid == sessionTid s = purge' tid ret sx
                          | otherwise = purge' tid (s:ret) sx

mkCollisionDetector :: IO CollisionDetector
mkCollisionDetector = newMVar ([] :: [ Session ])

raceCheck :: CollisionDetector -> IPv4 -> SockAddr -> IO (Maybe Session)
raceCheck c bgpid addr = do
    threadID <- myThreadId
    sessions <- takeMVar c
    let collision = lookupBGPid bgpid sessions
    putMVar c $ Session bgpid addr threadID False : sessions
    return collision
    where
    lookupBGPid id [] = Nothing
    lookupBGPid id (s:sx) | id == sessionBgpid s = Just s
                          | otherwise = lookupBGPid id sx 

registerEstablished :: CollisionDetector -> IPv4 -> SockAddr -> IO ()
registerEstablished c bgpid addr = do
    threadID <- myThreadId
    sessions <- takeMVar c
    let sessions' = purge threadID sessions
    putMVar c $ Session bgpid addr threadID True : sessions'

deregister :: CollisionDetector -> IO ()
deregister c = do
    threadID <- myThreadId
    sessions <- takeMVar c
    putMVar c $ purge threadID sessions
    
