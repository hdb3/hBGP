module Main where
import Network.Socket
import Control.Concurrent
import Data.IP

import Collision
import Common

main = do
    c <- mkCollisionDetector
    threadID <- myThreadId
    t <- newMVar threadID
    tid1 <- forkIO $ loop c t 5 (read "127.0.0.1") (SockAddrInet bgpPort (toHostAddress $ read "127.0.0.1") ) 
    tid2 <- forkIO $ loop c t 3 (read "127.0.0.1") (SockAddrInet 50000   (toHostAddress $ read "127.0.0.1") )
    monitor c t

monitor :: CollisionDetector -> MVar ThreadId -> IO()
monitor c t = do
    tid <- takeMVar t
    s <- readMVar c
    putStrLn $ "monitor" ++ show tid
    putStrLn $  show s
    putStrLn ""
    monitor c t

loop :: CollisionDetector -> MVar ThreadId -> Int -> IPv4 -> SockAddr -> IO()
loop c t n bgpid addr = do
    threadDelay 1000000
    threadID <- myThreadId
    let p s = putStrLn $ "thread: " ++ show threadID ++ " - " ++ s
    p "start"
    threadDelay $ 1000000 * n
    p "raceCheck"
    ms <- raceCheck c bgpid addr
    putMVar t threadID
    maybe (do p "no collision"
              threadDelay $ 1000000 * n
              p "registerEstablished"
              registerEstablished c bgpid addr
              putMVar t threadID
              threadDelay $ 1000000 * n
              p "deregister"
              deregister c
              putMVar t threadID
              p "exit")
          (\collision -> do p $ "collision detected with" ++ show collision
                            p "deregister"
                            deregister c
                            putMVar t threadID
                            p "exit")
          ms

