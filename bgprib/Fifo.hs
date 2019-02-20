module Fifo where

import System.Timeout(timeout)
import Control.Concurrent
import Data.Maybe

type Fifo t = MVar ([t],[t])

-- this is a blocking call
dequeueN  :: Int -> MVar ([a],[a] ) -> IO [a]
dequeueN n mvar  = do
    (h,t) <- takeMVar mvar
    if length t >= n then do
        let (r,t') = splitAt n t
        putMVar mvar (h,t')
        return r
    else do
        let (r,t') = splitAt n (t ++ reverse h)
        putMVar mvar ([],t')
        return r

enqueue :: MVar ([a],[a] ) -> a -> IO ()
enqueue mvar item = do
    maybeFifo <- tryTakeMVar mvar
    let (h,t) = fromMaybe ([],[]) maybeFifo
    -- note this will not block as long the calling thread is the only producer
    putMVar mvar (item:h,t)

-- this is a blocking call
dequeue  :: MVar ([a],[a] ) -> IO [a]
dequeue mvar = do
    (h,t) <- takeMVar mvar
    -- putMVar mvar([],[])
    return (t ++ reverse h)

-- this is a non-blocking call which will return an empty list when there are no items in the queue
dequeue'  :: MVar ([a],[a] ) -> IO [a]
dequeue' mvar = do
    maybeFifo <- tryTakeMVar mvar
    maybe (return [])
          (\(h,t) -> do
              putMVar mvar([],[])
              return (t ++ reverse h))
          maybeFifo

dequeueAll = dequeue'

newFifo :: IO (Fifo t)
newFifo = newMVar ([],[])

emptyFifo :: IO (Fifo t)
emptyFifo = newFifo

fifo :: [t] -> IO (Fifo t)
fifo l = newMVar ([],reverse l)

showFifo :: Show t => Fifo t -> IO String
showFifo m = do
    t <- readMVar m 
    return $ show t

-- calls with timeouts....

dequeueTimeout :: Int -> Fifo t -> IO [t]
dequeueTimeout t mvar = do
     resMaybe <- timeout t (dequeue mvar)
     maybe
         (return [])
         return
         resMaybe
