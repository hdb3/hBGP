module BGPRib.Fifo where

import System.Timeout(timeout)
import Control.Concurrent
import Data.Maybe

type Fifo t = MVar ([t],[t])

enqueue :: MVar ([a],[a] ) -> a -> IO ()
enqueue mvar item = do
    maybeFifo <- tryTakeMVar mvar
    let (h,t) = fromMaybe ([],[]) maybeFifo
    -- note this will not block as long the calling thread is the only producer
    putMVar mvar (item:h,t)

enqueueN :: MVar ([a],[a] ) -> [a] -> IO ()
enqueueN mvar [] = return ()
enqueueN mvar items = do
    maybeFifo <- tryTakeMVar mvar
    let (h,t) = fromMaybe ([],[]) maybeFifo
        cat [] bx = bx
        cat (a:[]) bx = a:bx
        cat (a:ax) bx = cat ax (a:ax)
    putMVar mvar (cat items h,t)


dequeue  :: MVar ([a],[a] ) -> IO [a]
dequeue mvar = do
    (h,t) <- takeMVar mvar
    return (t ++ reverse h)

-- this is a non-blocking call which will return an empty list when there are no items in the queue
-- WARNING - this function UNTESTED 
dequeue'  :: MVar ([a],[a] ) -> IO [a]
dequeue' mvar = do
    maybeFifo <- tryTakeMVar mvar
    maybe (return [])
          (\(h,t) -> do
              return (t ++ reverse h))
          maybeFifo

dequeueAll = dequeue

newFifo :: IO (Fifo t)
newFifo = newEmptyMVar

emptyFifo :: IO (Fifo t)
emptyFifo = newFifo

mkFifo :: [t] -> IO (Fifo t)
mkFifo [] = newEmptyMVar
mkFifo l = newMVar ([], reverse l)

showFifo :: Show t => Fifo t -> IO String
showFifo m = do
    t <- readMVar m
    return $ show t
