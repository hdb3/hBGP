module BGPRib.AdjRIBOut (module BGPRib.Fifo, module BGPRib.AdjRIBOut) where

-- TDOD - integrate this in some larger source file (Rib.hs?)
import BGPRib.Fifo
import BGPlib.BGPlib (Prefix)

type PathChange = ([Prefix], Int)

type PeerAdjRIBOut = Fifo PathChange

newPeerAdjRIBOut :: IO PeerAdjRIBOut
newPeerAdjRIBOut = emptyFifo

insertPathChanges :: [PathChange] -> PeerAdjRIBOut -> IO ()
insertPathChanges arex table = enqueueN table arex
