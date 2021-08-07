module BGPRib.AdjRIBOut (module BGPRib.Fifo, module BGPRib.AdjRIBOut) where

{-
 - AdjRIBOut provides a list structure to support route dissemination
 - every peer has its own PeerAdjRIBOut in order to allow each peer to consume Updates at its own pace
 - an optimisation to suppress duplicate updates due to slow consumption can be implnented outside this API
 - using the route identity which is stored with the prefix set
 - the structure is a simple list which holds a set of prefixes
 - prefixes are grouped to maintain packing of prefixes within a single update message
 - fixing up fragmentation of prefixes over common routes is not attempted because it is unlikely to be useful
 - the exception is in the event of route refresh or peer session initilisation,
 - when an entire route table must be exchanged
-}

import BGPRib.Fifo
import BGPlib.BGPlib
import qualified Data.IntMap.Strict
import qualified Data.Tuple

type PathChange = ([Prefix], Int)

data PeerAdjRIBOut = PeerAdjRIBOut {pathChanges :: Fifo PathChange}

showPeerAdjRIBOut :: PeerAdjRIBOut -> IO String
showPeerAdjRIBOut = showFifo . pathChanges

newPeerAdjRIBOut :: IO PeerAdjRIBOut
newPeerAdjRIBOut = fmap PeerAdjRIBOut emptyFifo

insertPathChange :: PathChange -> PeerAdjRIBOut -> IO ()
insertPathChange are table = enqueue (pathChanges table) are

insertPathChanges :: [PathChange] -> PeerAdjRIBOut -> IO ()
insertPathChanges arex table = enqueueN (pathChanges table) arex

getPathChanges :: PeerAdjRIBOut -> IO [PathChange]
getPathChanges = dequeueAll . pathChanges

groomPathChanges :: [PathChange] -> [PathChange]
groomPathChanges = map Data.Tuple.swap . Data.IntMap.Strict.toList . Data.IntMap.Strict.fromList . map Data.Tuple.swap
