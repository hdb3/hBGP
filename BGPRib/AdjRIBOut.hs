module BGPRib.AdjRIBOut (module BGPRib.Fifo, module BGPRib.AdjRIBOut) where

{-
 - AdjRIBTable provides a list structure to support route dissemination
 - every peer has its own AdjRIBTable in order to allow each peer to consume Updates at its own pace
 - an optimisation to suppress duplicate updates due to slow consumption can be implnented outside this API
 - using the route identity which is stored with the prefix set
 - the structure is a simple list which holds a set of prefixes
 - prefixes are grouped to maintain packing of prefixes within a single update message
 - fixing up fragmentation of prefixes over common routes is not attempted because it is unlikely to be useful
 - the exception is in the event of route reefersh or peer session initialisation,
 - when an entire route table must be exchanged
-}

import BGPRib.Fifo
import BGPlib.BGPlib
import qualified Data.IntMap.Strict
import qualified Data.Tuple

type AdjRIBEntry = ([Prefix], Int)

newtype AdjRIBTable = AdjRIBTable {fifo :: Fifo AdjRIBEntry}

showAdjRIBTable :: AdjRIBTable -> IO String
showAdjRIBTable = showFifo . fifo

newAdjRIBTable :: IO AdjRIBTable
newAdjRIBTable = fmap AdjRIBTable emptyFifo

insertAdjRIBTable :: AdjRIBEntry -> AdjRIBTable -> IO ()
insertAdjRIBTable are table = enqueue (fifo table) are

insertNAdjRIBTable :: [AdjRIBEntry] -> AdjRIBTable -> IO ()
insertNAdjRIBTable arex table = enqueueN (fifo table) arex

getAllAdjRIBTable :: AdjRIBTable -> IO [AdjRIBEntry]
getAllAdjRIBTable = dequeueAll . fifo

groomAdjRIBList :: [AdjRIBEntry] -> [AdjRIBEntry]
groomAdjRIBList = map Data.Tuple.swap . Data.IntMap.Strict.toList . Data.IntMap.Strict.fromList . map Data.Tuple.swap
