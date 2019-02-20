module AdjRIBOut(module Fifo, module AdjRIBOut) where

{-
 - AdjRIBTable provdes a list structure to support route dissemination
 - every peer has its own AdjRIBTable in order to allow each peer to consume Updates at its own pace
 - an optimisation to suppress duplicate updates due to slow consumption can be implnented outside this API
 - using the route identity which is stored with the prefix set
 - the structure is a simple list which holds a set of prefixes
 - prefixes are grouped to maintain packing of prefixes within a single update message
 - fixing up fragmentation of prefixes over common routes is not attempted because it is unlikely to be useful
 - the exception is in the event of route reefersh or peer session initilisation,
 - when an entire route table must be exchanged
-}

import qualified Data.IntMap.Strict
import qualified Data.Tuple

import Fifo
import BGPlib

-- TODO
-- bite this bullet? - the AdjRIBEntry should simply carry the RouteData itself rather than the proxy Int which is a hash over the original
-- Update Path Attributes
--

type AdjRIBEntry = ( [IPrefix], Int )
type AdjRIBTable = Fifo AdjRIBEntry

showAdjRIBTable :: AdjRIBTable -> IO String
showAdjRIBTable = showFifo

newAdjRIBTable  ::  IO AdjRIBTable
newAdjRIBTable = emptyFifo

insertAdjRIBTable :: AdjRIBEntry -> AdjRIBTable -> IO ()
insertAdjRIBTable are table = enqueue table are

getAllAdjRIBTable :: AdjRIBTable -> IO [AdjRIBEntry]
getAllAdjRIBTable = dequeueAll

groomAdjRIBList :: [AdjRIBEntry] -> [AdjRIBEntry]
groomAdjRIBList = map Data.Tuple.swap . Data.IntMap.Strict.toList . Data.IntMap.Strict.fromList . map Data.Tuple.swap
