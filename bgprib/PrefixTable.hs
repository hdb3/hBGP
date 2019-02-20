{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module PrefixTable where

{- A single prefix table holds everything about a prefix we could care about
 - but, this is merely the prefix itself, and the associated path
 -
 - for IPv4 the prefix including length fits in a 64 bit word, so can be the actual key
 - though it might be that a simple scarmable operation would make a better key for a tree...
 - Note also that the pathtable key is also a 64 bit word, so a map of Ints is all that is required....
 -
 - However, the LocRIB needs to access every prefix table when performing selection
 -
 - Note: the route selection algorithm is at the heart of this system, and is performed for every prefix inserted
 - hence a fast implementation is essential
-}

import qualified Data.IntMap.Strict as IntMap
import qualified Data.SortedList as SL -- package sorted-list
import qualified Data.List

import BGPData
import BGPlib (IPrefix(..))

-- TODO
-- prefix tabel semantics require prefixes to be removed from the table when routes are lost.
-- This is suboptimal, as well as leading to more complex code.
-- The proposal is to change the logic, and retain prefeixes in the table even when there are no longer
-- any routes to them.
-- This should have low impact in worst case, and is postive in all others
-- Contrived worst cases could involve huge numbers of transient prefixes, e.g. /32s
-- This could be mitigated by grooming the table at (quiet) ointervals

type PrefixTableEntry = SL.SortedList RouteData 
type PrefixTable = IntMap.IntMap PrefixTableEntry
type PrefixTableElement = (Int,SL.SortedList RouteData)

instance {-# OVERLAPPING #-} Show PrefixTableElement where
    show (k,v) = "(" ++ show (IPrefix k) ++ "," ++ show v ++ ")"

instance {-# OVERLAPPING #-} Show PrefixTable where
    show = show . IntMap.toList

newPrefixTable :: PrefixTable
newPrefixTable = IntMap.empty

slHead sl = x where
    Just (x,_) = SL.uncons sl

update:: PrefixTable -> [IPrefix] -> RouteData -> (PrefixTable,[IPrefix])
update pt pfxs route = Data.List.foldl' f (pt,[]) pfxs where
    f (pt_,updated) pfx = if p then (pt__,pfx:updated) else (pt__,updated) where
        (pt__,p) = updatePrefixTable pt_ pfx route

updatePrefixTable :: PrefixTable -> IPrefix -> RouteData -> (PrefixTable,Bool)
updatePrefixTable pt (IPrefix ipfx) route = (newPrefixTable, isNewBestRoute) where 
    updatePrefixTableEntry :: PrefixTableEntry -> PrefixTableEntry -> PrefixTableEntry
    updatePrefixTableEntry singletonRoute routes = let newRoute = slHead singletonRoute
                                                       pIsNotOldRoute r = peerData r /= peerData newRoute
                                                   in SL.insert newRoute $ SL.filter pIsNotOldRoute routes

    newSingletonPrefixTableEntry = SL.singleton route
    (maybeOldPrefixTableEntry, newPrefixTable) = IntMap.insertLookupWithKey f ipfx newSingletonPrefixTableEntry pt where
        f _ = updatePrefixTableEntry
    newPrefixTableEntry = maybe newSingletonPrefixTableEntry ( updatePrefixTableEntry newSingletonPrefixTableEntry ) maybeOldPrefixTableEntry
    newBestRoute = slHead newPrefixTableEntry
    isNewBestRoute = newBestRoute == route

-- this function finds the best route for a specicif prefix
-- if the requirement is bulk look up then another function might be better.....
queryPrefixTable :: PrefixTable -> IPrefix -> Maybe RouteData
queryPrefixTable table (IPrefix iprefix) = fmap slHead (IntMap.lookup iprefix table)

{-
  Withdraw Operations

  'withdraw' is a wrapper around 'withdrawPrefixTable'.
  'withdrawPrefixTable' updates the prefixTable Map for a specific prefix
  Within 'withdrawPrefixTable' is an inner function which manipulates the route list for the prefix.
  The manipulation removes the given route, identified by the peer which originated it.
  In the case that the withdrawn route was the 'best' route then a Bool return flag is set to allow the calling context
  to send updates replacing the route which has been withdrawn.

  TODO

  Withdraw semantics require the withdrawn route to be known, because 'withdraw' should only be sent to peers
  which have received the corresponding route (e.g., dont' send a withdraw to a peer which originated a route..)
  As of now, withdraw only supports bare prefixes.
-}

withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[IPrefix])
-- core function is mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
-- which acts on the prefix table, returning a modified prefix table and also the list of prefixes which have been modifed
-- in a way which REQUIRES an update
-- the kernel function of type (a -> Key -> b -> (a, c)) has concrete signature [IPrefix] -> IPrefix -> PrefixTableEntry -> ([IPrefix], PrefixTableEntry)
-- (note that our input and output mapas have the same type, so type a == type b = PrefixTableEntry)
-- hence the outer function is simply:
withdrawPeer prefixTable peerData = swapNgroom $ IntMap.mapAccumWithKey (updateFunction peerData) [] prefixTable where
    swapNgroom (pfxs,pt) = (groomPrefixTable pt,pfxs)
    updateFunction = activeUpdateFunction
-- and the inner function has the shape: PeerData -> [IPrefix] -> Int -> PrefixTableEntry -> ([IPrefix], PrefixTableEntry)
-- the needed function uses the 'peerData' entry of the routes in the sorted list:
-- it deletes the entry corresponding to the target peer, if it exists
-- if the deleted entry is the previous best then it adds the corresponding prefix to the accumulator
-- the required (available) operaions in Data.SortedList are: uncons :: SortedList a -> Maybe (a, SortedList a) / filter :: (a -> Bool) -> SortedList a -> SortedList a
--
-- the required equality test is (\route -> peer == peerData route)
-- use uncons to extract and use if needed the case where the change has effect...
-- in the other case just use filter
    activeUpdateFunction peer prefixList prefix prefixTableEntry =
        if p top
        then (prefixList',tail)
        else (prefixList, SL.filter ( not . p ) prefixTableEntry)
        where
            Just (top,tail) = SL.uncons prefixTableEntry -- safe because the list cannot be null
                                                         -- however!!!! this can MAKE an empty list which we cannot delet in this operation
                                                         -- so we need a final preen before returning the Map to the RIB!!!!
            p route = peer == BGPData.peerData route
            prefixList' = IPrefix prefix : prefixList

groomPrefixTable :: PrefixTable -> PrefixTable
groomPrefixTable = IntMap.filter ( not . null )

withdrawPrefixTable :: PrefixTable -> IPrefix -> PeerData -> (PrefixTable,Bool)
withdrawPrefixTable pt (IPrefix ipfx) peer = (pt', wasBestRoute) where
    wasBestRoute = maybe
                         False -- This is the 'prefix not found' return value
                               -- there are really three possible outcomes, so a tri-valued resuklt could be used
                               -- a) route was found and removed, but was not the 'best' route
                               -- b) route was found and removed, and WAS the 'best' route
                               -- c) the route was not found, which could be a programming error
                               --    or an external issue
                         (\oldRouteList -> peerData (slHead oldRouteList) == peer )
                         maybeOldRouteList
    (maybeOldRouteList , pt') = IntMap.updateLookupWithKey tableUpdate ipfx pt
    -- 'tableUpdate' is the 'inner' fundtion which does the sortde list update and posts back the result,
    -- including the instruction to delete the entry...
    tableUpdate :: Int -> PrefixTableEntry -> Maybe PrefixTableEntry
    tableUpdate _ routes = let notPeer pd rd = pd /= peerData rd
                               routes' = SL.filter (notPeer peer) routes
                           in if null routes' then Nothing else Just routes'
    -- notPeer :: PeerData -> RouteData -> Bool
    -- notPeer pd rd = pd /= peerData rd 
    --oldBestRoute = slHead oldRouteList -- 'head' operation guaranteed safe as long as the precondition that empty prefix list are removed immediately...
    --wasBestRoute = peerData oldBestRoute == peer
    -- oldBestRoute = slHead oldRouteList -- 'head' operation guaranteed safe as long as the precondition that empty prefix list are removed immediately...
    -- wasBestRoute = peerData oldBestRoute == peer

withdraw :: PrefixTable -> [IPrefix] -> PeerData -> (PrefixTable,[IPrefix])
withdraw rib prefixes peer = Data.List.foldl' f (rib,[]) prefixes where
    f (pt,withdrawn) pfx = if p then (pt',pfx:withdrawn) else (pt',withdrawn) where
        (pt',p) = withdrawPrefixTable pt pfx peer
