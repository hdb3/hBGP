{-# LANGUAGE FlexibleInstances #-}
module BGPRib.PrefixTable(PrefixTable,update,newPrefixTable,queryPrefixTable,getPeerPrefixes,showRibAt) where

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
import qualified Data.Map.Strict as Map
import Data.List ((\\),foldl')
import qualified Data.List
import Data.Maybe(fromMaybe) 

import BGPRib.BGPData
import BGPlib.BGPlib (Prefix,toPrefix,fromPrefix)

type PrefixTableEntry = [RouteData]
type PrefixTable = IntMap.IntMap PrefixTableEntry

instance {-# OVERLAPPING #-} Show PrefixTable where
    show = show . IntMap.toList

newPrefixTable :: PrefixTable
newPrefixTable = IntMap.empty

update:: PrefixTable -> [Prefix] -> PeerData -> Maybe RouteData -> (PrefixTable, [(PeerData, Int, [Prefix])])
update pt pfxs sourcePeer routeM = (pt,updates) where

    (pt', updateList) = foldl' kf (pt,[]) pfxs 
    kf :: (PrefixTable, [(PeerData,Int,Prefix)]) -> Prefix -> (PrefixTable, [(PeerData,Int, Prefix)])
    kf (pt,acc) pfx = let (pt',ax) = updatePrefixTable sourcePeer routeM pt pfx in (pt', acc ++ ax)

    updates = extract map
    map = foldl' f Map.empty updateList where f m (peer,route,pfx) = insert m peer route pfx

type LocalMap = Map.Map (PeerData, Int) [Prefix]

insert :: LocalMap -> PeerData -> Int -> Prefix -> LocalMap
insert map peer route prefix = Map.alter f (peer,route) map where f pre = Just $ prefix : (fromMaybe [] pre)
-- inserts :: LocalMap -> [(PeerData, Int)] -> Prefix -> LocalMap
-- inserts lm ax pfx = foldl' (\m (a,b) -> insert m a b pfx) lm ax

extract :: LocalMap -> [ (PeerData, Int, [Prefix]) ]
extract m = map ( \((a,b),c) -> (a,b,c) ) $ Map.toList m

{-
    This function merges the output of multiple invocations of updatePrefixTable.
    The output is bundled for direct consumption by updateRibOutWithPeerData in Rib.hs.
    In many but not all cases there are 1:1 bundle:peers.
    Each invocation of updatePrefixTable effectively returns lists of (peer, prefix, route).
    The accumulator value is a map over (peer,route) containing a list of prefixes.
-}

updatePrefixTable :: PeerData -> Maybe RouteData -> PrefixTable -> Prefix -> (PrefixTable,[(PeerData,Int,Prefix)])
updatePrefixTable sourcePeer routeM pt pfx = (pt', rval) where
    -- NB - this function assumes that the list is sorted on entry!
    oldList = fromMaybe [] $ IntMap.lookup (fromPrefix pfx) pt
    
    -- delete strategy uses the route origin as the basis for equality - in base case this is the peer, in ADDPATH it is (peer,PathID) 
    tmpList = filter p oldList where p r = peerData r == sourcePeer

    newList = maybe tmpList (\route -> Data.List.sort $ route : tmpList) routeM
        
    pt' = IntMap.insert (fromPrefix pfx) newList pt

    -- LOCRIB update complete, now calculate the impact....
    -- this section is BGPPROTECTION specific
    -- however the mechanism is backportable to the general case, by changing the target peer and best route selection strategy
    -- note that the context here includes both old and new locrib state, which is required in all cases for calculating the target peer set
    -- unless/until the old per peer adjRIBout status is persistsed elsewhere

    -- TODO  - make this an IO function and perform adjRIBout push directly rather than using the return values for this purpose

    (oldPoisoned,oldUnpoisoned) = Data.List.span poisoned oldList
    (newPoisoned,newUnpoisoned) = Data.List.span poisoned newList
    safeHeadId ax = if null ax then 0 else routeId (head ax)
    oldBest = safeHeadId oldUnpoisoned
    newBest = safeHeadId newUnpoisoned
    oldPoisonedPeers = map peerData oldPoisoned
    newPoisonedPeers = map peerData newPoisoned
    (withdrawTargets, updateTargets) = if (oldBest == newBest) then (oldPoisonedPeers \\ newPoisonedPeers, newPoisonedPeers \\ oldPoisonedPeers)
                                                               else (oldPoisonedPeers \\ newPoisonedPeers, newPoisonedPeers)
    rval = map (\x -> (x,0,pfx)) withdrawTargets ++ map (\x -> (x,newBest,pfx)) updateTargets 
    

-- this function returns the best route for a specific prefix
queryPrefixTable :: PrefixTable -> Prefix -> Maybe RouteData
queryPrefixTable table pfx = fmap head (IntMap.lookup (fromPrefix pfx) table)

showRibAt :: PrefixTable -> Prefix -> String
showRibAt table pfx = show (IntMap.lookup (fromPrefix pfx) table)

withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[Prefix])
-- core function is mapAccumWithKey
-- which acts on the prefix table, returning a modified prefix table and also the list of prefixes which have been modifed
-- in a way which REQUIRES an update
withdrawPeer prefixTable peerData = swapNgroom $ IntMap.mapAccumWithKey (updateFunction peerData) [] prefixTable where
    swapNgroom (pfxs,pt) = (groomPrefixTable pt,pfxs)
    updateFunction = activeUpdateFunction
-- the inner function has the shape: PeerData -> [Prefix] -> Int -> PrefixTableEntry -> ([Prefix], PrefixTableEntry)
-- and uses the 'peerData' entry of the routes in the sorted list:
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
        else (prefixList, filter ( not . p ) prefixTableEntry)
        where
            Just (top,tail) = Data.List.uncons prefixTableEntry -- safe because the list cannot be null
                                                         -- however!!!! this can MAKE an empty list which we cannot delet in this operation
                                                         -- so we need a final preen before returning the Map to the RIB!!!!
            p route = peer == BGPRib.BGPData.peerData route
            prefixList' = toPrefix prefix : prefixList

groomPrefixTable :: PrefixTable -> PrefixTable
groomPrefixTable = IntMap.filter ( not . null )

getPeerPrefixes :: PrefixTable -> PeerData -> [Prefix]
getPeerPrefixes pt peer = IntMap.foldlWithKey' f [] pt where 
    f acc key val = if p val then (toPrefix key:acc) else acc
    p = Data.List.any p'
    p' = ( peer == ) . peerData

{-
  the function requires collection of just those keys for which the associated value matches the function subject
  this is a fold using the predicate p over the value field:
  IntMap.foldlWithKey' f [] map

  where f acc key val = if p val then (k:acc) else acc
  and in this case p val is actually Data.List.any p', where p' v' = peer == peerData v'   
-}

withdrawPrefixTable :: PrefixTable -> Prefix -> PeerData -> (PrefixTable,Bool)
withdrawPrefixTable pt pfx peer = (pt', wasBestRoute) where
    wasBestRoute = maybe
                         False -- This is the 'prefix not found' return value
                               -- there are really three possible outcomes, so a tri-valued resuklt could be used
                               -- a) route was found and removed, but was not the 'best' route
                               -- b) route was found and removed, and WAS the 'best' route
                               -- c) the route was not found, which could be a programming error
                               --    or an external issue
                         (\oldRouteList -> peerData (head oldRouteList) == peer )
                         maybeOldRouteList
    (maybeOldRouteList , pt') = IntMap.updateLookupWithKey tableUpdate (fromPrefix pfx) pt
    tableUpdate :: Int -> PrefixTableEntry -> Maybe PrefixTableEntry
    tableUpdate _ routes = let notPeer pd rd = pd /= peerData rd
                               routes' = filter (notPeer peer) routes
                           in if null routes' then Nothing else Just routes'

withdraw :: PrefixTable -> [Prefix] -> PeerData -> (PrefixTable,[Prefix])
withdraw rib prefixes peer = Data.List.foldl' f (rib,[]) prefixes where
    f (pt,withdrawn) pfx = if p then (pt',pfx:withdrawn) else (pt',withdrawn) where
        (pt',p) = withdrawPrefixTable pt pfx peer