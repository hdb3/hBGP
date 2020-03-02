{-# LANGUAGE FlexibleInstances,TupleSections #-}
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

 - ADDPATH extension notes
 - Under ADDPATH the received routes are indexed by (Prefix,PathID) a.k.a. XPrefix
 - The prefux table meanwhile is indexed solely on Prefix, whilst the table entries are augmented from containers of routes to containers of 
 - (Route,PathID) (no current type alias).
 - Incoming units retain (Prefix,PathID) association until the transfer into (Route,PathID) which occurs in the function
 - updatePrefixTable.  The output signature of this function is (PrefixTable,[(PeerData,Int,Prefix)]),
 -  which contains the type '[(PeerData,Int, Prefix)]', which is the interface type used to populate and extract from AdjRIBOut,
 - and PrefixTable, which binds PathID.  In this implementation AdjRIBOut is PathID agnostic.
 - Thus the output type binds PathID only in the PrefixTable, and any other Prefix references are just that (not XPrefix/NLRI) 
-}

import qualified Debug.Trace
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List ((\\),foldl')
import qualified Data.List
import Data.Maybe(fromMaybe)
import Data.Word(Word32) 

import BGPRib.BGPData
import BGPlib.BGPlib (XPrefix(..),Prefix(..),toPrefix,fromPrefix,pathID)

trace _ = id
-- trace = Debug.Trace.trace

type PrefixTableEntry = [(Word32,RouteData)]
type PrefixTable = IntMap.IntMap PrefixTableEntry

instance {-# OVERLAPPING #-} Show PrefixTable where
    show = show . map (\(k,v) -> (toPrefix k,v)) . IntMap.toList

newPrefixTable :: PrefixTable
newPrefixTable = IntMap.empty

update:: PrefixTable -> [XPrefix] -> PeerData -> Maybe RouteData -> (PrefixTable, [(PeerData, Int, [Prefix])])
update pt pfxs sourcePeer routeM = (pt', trace (show updates) updates) where

    (pt', updateList) = foldl' kf (pt,[]) pfxs 
    kf :: (PrefixTable, [(PeerData,Int,Prefix)]) -> XPrefix -> (PrefixTable, [(PeerData,Int, Prefix)])
    kf (pt,acc) pfx = let (pt',ax) = updatePrefixTable sourcePeer routeM pt pfx in (pt', acc ++ ax)

    updates = extract map
    map = foldl' f Map.empty updateList where f m (peer,route,pfx) = insert m peer route pfx

type LocalMap = Map.Map (PeerData, Int) [Prefix]

insert :: LocalMap -> PeerData -> Int -> Prefix -> LocalMap
insert map peer route prefix = Map.alter f (peer,route) map where f pre = Just $ prefix : fromMaybe [] pre

extract :: LocalMap -> [ (PeerData, Int, [Prefix]) ]
extract m = map ( \((a,b),c) -> (a,b,c) ) $ Map.toList m

{-
    This function merges the output of multiple invocations of updatePrefixTable.
    The output is bundled for direct consumption by updateRibOutWithPeerData in Rib.hs.
    In many but not all cases there are 1:1 bundle:peers.
    Each invocation of updatePrefixTable effectively returns lists of (peer, prefix, route).
    The accumulator value is a map over (peer,route) containing a list of prefixes.
-}

updatePrefixTable :: PeerData -> Maybe RouteData -> PrefixTable -> XPrefix -> (PrefixTable,[(PeerData,Int,Prefix)])
updatePrefixTable sourcePeer routeM pt (XPrefix pathID prefix) = (pt', rval) where
    -- NB - this function assumes that the list is sorted on entry!
    oldList = fromMaybe [] $ IntMap.lookup (fromPrefix $ prefix ) pt
    
    -- delete strategy uses the route origin as the basis for equality - in base case this is the peer, in ADDPATH it is (peer,PathID) 
    tmpList = filter p oldList where p (id,r) = not (peerData r == sourcePeer && id == pathID )

    newList = maybe tmpList (\route -> Data.List.sortOn snd $ (pathID ,route) : tmpList) routeM
        
    pt' = IntMap.insert (fromPrefix prefix) newList pt

    -- LOCRIB update complete, now calculate the impact....
    -- this section is BGPPROTECTION specific
    -- however the mechanism is backportable to the general case, by changing the target peer and best route selection strategy
    -- note that the context here includes both old and new locrib state, which is required in all cases for calculating the target peer set
    -- unless/until the old per peer adjRIBout status is persistsed elsewhere

    -- TODO  - make this an IO function and perform adjRIBout push directly rather than using the return values for this purpose

    (initialRemediated, initialOther) = Data.List.span (poisoned.snd) oldList
    (finalRemediable, finalOther) = Data.List.span (poisoned.snd) newList
    newBestRoute = (routeId . snd . head) finalOther  -- partial result guarded by use only when update list is not empty
    (withdraw,update) = case (null initialRemediated || null initialOther, null finalRemediable || null finalOther) of
        (True,True) -> ([],[])
        (True,False) -> ([],finalRemediable)
        (False,True) -> (initialRemediated,[])
        (False,False) -> (initialRemediated \\ finalRemediable,finalRemediable)

    (withdrawTargets, updateTargets) = (map (peerData.snd) withdraw, map (peerData.snd) update)

    traceData =    "\n(initialRemediated,initialOther )" ++ show (initialRemediated,initialOther) 
                ++ "\n(finalRemediable,finalOther)     " ++ show (finalRemediable,finalOther)
                ++ "\n(withdrawTargets, updateTargets) " ++ show (withdrawTargets, updateTargets)                                                   
                                                 
    rval = trace traceData ( map (,0,prefix) withdrawTargets ++ map  (,newBestRoute,prefix) updateTargets)
    


-- this function returns the best route for a specific prefix
queryPrefixTable :: PrefixTable -> Prefix -> Maybe RouteData
queryPrefixTable table pfx = let
    rawRouteList = fromMaybe [] $ IntMap.lookup (fromPrefix pfx) table
    safeHead ax = if null ax then Nothing else Just $ snd (head ax)
    in (safeHead . Data.List.dropWhile (poisoned.snd)) rawRouteList

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
--
-- ADDPATH CHANGES
-- the trivial change accomdates the extended PrefixTableEntry structure blidnly
-- HOWEVER
-- This does not address the problem that the requirement to send updates is different now, i.e. thsat it is only unpoisoned routes which should be considered.
-- Also to be considered is that there may be multiple routes from a single peer, all of which must be removed..
-- THEREFORE there remains a TODO requirement witout which the peer withdrawal will not cause the right action in the presence of poisoned routes.
-- *** TODO *** !!!
    activeUpdateFunction peer prefixList prefix prefixTableEntry =
        if p top
        then (prefixList',tail)
        else (prefixList, filter ( not . p ) prefixTableEntry)
        where
            Just (top,tail) = Data.List.uncons prefixTableEntry -- safe because the list cannot be null
                                                         -- however!!!! this can MAKE an empty list which we cannot delet in this operation
                                                         -- so we need a final preen before returning the Map to the RIB!!!!
            p (_,route) = peer == BGPRib.BGPData.peerData route
            prefixList' = toPrefix prefix : prefixList

groomPrefixTable :: PrefixTable -> PrefixTable
groomPrefixTable = IntMap.filter ( not . null )

-- ADDPATH changes
-- this function must return full prefixes (inclusidnng pathID)
-- because it determines which withdraws must be generated after delPeer
-- this reasoning may be questioned if the subsequent function call did noot simply use 'update' to do its work....

getPeerPrefixes :: PrefixTable -> PeerData -> [XPrefix]
getPeerPrefixes pt peer = IntMap.foldlWithKey' f [] pt where 
    f acc key val = prefixes ++ acc where
    -- 'val' is [PrefixTableEntry], which is [(PathID,RouteData)], key is proxy Prefix
    -- we need the list [Prefix] where Prefix is composed as Prefix pathID key
        prefixes = map (\(x,_) -> XPrefix x (toPrefix key) ) ( filter p val) 
        p = ( peer == ) . peerData . snd
