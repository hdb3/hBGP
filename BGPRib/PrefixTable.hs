{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module BGPRib.PrefixTable where

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

import BGPRib.BGPData
import BGPlib.BGPlib (Prefix, fromPrefix, toPrefix)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', (\\))
import qualified Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

trace _ = id

type PrefixTableEntry = [RouteData]

type PrefixTable = IntMap.IntMap PrefixTableEntry

instance {-# OVERLAPPING #-} Show PrefixTable where
  show = show . map (\(k, v) -> (toPrefix k, v)) . IntMap.toList

newPrefixTable :: PrefixTable
newPrefixTable = IntMap.empty

-- instance {-# OVERLAPPING #-} Show PrefixTable where
--   show = unlines . map showPTE . PT.ptList
--     where
--       showPTE (k, v) = show (toPrefix k, v)

type LocalMap = Map.Map (PeerData, RouteData) [Prefix]

updateC :: PrefixTable -> [Prefix] -> RouteData -> (PrefixTable, [(PeerData, RouteData, [Prefix])])
{-
    This function merges the output of multiple invocations of updatePrefixTable.
    The output is bundled for direct consumption by updateRibOutWithPeerData in Rib.hs.
    In many but not all cases there are 1:1 bundle:peers.
    Each invocation of updatePrefixTable effectively returns lists of (peer, prefix, route).
    The accumulator value is a map over (peer,route) containing a list of prefixes.
-}
updateC pt pfxs route = (pt', trace (show updates) updates)
  where
    sourcePeer = peerData route
    (pt', updateList) = foldl' kf (pt, []) pfxs
    kf :: (PrefixTable, [(PeerData, RouteData, Prefix)]) -> Prefix -> (PrefixTable, [(PeerData, RouteData, Prefix)])
    kf (pt, acc) pfx = let (pt', ax) = updatePrefixTable route pt pfx in (pt', acc ++ ax)
    --
    -- now consolidate individual rows so that subsequqnt update messages are packed
    updates = extract $ foldl' f Map.empty updateList where f m (peer, route, pfx) = insert m peer route pfx
    insert :: LocalMap -> PeerData -> RouteData -> Prefix -> LocalMap
    insert map peer route prefix = Map.alter f (peer, route) map where f pre = Just $ prefix : fromMaybe [] pre
    extract :: LocalMap -> [(PeerData, RouteData, [Prefix])]
    extract m = map (\((a, b), c) -> (a, b, c)) $ Map.toList m
    updatePrefixTable :: RouteData -> PrefixTable -> Prefix -> (PrefixTable, [(PeerData, RouteData, Prefix)])
    {-
      This function processes a single prefix.
      The input may be withdraw or update - the outcome is calculated identically after the different initial treatment based on whether
      the input is Update and thus carries a new route which must be added to the list after any existing route for the peer is removed.
      In either case the first action is to discard an existing route.
    -}
    updatePrefixTable route pt pfx = (pt', rval)
      where
        sourcePeer = peerData route
        -- NB - this function assumes that the list is sorted on entry!

        oldList = fromMaybe [] $ IntMap.lookup (fromPrefix pfx) pt
        -- now remove any existing route from this peer from the table
        -- delete strategy uses the route origin as the basis for equality - in base case this is the peer, in ADDPATH it is (peer,PathID)
        tmpList = filter ((sourcePeer /=) . peerData) oldList
        -- now add in the new route, if present
        newList = updatePT tmpList route
          where
            updatePT l r@RouteData {} = Data.List.sort (r : l)
            updatePT l _ = l
        -- insert the new PT, replacing if any the old PT for this prefix
        pt' = IntMap.insert (fromPrefix pfx) newList pt
        -- LOCRIB update complete, now calculate the impact....
        -- this section is BGPPROTECTION specific
        -- however the mechanism is backportable to the general case, by changing the target peer and best route selection strategy
        -- note that the context here includes both old and new locrib state, which is required in all cases for calculating the target peer set
        -- unless/until the old per peer adjRIBout status is persistsed elsewhere

        -- TODO  - make this an IO function and perform adjRIBout push directly rather than using the return values for this purpose

        (initialRemediated, initialOther) = Data.List.span poisoned oldList
        (finalRemediable, finalOther) = Data.List.span poisoned newList
        newBestRoute = head finalOther -- partial result guarded by use only when update list is not empty
        (withdraw, update) = case (null initialRemediated || null initialOther, null finalRemediable || null finalOther) of
          (True, True) -> ([], [])
          (True, False) -> ([], finalRemediable)
          (False, True) -> (initialRemediated, [])
          (False, False) -> (initialRemediated \\ finalRemediable, finalRemediable)
        (withdrawTargets, updateTargets) = (map peerData withdraw, map peerData update)
        traceData =
          "\n(initialRemediated,initialOther )"
            ++ show (initialRemediated, initialOther)
            ++ "\n(finalRemediable,finalOther)     "
            ++ show (finalRemediable, finalOther)
            ++ "\n(withdrawTargets, updateTargets) "
            ++ show (withdrawTargets, updateTargets)
        rval = trace traceData (map (,NullRoute,pfx) withdrawTargets ++ map (,newBestRoute,pfx) updateTargets)

-- this function returns the best route for a specific prefix
queryPrefixTableC :: PrefixTable -> Prefix -> RouteData
queryPrefixTableC table pfx =
  let rawRouteList = fromMaybe [] $ IntMap.lookup (fromPrefix pfx) table
      safeHead ax = if null ax then NullRoute else head ax
   in safeHead $ Data.List.dropWhile poisoned rawRouteList

showRibAtC :: PrefixTable -> Prefix -> String
showRibAtC table pfx = show (IntMap.lookup (fromPrefix pfx) table)

withdrawPeerC :: PrefixTable -> PeerData -> (PrefixTable, [Prefix])
-- core function is mapAccumWithKey
-- which acts on the prefix table, returning a modified prefix table and also the list of prefixes which have been modifed
-- in a way which REQUIRES an update
withdrawPeerC prefixTable peerData = swapNgroom $ IntMap.mapAccumWithKey (activeUpdateFunction peerData) [] prefixTable
  where
    -- swapNgroom (pfxs, pt) = (groomPrefixTable pt, pfxs)
    -- updateFunction = activeUpdateFunction

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

    groomPrefixTable :: PrefixTable -> PrefixTable
    groomPrefixTable = IntMap.filter (not . null)
    activeUpdateFunction :: PeerData -> [Prefix] -> Int -> [RouteData] -> ([Prefix], [RouteData])
    activeUpdateFunction peer prefixList prefix prefixTableEntry =
      let Just (top, tail) = Data.List.uncons prefixTableEntry
          p = (peer /=) . BGPRib.BGPData.peerData
          prefixList' = toPrefix prefix : prefixList
       in if p top then (prefixList, filter p prefixTableEntry) else (prefixList', tail)
    swapNgroom :: ([Prefix], PrefixTable) -> (PrefixTable, [Prefix])
    swapNgroom (pfxs, pt) = (groomPrefixTable pt, pfxs)

-- -- ADDPATH changes
-- -- this function must return full prefixes (inclusidnng pathID)
-- -- because it determines which withdraws must be generated after delPeer
-- -- this reasoning may be questioned if the subsequent function call did noot simply use 'update' to do its work....

getPeerPrefixes :: PrefixTable -> PeerData -> [Prefix]
getPeerPrefixes pt peer = IntMap.foldlWithKey' f [] pt
  where
    -- base code - assumes that only one prefix can be returned per prefix table entry....
    f acc key val = if p val then toPrefix key : acc else acc
    p = Data.List.any p'
    p' = (peer ==) . peerData

-- newPrefixTable :: PrefixTable
-- newPrefixTable = PT.ptNew

-- update :: PrefixTable -> [Prefix] -> RouteData -> (PrefixTable, [(Prefix, RouteData)])
-- update pt pfxs route = Data.List.foldl' f (pt, []) pfxs
--   where
--     f (pt', acc) pfx = (pt'', acc')
--       where
--         acc' = if PT.pteBest new == PT.pteBest old then acc else (pfx, PT.pteBest new) : acc
--         (old, new, pt'') = PT.ptUpdate (fromPrefix pfx) route pt

-- queryPrefixTable :: PrefixTable -> Prefix -> RouteData
-- queryPrefixTable table pfx = PT.pteBest $ PT.ptQuery (fromPrefix pfx) table

-- showRibAt :: PrefixTable -> Prefix -> String
-- showRibAt table pfx = show (PT.ptQuery (fromPrefix pfx) table)

-- withdraw :: PrefixTable -> [Prefix] -> PeerData -> (PrefixTable, [(Prefix, RouteData)])
-- withdraw pt pfxs pd = update pt pfxs (Withdraw pd)

-- withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable, [(Prefix, RouteData)])
-- withdrawPeer pt = withdraw pt (map toPrefix $ PT.ptKeys pt)
