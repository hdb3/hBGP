{-# LANGUAGE FlexibleInstances #-}
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

import qualified Data.IntMap.Strict as IntMap
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

ptHead (a:_) = a

update:: PrefixTable -> [Prefix] -> RouteData -> (PrefixTable,[Prefix])
update pt pfxs route = Data.List.foldl' f (pt,[]) pfxs where
    f (pt_,updated) pfx = if p then (pt__,pfx:updated) else (pt__,updated) where
        (pt__,p) = updatePrefixTable pt_ pfx route

{-
  'insert' a new route in the prefix table.
  Return the resulting new prefix table and a flag to show if the best route changed as a result of the operation.
  At the top level is an update operation on a single member of a container indexed by the Prefix input parameter.

  There is a requirement to 'smuggle out' from the update a side effect outcome of the inner operation.
  The simple method adopted here is to separate the two operations and accept that an optimal solution requires a different approach - 
  whilst hoping that cache behaviour will render the optimisation unneeded.

  The inner function defines the RIB semantics.
  It is a home grown sorted list solution - insertion requires list traversal until the candidate is inserted and an older counterpart removed if present.
        - traversal shortcircuits only when both criteria are met.
        - the algorithm assumes that the list is already ordered
        The incremental operation has two phases - in the first phase the new element has not been inserted or matched/replaced an old sibling
                                                 - there are two forms of second phase, depending on whether mtach/replacement or insertion happens first
  -}
updatePrefixTable :: PrefixTable -> Prefix -> RouteData -> (PrefixTable,Bool)
updatePrefixTable pt pfx rd = (pt', bestChanged) where
    k =  fromPrefix pfx
    pt' = IntMap.alter ( pteUpdate rd ) k pt
    best = fmap (take 1) . IntMap.lookup k
    newBest = best pt'
    oldBest = fmap (take 1) ( IntMap.lookup k pt)
    bestChanged = oldBest /= newBest

    pteUpdate :: RouteData -> Maybe PrefixTableEntry -> Maybe PrefixTableEntry
    pteUpdate rd = Just . maybe [rd] (pteInsert rd ) where    
        pteInsert :: RouteData -> PrefixTableEntry -> PrefixTableEntry

        pteInsert v = f where
            match rd1 rd2 = peerData rd1 == peerData rd2
            f [] = [v]
            f (a:ax) | match v a = f' ax -- remove sibling
                     | v > a = v : f'' (a:ax)
                     | otherwise = a : f ax
            -- simple ordered insertion without duplicate removal
            f' [] = [v]
            f' (a:ax) = if v > a then v : a : ax else a : f' ax
            -- one-shot duplicate removal - no need to ever insert
            f'' [] = []
            f'' (a:ax) = if match v a then ax else a : f'' ax


-- this function finds the best route for a specicif prefix
-- if the requirement is bulk look up then another function might be better.....

queryPrefixTable :: PrefixTable -> Prefix -> RouteData
queryPrefixTable table pfx = maybe NullRoute ptHead (IntMap.lookup (fromPrefix pfx) table)

showRibAt :: PrefixTable -> Prefix -> String
showRibAt table pfx = show (IntMap.lookup (fromPrefix pfx) table)

{-
  Withdraw Operations

  'withdraw' is a wrapper around 'withdrawPrefixTable'.
  'withdrawPrefixTable' updates the prefixTable Map for a specific prefix
  Within 'withdrawPrefixTable' is an inner function which manipulates the route list for the prefix.
  The manipulation removes the given route, identified by the peer which originated it.
  In the case that the withdrawn route was the 'best' route then a Bool return flag is set to allow the calling context
  to send updates replacing the route which has been withdrawn.

  TODO

  Withdraw semantics require the withdrawn route to be known, because ideally 'withdraw' should only be sent to peers
  which have received the corresponding route (e.g., dont' send a withdraw to a peer which originated a route..)
  As of now, withdraw only supports bare prefixes.
-}

withdraw :: PrefixTable -> [Prefix] -> PeerData -> (PrefixTable,[Prefix])

-- withdraw acts on the prefix table, returning a modified prefix table and also the list of prefixes which have been modifed
-- in a way which REQUIRES an update
-- An update is required when the current best route in a PrefixTableEntry matches the withdrawing peer
-- As with insert, the inner function is implmented as a separate lookup followed by an inner delete.
-- Lookup should not fail, but if it does then the subsequent delete is clearly unproductive!
-- The inner function returns Bool to show if 'best' changed

-- note - this implementation does not remove empty entries in the prefix table
--      - but that is a choice - the alternative is to use IntMap.update rather than IntMap.adjust

withdraw pt pfxs pd = Data.List.foldl' f (pt,[]) pfxs where
    f (pt0,acc) pfx = let (pt0',p) = ptWithdraw pfx pt0 in if p then (pt0',pfx:acc) else (pt0,acc)

    ptWithdraw :: Prefix -> PrefixTable -> (PrefixTable,Bool)
    ptWithdraw pfx_ pt = (pt',changeFlag) where
        pfx = fromPrefix pfx_
        oldPTE = fromMaybe [] ( IntMap.lookup pfx pt ) -- lookup fail or empty slot are equivalent...
        changeFlag = not (null oldPTE) && (pd == peerData (head oldPTE))
        pt' = IntMap.adjust pteWithdraw pfx pt

        pteWithdraw [] = []
        pteWithdraw (a:ax) | (pd == peerData a) = ax
                           | otherwise = a : pteWithdraw ax

withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[Prefix])
withdrawPeer pt = withdraw pt (map toPrefix $ IntMap.keys pt)