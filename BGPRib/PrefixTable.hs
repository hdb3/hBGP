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

import qualified Data.List
import BGPRib.BGPData
import BGPlib.BGPlib (Prefix,toPrefix,fromPrefix)
import qualified BGPRib.PT as PT

type PrefixTableEntry = PT.PTE
type PrefixTable = PT.PT

instance {-# OVERLAPPING #-} Show PrefixTable where
    show = unlines . map showPTE . PT.ptList where
        showPTE (k,v) = show (toPrefix k,v)


newPrefixTable :: PrefixTable
newPrefixTable = PT.ptNew

update:: PrefixTable -> [Prefix] -> RouteData -> (PrefixTable,[(Prefix,RouteData)])
update pt pfxs route = Data.List.foldl' f (pt,[]) pfxs where
    f (pt',acc) pfx = (pt'',acc') where
        acc' = if PT.pteBest new == PT.pteBest old then acc else (pfx,PT.pteBest new):acc
        (old,new,pt'') = PT.ptUpdate (fromPrefix pfx) route pt'

queryPrefixTable :: PrefixTable -> Prefix -> RouteData
queryPrefixTable table pfx = PT.pteBest $ PT.ptQuery (fromPrefix pfx) table

showRibAt :: PrefixTable -> Prefix -> String
showRibAt table pfx = show (PT.ptQuery (fromPrefix pfx) table)

-- TODO merge update and withdraw by using a route value of Withdraw {..}
withdraw :: PrefixTable -> [Prefix] -> PeerData -> (PrefixTable,[(Prefix,RouteData)])
withdraw pt pfxs pd = update pt pfxs (Withdraw pd)
-- withdraw pt pfxs pd = Data.List.foldl' f (pt,[]) pfxs where
--     f (pt',acc) pfx = let acc' = if (PT.pteBest new) == (PT.pteBest old) then acc else (pfx,PT.pteBest new):acc
--                           (old,new,pt'') = PT.ptUpdate (fromPrefix pfx) (Withdraw pd) pt'
--                       in (pt'',acc')

withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[(Prefix,RouteData)])
withdrawPeer pt = withdraw pt (map toPrefix $ PT.ptKeys pt)