module PrefixTableUtils where

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

import Data.IntMap.Strict(toList)
import qualified Data.SortedList as SL -- package sorted-list
import qualified Data.List
import Data.IP

import Common
import BGPData
import BGPlib (IPrefix(..), toPrefix, Prefix)
import PrefixTable(PrefixTable,slHead)

-- ===================================================
--
-- some useful functions on prefix tables:
--
-- ===================================================

getDB :: PrefixTable -> [(IPrefix,[RouteData])]
getDB pt = map f (toList pt) where
    f (pfx,routes) = (IPrefix pfx,SL.fromSortedList routes)

getRIB :: PrefixTable -> [(RouteData,Prefix)]
getRIB = map (\(a,b) -> (a,toPrefix b)) . getRIB'

getRIB' :: PrefixTable -> [(RouteData,IPrefix)]
getRIB' pt = map f (toList pt) where
    f (pfx,routes) = (slHead routes , IPrefix pfx)

getFIB :: PrefixTable -> [(Prefix,IPv4)]
getFIB pt = map f (getRIB pt) where
    f (route,pfx) = (pfx , nextHop route)

getAdjRIBOut :: PrefixTable -> [(RouteData,[IPrefix])]
getAdjRIBOut = groupBy_ . getRIB'

showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (getDB pt) where
    showPrefixTableItem (k,v) = show k ++ " [" ++ Data.List.intercalate " , " (showRoutes v) ++ "]"
    showRoutes = map (\route -> ( show.nextHop) route ++ " (" ++ (show.pathLength) route ++ ")" ) 

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute = showPrefixTableByRoute' show 
showPrefixTableByRoute' fr pt = unlines $ map showRoute (getAdjRIBOut pt) where
    showRoute (r,pfxs) = unwords $  fr r : ":" : if length pfxs < 3 then map show pfxs else map show (take 2 pfxs) ++ ["..."]
