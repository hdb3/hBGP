module BGPRib.PrefixTableUtils where

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

--import Data.IntMap.Strict(toList)

import BGPRib.BGPData
import BGPRib.Common
import qualified BGPRib.PT as PT
import BGPRib.PrefixTable (PrefixTable)
import BGPlib.BGPlib (Prefix, toPrefix)
import qualified Data.List

-- ===================================================
--
-- some useful functions on prefix tables:
--
-- ===================================================

getDB :: PrefixTable -> [(Prefix, [RouteData])]
getDB pt = map f (PT.ptList pt)
  where
    f (pfx, routes) = (toPrefix pfx, routes)

lengthRIB :: PrefixTable -> Int
lengthRIB pt = length (PT.ptList pt)

getRIB :: PrefixTable -> [(RouteData, Prefix)]
getRIB pt = f (PT.ptList pt)
  where
    f [] = []
    f ((k, pte) : ax)
      | PT.pteNull pte = f ax
      | otherwise = (PT.pteBest pte, toPrefix k) : f ax

-- f (pfx,routes) = (fromJust $ PT.pteBest routes, toPrefix pfx)

getFIB :: PrefixTable -> [(Prefix, IPv4)]
getFIB pt = map f (getRIB pt)
  where
    f (route, pfx) = (pfx, nextHop (path route))

getAdjRIBOut :: PrefixTable -> [(RouteData, [Prefix])]
getAdjRIBOut = groupBy_ . getRIB

showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (getDB pt)
  where
    showPrefixTableItem (k, v) = show k ++ " [" ++ Data.List.intercalate " , " (showRoutes v) ++ "]"
    showRoutes = map (\route -> (show . nextHop . path) route ++ " (" ++ (show . pathLength . path) route ++ ")")

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute = showPrefixTableByRoute' show

showPrefixTableByRoute' fr pt = unlines $ map showRoute (getAdjRIBOut pt)
  where
    showRoute (r, pfxs) = unwords $ fr r : ":" : if length pfxs < 3 then map show pfxs else map show (take 2 pfxs) ++ ["..."]
