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

import Data.IntMap.Strict(toList)
import qualified Data.List
import Data.IP

import BGPlib.BGPlib (XPrefix(..),Prefix(..),toPrefix)
import BGPRib.Common
import BGPRib.BGPData
import BGPRib.PrefixTable(PrefixTable)

-- ===================================================
--
-- some useful functions on prefix tables:
--
-- ===================================================

-- TODO for ADDPATH - fix these Show instances to be more usefull

getDB :: PrefixTable -> [(XPrefix,[RouteData])]
getDB pt = map f (toList pt) where
    f (pfx,routes) = (XPrefix 0 (toPrefix pfx), map snd routes)

lengthRIB :: PrefixTable -> Int
lengthRIB pt = length (toList pt)

-- TODO - should use 'bestPath' not head......
-- ADDPATH - NOTE - this logic and the consumer (addPeer) in Rib.hs need revisiting
-- under ADDPATH the correct behaviour is to send nothing since a new peer cannot have raised poisoned routes.....

getRIB :: PrefixTable -> [(RouteData,Prefix)]
getRIB _ = []

getFIB :: PrefixTable -> [(Prefix,IPv4)]
getFIB _ = []

getXRIB :: PrefixTable -> [(RouteData,XPrefix)]
getXRIB pt = map f (toList pt) where
    f (pfx,ptes) = (snd $ head ptes, XPrefix (fst $ head ptes) (toPrefix pfx))

getXFIB :: PrefixTable -> [(XPrefix,IPv4)]
getXFIB pt = map f (getXRIB pt) where
    f (route,pfx) = (pfx , nextHop route)

getAdjRIBOut :: PrefixTable -> [(RouteData,[Prefix])]
getAdjRIBOut = groupBy_ . getRIB

showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (getDB pt) where
    showPrefixTableItem (k,v) = show k ++ " [" ++ Data.List.intercalate " , " (showRoutes v) ++ "]"
    showRoutes = map (\route -> ( show.nextHop) route ++ " (" ++ (show.pathLength) route ++ ")" )

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute = showPrefixTableByRoute' show
showPrefixTableByRoute' fr pt = unlines $ map showRoute (getAdjRIBOut pt) where
    showRoute (r,pfxs) = unwords $  fr r : ":" : if length pfxs < 3 then map show pfxs else map show (take 2 pfxs) ++ ["..."]
