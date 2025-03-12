module BGPRib.PrefixTableUtils where

import BGPRib.BGPData
import BGPRib.Common
import qualified BGPRib.PTE as PTE
import BGPRib.PrefixTable (PrefixTable, ptList)
import BGPlib.BGPlib (Prefix, toPrefix)
import Data.IP
import qualified Data.List

-- ===================================================
--
-- some useful functions on prefix tables:
--
-- ===================================================

getDB :: PrefixTable -> [(Prefix, [RouteData])]
getDB pt = map f (ptList pt)
  where
    f (pfx, routes) = (toPrefix pfx, routes)

lengthRIB :: PrefixTable -> Int
lengthRIB pt = length (ptList pt)

getRIB :: PrefixTable -> [(RouteData, Prefix)]
getRIB pt = f (ptList pt)
  where
    f [] = []
    f ((k, pte) : ax)
      | PTE.pteNull pte = f ax
      | otherwise = (PTE.pteBest pte, toPrefix k) : f ax

getFIB :: PrefixTable -> [(Prefix, IPv4)]
getFIB pt = map f (getRIB pt)
  where
    f (route, pfx) = (pfx, nextHop route)

getAdjRIBOut :: PrefixTable -> [(RouteData, [Prefix])]
getAdjRIBOut = groupBy_ . getRIB

showPrefixTable :: PrefixTable -> String
showPrefixTable pt = unlines $ map showPrefixTableItem (getDB pt)
  where
    showPrefixTableItem (k, v) = show k ++ " [" ++ Data.List.intercalate " , " (showRoutes v) ++ "]"
    showRoutes = map (\route -> (show . nextHop) route ++ " (" ++ (show . pathLength) route ++ ")")

showPrefixTableByRoute :: PrefixTable -> String
showPrefixTableByRoute = showPrefixTableByRoute' show

showPrefixTableByRoute' fr pt = unlines $ map showRoute (getAdjRIBOut pt)
  where
    showRoute (r, pfxs) = unwords $ fr r : ":" : if length pfxs < 3 then map show pfxs else map show (take 2 pfxs) ++ ["..."]
