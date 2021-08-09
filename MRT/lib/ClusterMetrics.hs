module ClusterMetrics where

import qualified Data.IntMap.Strict as Map
import Data.List (uncons)
import Data.Maybe (mapMaybe)
import GroupRIBReport
import MRTrib

fromRouteMapv4 :: RouteMapv4 -> [[IP4PrefixHash]]
fromRouteMapv4 = map (map v4hash) . Map.elems . Map.map snd

simpleCompareRouteMaps :: [RouteMapv4] -> String
simpleCompareRouteMaps = simpleGroupRIBReport . concatMap fromRouteMapv4

compareRouteMaps :: [RouteMapv4] -> String
compareRouteMaps = groupRIBReport . concatMap fromRouteMapv4
  where
    --compareRouteMaps = groupRIBReport . interlace . map fromRouteMapv4
    --compareRouteMaps = groupRIBReport . take 100 . interlace . map fromRouteMapv4

    interlace :: [[a]] -> [a]
    interlace [] = []
    interlace ax = heads ++ interlace tails
      where
        (heads, tails) = unzip $ mapMaybe uncons ax

compareRouteMapv4 :: RouteMapv4 -> RouteMapv4 -> String
compareRouteMapv4 a b = compareRouteMaps [a, b]
