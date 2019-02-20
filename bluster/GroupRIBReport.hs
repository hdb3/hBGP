module GroupRIBReport where

import Core
import Operations
import Consistency
import Analysis

colour = False

simpleGroupRIBReport :: [[Int]] -> String
simpleGroupRIBReport pfxs =
    "simpleGroupRIBReport"
    ++ "\ninput prefix groups: " ++ show (length pfxs)
    -- ++ "\n" ++ ( maybe (green "Consistency check pass") (\s -> red $ "Consistency check fail: " ++ s) (consistency st) )
    -- couls leave colur in if writing to stderr
    ++ "\n" ++ maybe (basicAnalysis st) (\s -> "Consistency check fail: " ++ s) (consistency st)
    ++ "\nGroupRIBReport done"
    where
        st = mapRu newState pfxs
        mapRu = foldl (flip ru)
        ru pl = ribUpdate (map Prefix pl)

groupRIBReport :: [[Int]] -> String
groupRIBReport pfxs =
    "groupRIBReport"
    ++ "\ninput prefix groups: " ++ show (length pfxs)
    ++ "\n" ++ yellow ( basicAnalysis st)
    ++ "\n" ++ maybe (green "Consistency check pass") (\s -> red $ "Consistency check fail: " ++ s) (consistency st)
    ++ "\n" ++ yellow ( "analysis\n" ++ analysis st)
    ++ "\nGroupRIBReport done"

    where

    maybeColour s = if colour then s else ""
    yellow s = maybeColour "\x001b[33m" ++ s ++ "\x001b[0m"
    green s = maybeColour "\x001b[32m" ++ s ++ "\x001b[0m"
    red   s = maybeColour "\x001b[31m" ++ s ++ "\x001b[0m"

    st = mapRu newState pfxs
    ru pl = ribUpdate (map Prefix pl)
    mapRu = foldl (flip ru)
