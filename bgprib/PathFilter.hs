{-# LANGUAGE OverloadedStrings #-}
module PathFilter where

import BGPlib

applyPathFilter :: [((a, [PathAttribute]),b)] -> [((a, [PathAttribute]),b)]
applyPathFilter = map applyPathFilterOnce
applyPathFilterOnce ((a,path),b) = ((a, pathFilter path),b)

pathFilter :: [PathAttribute] -> [PathAttribute]
pathFilter = normaliseASPath . deletePathAttributeType TypeCodePathAttributeAggregator . deletePathAttributeType TypeCodePathAttributeAtomicAggregate
-- for now do the minimum...
