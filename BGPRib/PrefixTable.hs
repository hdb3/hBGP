{-# LANGUAGE FlexibleInstances #-}

module BGPRib.PrefixTable where

import BGPRib.BGPData
import qualified BGPRib.PTE as PTE
import BGPlib.BGPlib (Prefix, fromPrefix, toPrefix)
import Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List
import Data.Maybe (fromMaybe)

type PrefixTableEntry = [RouteData]

type PrefixTable = IntMap PrefixTableEntry

instance {-# OVERLAPPING #-} Show PrefixTable where
  show = unlines . map showPTE . ptList
    where
      showPTE (k, v) = show (toPrefix k, v)

update :: PrefixTable -> [Prefix] -> RouteData -> (PrefixTable, [(Prefix, RouteData)])
update pt pfxs route = Data.List.foldl' f (pt, []) pfxs
  where
    f (pt', acc) pfx = (pt'', acc')
      where
        acc' = if newBest == oldBest then acc else (pfx, newBest) : acc
        (oldBest, newBest, pt'') = ptUpdate (fromPrefix pfx) route pt'

    ptUpdate k r pt = (PTE.pteBest oldVal, PTE.pteBest newVal, IntMap.insert k newVal pt)
      where
        oldVal = fromMaybe PTE.pteEmpty (IntMap.lookup k pt)
        newVal = PTE.pteUpdate r oldVal

queryPrefixTable :: PrefixTable -> Prefix -> RouteData
queryPrefixTable table pfx = PTE.pteBest $ ptQuery (fromPrefix pfx) table

showRibAt :: PrefixTable -> Prefix -> String
showRibAt table pfx = show (ptQuery (fromPrefix pfx) table)

ptQuery :: Key -> PrefixTable -> PrefixTableEntry
ptQuery k pt = fromMaybe PTE.pteEmpty (IntMap.lookup k pt)

withdraw :: PrefixTable -> [Prefix] -> PeerData -> (PrefixTable, [(Prefix, RouteData)])
withdraw pt pfxs pd = update pt pfxs (Withdraw pd)

withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable, [(Prefix, RouteData)])
withdrawPeer pt = withdraw pt (map toPrefix $ ptKeys pt)

newPrefixTable :: PrefixTable
newPrefixTable = IntMap.empty

ptList :: PrefixTable -> [(Key, PrefixTableEntry)]
ptList = IntMap.toList

ptKeys :: PrefixTable -> [Key]
ptKeys = IntMap.keys
