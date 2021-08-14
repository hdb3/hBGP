module BGPRib.PT where

import BGPRib.BGPData
import Control.Logger.Simple
import Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)

type PrefixTableEntry = [RibRoute]

pteNull :: PrefixTableEntry -> Bool
pteNull = Prelude.null

pteEmpty :: PrefixTableEntry
pteEmpty = []

pteBest :: PrefixTableEntry -> RouteExport
pteBest [] = RouteExportWithdraw
pteBest (a : _) = ribExport a

pteUpdate :: RouteData -> PrefixTableEntry -> PrefixTableEntry
pteUpdate (Withdraw _) [] = pureWarn "Withdraw in an empty RIB" []
pteUpdate NullRoute [] = pureTrace "Nullroute in an empty RIB" []
pteUpdate NullRoute rx = pureTrace "Nullroute in an empty RIB" rx
pteUpdate route@Update {} [] = [ribRoute route]
pteUpdate update@Update {} rx = reverse $ f_start [] rx
  where
    route = ribRoute update
    f_start :: [RibRoute] -> [RibRoute] -> [RibRoute]
    f_start head [] = head
    f_start head (r : rr)
      | ribPeer route == ribPeer r = f_got_match head rr
      | route > r = f_done_insert (route : head) (r : rr)
      | otherwise = f_start (r : head) rr
    f_got_match :: [RibRoute] -> [RibRoute] -> [RibRoute]
    f_got_match head [] = head
    f_got_match head (r : rr)
      | route > r = f_finish (route : head) (r : rr)
      | otherwise = f_got_match (r : head) rr
    f_done_insert :: [RibRoute] -> [RibRoute] -> [RibRoute]
    f_done_insert head [] = head
    f_done_insert head (r : rr)
      | ribPeer route == ribPeer r = f_finish head rr
      | otherwise = f_done_insert (r : head) rr
    f_finish :: [RibRoute] -> [RibRoute] -> [RibRoute]

    f_finish head [] = head
    f_finish head (r : rr) = f_finish (r : head) rr
pteUpdate (Withdraw peer) rx = reverse $ f_start [] rx
  where
    f_start :: [RibRoute] -> [RibRoute] -> [RibRoute]
    f_start head [] = head
    f_start head (r : rr)
      | peer == ribPeer r = f_finish head rr
      | otherwise = f_start (r : head) rr
    f_finish :: [RibRoute] -> [RibRoute] -> [RibRoute]
    f_finish head [] = head
    f_finish head (r : rr) = f_finish (r : head) rr

type PT = IntMap PrefixTableEntry

ptUpdate :: Key -> RouteData -> PT -> (PrefixTableEntry, PrefixTableEntry, PT)
ptUpdate k r pt = (oldVal, newVal, IntMap.insert k newVal pt)
  where
    oldVal = fromMaybe pteEmpty (IntMap.lookup k pt)
    newVal = pteUpdate r oldVal

ptQuery :: Key -> PT -> PrefixTableEntry
ptQuery k pt = fromMaybe pteEmpty (IntMap.lookup k pt)

ptBest :: Key -> PT -> RouteExport
ptBest k pt = maybe RouteExportWithdraw safeHead (IntMap.lookup k pt)
  where
    safeHead :: PrefixTableEntry -> RouteExport
    safeHead [] = RouteExportWithdraw
    safeHead ax = ribExport (head ax)

ptNew :: PT
ptNew = IntMap.empty

ptList :: PT -> [(Key, PrefixTableEntry)]
ptList = IntMap.toList

ptKeys :: PT -> [Key]
ptKeys = IntMap.keys
