module BGPRib.PT where

import BGPRib.BGPData
import Control.Logger.Simple
import Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)

type PrefixTableEntry = [RouteData]

pteNull :: PrefixTableEntry -> Bool
pteNull = Prelude.null

pteEmpty :: PrefixTableEntry
pteEmpty = []

pteBest :: PrefixTableEntry -> RouteData
pteBest [] = NullRoute
pteBest (a : _) = a

pteUpdate :: RouteData -> PrefixTableEntry -> PrefixTableEntry
pteUpdate (Withdraw _) [] = pureWarn "Withdraw in an empty RIB" $ []
pteUpdate NullRoute [] = pureTrace "Nullroute in an empty RIB" $ []
pteUpdate NullRoute rx = pureTrace "Nullroute in an empty RIB" $ rx
pteUpdate route@Update {} [] = [route]
pteUpdate route@Update {} rx = reverse $ f_start [] rx
  where
    f_start :: [RouteData] -> [RouteData] -> [RouteData]
    f_start head [] = head
    f_start head (r : rr)
      | peerData route == peerData r = f_got_match head rr
      | route > r = f_done_insert (route : head) (r : rr)
      | otherwise = f_start (r : head) rr
    f_got_match :: [RouteData] -> [RouteData] -> [RouteData]
    f_got_match head [] = head
    f_got_match head (r : rr)
      | route > r = f_finish (route : head) (r : rr)
      | otherwise = f_got_match (r : head) rr
    f_done_insert :: [RouteData] -> [RouteData] -> [RouteData]
    f_done_insert head [] = head
    f_done_insert head (r : rr)
      | (peerData route) == (peerData r) = f_finish head rr
      | otherwise = f_done_insert (r : head) rr
    f_finish :: [RouteData] -> [RouteData] -> [RouteData]

    f_finish head [] = head
    f_finish head (r : rr) = f_finish (r : head) rr
pteUpdate (Withdraw sourcePeer) rx = reverse $ f_start [] rx
  where
    f_start :: [RouteData] -> [RouteData] -> [RouteData]
    f_start head [] = head
    f_start head (r : rr)
      | sourcePeer == (peerData r) = f_finish head rr
      | otherwise = f_start (r : head) rr
    f_finish :: [RouteData] -> [RouteData] -> [RouteData]
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

ptBest :: Key -> PT -> Maybe RouteData
ptBest k pt = (IntMap.lookup k pt) >>= safeHead
  where
    safeHead :: PrefixTableEntry -> Maybe RouteData
    safeHead [] = Nothing
    safeHead ax = Just (head ax)

ptNew :: PT
ptNew = IntMap.empty

ptList :: PT -> [(Key, PrefixTableEntry)]
ptList = IntMap.toList

ptKeys :: PT -> [Key]
ptKeys = IntMap.keys
