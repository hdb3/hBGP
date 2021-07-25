{-# LANGUAGE FlexibleInstances #-}
module BGPRib.PT where
import Data.IntMap.Strict as IntMap
import Data.Maybe(fromMaybe)
import BGPRib.BGPData

class PrefixTableEntry pte where
  pteNull :: pte a -> Bool
  pteEmpty :: pte a
  pteUpdate :: (Route r) => r -> pte r -> pte r
  pteBest :: (Route r) => pte r -> r

---And the instance for List is:
instance PrefixTableEntry [] where

  pteNull = Prelude.null

  pteEmpty = []

  pteBest [] = BGPRib.PT.null
  pteBest (a:_) = a

  pteUpdate r0 rx = if isWithdraw r0 then f'' rx else f rx where
    
    f [] = [r0]
    f (r:rx) | eq r0 r = f' rx -- remove first sibling
             | gt r0 r = r0 : f'' (r:rx)
             | otherwise = r : f rx

    -- simple ordered insertion without duplicate removal
    f' [] = [r0]
    f' (r:rx) = if gt r0 r then r0 : r : rx else r : f' rx

    -- one-shot duplicate removal - no need to ever insert
    f'' [] = []
    f'' (r:rx) = if eq r0 r then rx else r : f'' rx

-- This exposes the minimal requirements on Route - an ‘equality’ function (shared source peer in practice, or (peer,pathID) for ADD-PATH) - and a ranking function.
-- e.g.

class (Show a) => Route a where
  eq :: a -> a -> Bool
  gt :: a -> a -> Bool
  isWithdraw :: a -> Bool
  isNull :: a -> Bool
  null :: a

instance Route RouteData where
  eq r1 r2 = peerData r1 == peerData r2
  gt r1 r2 = r1 > r2
  isWithdraw Withdraw {} = True
  isWithdraw _ = False
  isNull NullRoute = True
  isNull _ = False
  null = NullRoute

{-
  ### TODO - work out why this is not valid, i.e. define a class PrefixTable
  of which IntMap [RouteData] is just an instance...

class PrefixTable pt where
  ptUpdate :: (Route r, PrefixTableEntry pte) =>
    Key -> r -> pt -> (pte r, pte r, pt)
  ptQuery :: (Route r, PrefixTableEntry pte) =>
    Key -> pt -> pte r
  ptNew :: pt

instance PrefixTable ( IntMap [RouteData]) where
  ptUpdate k r pt = ( oldVal, newVal, IntMap.insert k newVal pt) where
    oldVal = fromMaybe pteEmpty (IntMap.lookup k pt)
    newVal = pteUpdate r oldVal

  ptQuery k pt = fromMaybe pteEmpty (IntMap.lookup k pt)
  -- ptQuery k pt = fromMaybe pteEmpty  (_ (IntMap.lookup k pt))
  ptNew = IntMap.empty

  -}

type PTE = [RouteData]
type PT = IntMap PTE

ptUpdate :: Key -> RouteData -> PT -> (PTE, PTE, PT)
ptUpdate k r pt = ( oldVal, newVal, IntMap.insert k newVal pt) where
  oldVal = fromMaybe pteEmpty (IntMap.lookup k pt)
  newVal = pteUpdate r oldVal

ptQuery ::Key -> PT -> PTE
ptQuery k pt = fromMaybe pteEmpty (IntMap.lookup k pt)

ptBest :: Key -> PT -> Maybe RouteData
ptBest k pt = (IntMap.lookup k pt) >>= safeHead
  where safeHead :: PTE -> Maybe RouteData
        safeHead [] = Nothing
        safeHead ax = Just (head ax)

ptNew :: PT
ptNew = IntMap.empty

ptList :: PT -> [(Key,PTE)]
ptList = IntMap.toList

ptKeys :: PT -> [Key]
ptKeys = IntMap.keys
