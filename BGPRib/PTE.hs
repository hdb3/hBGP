{-# LANGUAGE FlexibleInstances #-}

module BGPRib.PTE where

import BGPRib.BGPData
import Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)

class PrefixTableEntry pte where
  pteNull :: pte a -> Bool
  pteEmpty :: pte a
  pteUpdate :: (Route r) => r -> pte r -> pte r
  pteBest :: (Route r) => pte r -> r

instance PrefixTableEntry [] where
  pteNull = Prelude.null

  pteEmpty = []

  pteBest [] = BGPRib.PTE.null
  pteBest (a : _) = a

  pteUpdate r0 rx = if isWithdraw r0 then f'' rx else f rx
    where
      f [] = [r0]
      f (r : rx)
        | eq r0 r = f' rx -- remove first sibling
        | gt r0 r = r0 : f'' (r : rx)
        | otherwise = r : f rx

      -- simple ordered insertion without duplicate removal
      f' [] = [r0]
      f' (r : rx) = if gt r0 r then r0 : r : rx else r : f' rx

      -- one-shot duplicate removal - no need to ever insert
      f'' [] = []
      f'' (r : rx) = if eq r0 r then rx else r : f'' rx

-- This shows the minimal requirements on Route - an ‘equality’ function (shared source peer in practice, or (peer,pathID) for ADD-PATH) - and a ranking function.
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
