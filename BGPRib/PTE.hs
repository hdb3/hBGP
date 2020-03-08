{-# LANGUAGE FlexibleInstances #-}
module BGPRib.PTE where

import BGPRib.PT

type RD = (Int,Int)
instance Route (Int,Int) where
  eq (a,_) (b,_) = a == b
  gt (_,a) (_,b) = a > b
  isWithdraw (_,a) = 0 == a

insert :: RD -> [RD] -> [RD]
insert v = f where
    match (a,_) (b,_) = a == b
    gt (_,a) (_,b) = a > b
    f [] = [v]
    f (a:ax) | match v a = f' ax -- remove sibling
             | gt v a = v : f'' (a:ax)
             | otherwise = a : f ax
    -- simple ordered insertion without duplicate removal
    f' [] = [v]
    f' (a:ax) = if gt v a then v : a : ax else a : f' ax
    -- one-shot duplicate removal - no need to ever insert
    f'' [] = []
    f'' (a:ax) = if match v a then ax else a : f'' ax