module Overlap where

-- this is a fork of Overlap.hs from the router library

import Data.Bits (setBit, testBit)
import Data.List (foldl')
import Data.Word
import MRTPrefixes
import Prelude hiding (head)

-- regarding optimisation via strictness and unpacking - the strict form (bang patterns only)
-- showed ~10-20% better performance
-- the unpacked form is rejected as pointless by GHC
-- the UnboxedTuples pragma made a smaller difference (<5%)
-- data Tree a = Empty | Item (Maybe a) (Tree a) (Tree a) deriving Eq
-- data Tree a = Empty | Item {-# UNPACK #-} ! (Maybe a) {-# UNPACK #-} ! (Tree a) {-# UNPACK #-} ! (Tree a) deriving Eq
data Tree a = Empty | Item !(Maybe a) !(Tree a) !(Tree a) deriving (Eq)

instance Show a => Show (Tree a) where
  show = showRL
    where
      showRL Empty = ""
      showRL (Item (Just a) b c) = "[" ++ show a ++ "]" ++ show b ++ show c
      showRL (Item Nothing Empty c) = "R" ++ show c
      showRL (Item Nothing b Empty) = "L" ++ show b
      showRL (Item Nothing b c) = "RL" ++ show b ++ show c

      showN (Item (Just a) b c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
      showN (Item Nothing b c) = "(*," ++ show b ++ "," ++ show c ++ ")"
      showN Empty = "-"

insert :: Prefix -> a -> Tree a -> Tree a
-- Observation: insert overwrites silently any existing value at a given position
insert (Prefix (l, v)) a = ins (a, v, l, 0)
  where
    isSet :: Word8 -> Word32 -> Bool
    isSet level bits = testBit bits (31 - fromIntegral level)
    ins :: (a, Word32, Word8, Word8) -> Tree a -> Tree a
    ins (a, bits, target, level) Empty
      | level == target = Item (Just a) Empty Empty
      | isSet level bits = Item Nothing (ins (a, bits, target, level + 1) Empty) Empty
      | otherwise = Item Nothing Empty (ins (a, bits, target, level + 1) Empty)
    ins (a, bits, target, level) (Item x y z)
      | level == target = Item (Just a) y z
      | isSet level bits = Item x (ins (a, bits, target, level + 1) y) z
      | otherwise = Item x y (ins (a, bits, target, level + 1) z)

insertLS :: Prefix -> a -> Tree a -> Tree a
-- insertLS == insert 'Least Specific' - insertLS removes more specific instances when a covering shorter prefix is inserted
insertLS (Prefix (l, v)) a = ins (a, v, l, 0)
  where
    isSet :: Word8 -> Word32 -> Bool
    isSet level bits = testBit bits (31 - fromIntegral level)
    ins :: (a, Word32, Word8, Word8) -> Tree a -> Tree a
    ins (a, bits, target, level) Empty
      | level == target = Item (Just a) Empty Empty
      | isSet level bits = Item Nothing (ins (a, bits, target, level + 1) Empty) Empty
      | otherwise = Item Nothing Empty (ins (a, bits, target, level + 1) Empty)
    ins (a, bits, target, level) (Item x y z)
      | level == target = Item (Just a) Empty Empty
      | isSet level bits = Item x (ins (a, bits, target, level + 1) y) z
      | otherwise = Item x y (ins (a, bits, target, level + 1) z)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Item Nothing b c) = Item Nothing (fmap f b) (fmap f c)
  fmap f (Item (Just a) b c) = Item (Just (f a)) (fmap f b) (fmap f c)

instance Foldable Tree where
  foldr f z Empty = z
  foldr f z (Item Nothing b c) = foldr f (foldr f z b) c
  foldr f z (Item (Just a) b c) = foldr f (foldr f (f a z) b) c

count :: Tree a -> Int
count Empty = 0
count (Item _ b c) = 1 + count b + count c

singleton :: a -> Tree a
singleton x = Item (Just x) Empty Empty

elems :: Tree a -> [a]
elems = foldr (:) []

toList :: Tree a -> [(Prefix, a)]
toList t = tl 0 0 t
  where
    tl :: Word32 -> Word8 -> Tree a -> [(Prefix, a)]
    tl _ _ Empty = []
    tl prefix level (Item Nothing t0 t1) = tl (setBit prefix (31 - fromIntegral level)) (level + 1) t0 ++ tl prefix (level + 1) t1
    tl prefix level (Item (Just a) t0 t1) = (Prefix (level, prefix), a) : tl prefix level (Item Nothing t0 t1)

toListLS :: Tree a -> [(Prefix, a)]
toListLS t = ls 0 0 t
  where
    ls :: Word32 -> Word8 -> Tree a -> [(Prefix, a)]
    ls _ _ Empty = []
    ls prefix level (Item Nothing t0 t1) = ls (setBit prefix (31 - fromIntegral level)) (level + 1) t0 ++ ls prefix (level + 1) t1
    ls prefix level (Item (Just a) t0 t1) = [(Prefix (level, prefix), a)]

leastSpecific :: [(Prefix, a)] -> [(Prefix, a)]
leastSpecific = toList . fromListLS

leastSpecific' :: [(Prefix, a)] -> [(Prefix, a)]
leastSpecific' = toListLS . fromList

fromList :: [(Prefix, a)] -> Tree a
fromList = foldl' (\t (pfx, a) -> insert pfx a t) Empty

fromListLS :: [(Prefix, a)] -> Tree a
fromListLS = foldl' (\t (pfx, a) -> insertLS pfx a t) Empty

size :: Tree a -> Int
size = foldr (\_ b -> b + 1) 0

-- this version of longest took 11 minutes to run over a full internet route table
-- the longets sequence discovered was
-- [222.32.0.0/11,222.35.0.0/16,222.35.128.0/17,222.35.128.0/18,222.35.128.0/19,222.35.136.0/21,222.35.136.0/22,222.35.137.0/24]
-- and also validated the height function

longest :: Tree a -> [a]
longest Empty = []
longest (Item Nothing b c) = if length (longest c) > length (longest b) then longest c else longest b
longest (Item (Just a) b c) = a : longest (Item Nothing b c)

height :: Tree a -> Int
-- height = height' . reduce where
height = height'
  where
    height' Empty = 0
    height' (Item Nothing b c) = max (height' b) (height' c)
    height' (Item _ b c) = 1 + max (height' b) (height' c)

head :: Tree a -> a
head (Item (Just a) _ _) = a
head (Item Nothing b Empty) = head b
head (Item Nothing Empty c) = head c
head _ = undefined

reduce :: Tree a -> Tree a
reduce Empty = Empty
reduce (Item Nothing Empty c) = reduce c
reduce (Item Nothing b Empty) = reduce b
reduce (Item a b c) = Item a (reduce b) (reduce c)

partition :: Tree a -> [Tree a]
partition Empty = []
partition (Item Nothing b c) = partition b ++ partition c
partition t = [t]

{-
-}
