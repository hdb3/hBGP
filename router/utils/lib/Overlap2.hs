{-#LANGUAGE OverloadedStrings #-}
module Overlap2(Tree(Empty),PrefixTree,height,size,toList,fromList,insertPrefix,reduce,count,longest,partition,head) where
import Prelude hiding (head)
import Data.List(foldl')
import Data.Bits(testBit)
import Data.Word
import Prefixes

type PrefixTree = Tree Prefix

-- TODO - make the Tree gereic in Insert etc.....
data Tree a = Empty | Item (Maybe a) (Tree a) (Tree a) deriving Eq
instance Show a => Show (Tree a) where
    show = showRL where

        showRL Empty = ""
        showRL (Item (Just a) b c) = "[" ++ show a ++ "]" ++ show b ++ show c
        showRL (Item Nothing Empty c) = "R" ++ show c
        showRL (Item Nothing b Empty) = "L" ++ show b
        showRL (Item Nothing b c) = "RL" ++ show b ++ show c

        showN (Item (Just a) b c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
        showN (Item Nothing b c) = "(*," ++ show b ++ "," ++ show c ++ ")"
        showN Empty = "-"

isSet :: Word8 -> Word32 -> Bool
isSet level bits = testBit bits (31 - fromIntegral level)

ins (a,bits,target,level) Empty        | level == target  = Item (Just a) Empty Empty
                                       | isSet level bits = Item Nothing (ins (a,bits,target,level+1) Empty) Empty
                                       | otherwise        = Item Nothing Empty (ins (a,bits,target,level+1) Empty) 
ins (a,bits,target,level) (Item x y z) | level == target  = Item (Just a) y z
                                       | isSet level bits = Item x (ins (a,bits,target,level+1) y) z
                                       | otherwise        = Item x y (ins (a,bits,target,level+1) z) 

insert a bits target = ins (a,bits,target,0)

insertPrefix :: Prefix -> PrefixTree -> PrefixTree
insertPrefix pfx@(Prefix (l,v)) t = ins (pfx,v,l,0) t

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Item Nothing b c) = Item Nothing (fmap f b) (fmap f c)
    fmap f (Item (Just a) b c) = Item (Just (f a)) (fmap f b) (fmap f c)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Item Nothing b c ) = foldr f (foldr f z b) c
    foldr f z (Item (Just a) b c ) = foldr f (foldr f (f a z) b) c

count :: Tree a -> Int
count Empty = 0
count (Item _ b c ) = 1 + count b + count c

singleton x = Item (Just x) Empty Empty

toList :: PrefixTree -> [Prefix]
toList t = foldr (:) [] t

fromList :: [Prefix] -> PrefixTree
fromList = foldl' (flip insertPrefix) Empty

size :: Tree a -> Int
size = foldr (\_ b -> b+1) 0

-- this version of longest took 11 minutes to run over a full internet route table
-- the longets sequence discovered was 
-- [222.32.0.0/11,222.35.0.0/16,222.35.128.0/17,222.35.128.0/18,222.35.128.0/19,222.35.136.0/21,222.35.136.0/22,222.35.137.0/24]
-- and also validated the height function

longest :: Tree a -> [a]
longest Empty = []
longest (Item Nothing b c) = if (length $ longest c) > (length $ longest b) then longest c else longest b
longest (Item (Just a) b c) = a : (longest (Item Nothing b c))

height :: Tree a -> Int
-- height = height' . reduce where
height = height' where
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
reduce (Item Nothing b Empty)  = reduce b
reduce (Item a b c) = Item a (reduce b) (reduce c)

partition :: Tree a -> [Tree a]
partition Empty = []
partition (Item Nothing b c) = partition b ++ partition c
partition t = [t]
