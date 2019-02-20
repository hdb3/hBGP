{-# LANGUAGE FlexibleInstances,FlexibleContexts, OverloadedStrings #-}
module ASPathUtils where
import Prelude hiding (seq)
import Data.List(sort,sortOn,foldl')
import qualified Data.Map.Strict as DMS
import Data.Word

import BGPlib

flattenPath :: [ASSegment Word32] -> [Word32]
flattenPath [] = []
flattenPath (ASSequence []:segs) = flattenPath segs
flattenPath (ASSequence asns:segs) = asns ++ flattenPath segs
flattenPath (ASSet []:segs) = flattenPath segs
flattenPath (ASSet asns:segs) = head asns : flattenPath segs

hasLoop :: [Word32] -> Bool
hasLoop = go . sort where
    go [] = False
    go [_] = False
    go  (x:y:ax) | x==y = True
                 | otherwise = go (y:ax)


removePrepends :: [Word32] -> [Word32]
removePrepends [] = []
removePrepends [x] = [x]
removePrepends (x:y:ax) | x==y = removePrepends (y:ax)
                        | otherwise = x : removePrepends (y:ax)

showPath :: Show t => [ASSegment t] -> String
showPath [ASSequence seq1 , ASSet set, ASSequence seq2] = "SEQ+SET+SEQ " ++ show seq1 ++ " / " ++ show set ++ " / " ++ show seq2
showPath [ASSequence seq , ASSet set] = "SEQ+SET     " ++ show seq ++ " / " ++ show set
showPath [ASSequence seq] = "SEQ-       " ++ show seq
showPath [] = "EMPTY       "
showPath x = "UNKNOWN     " ++ show x

-- unsure which implementation of 'distribution' is better.....

distribution :: Ord a => [a] -> [(a,Int)]
distribution = sortOn ( (0 -) . snd) . DMS.toList . mapped
    where
    mapped = foldl' f' DMS.empty
    f' m k = DMS.insertWith (+) k 1 m


distribution_ :: Integral a => Int -> [a] -> [(a,Int)]
distribution_ n a | length (distribution a) > n = a1 ++ [rollUp a2]
                  | otherwise = distribution a
                  where
    (a1,a2) = splitAt n (distribution a)
    rollUp [] = (0,0)
    rollUp ax = (0, sum (map snd a2))
