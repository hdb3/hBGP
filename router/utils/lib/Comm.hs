{-# LANGUAGE FlexibleInstances #-}
module Comm where
import Data.List(delete,sortOn,foldl')
import qualified Data.Map.Strict as DMS

import Numeric(showHex)
hex x = showHex x ""


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
