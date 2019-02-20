module Prefix where

class (Ord a,Show a) => Prefix a where
  toInt :: a -> Int
  fromInt :: Int -> a
