{-# LANGUAGE FlexibleInstances #-}
module Common (module Common, module Data.IP) where
import Data.IP -- from package iproute
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict
import Data.Hashable
import Data.Maybe()

import qualified Data.ByteString.Base16 as Base16 -- from package base16-bytestring

toHex :: C8.ByteString -> [Char]
toHex = C8.unpack . Base16.encode

toHex' :: L.ByteString -> [Char]
toHex' = toHex . L.toStrict

groupBy_ :: (Eq k,Hashable k) => [(k, a)] -> [(k, [a])]
groupBy_ = Data.HashMap.Strict.toList . Data.HashMap.Strict.fromListWith (++) . Prelude.map (\(a,b) -> (a,[b]))

-- group and groupBy_ perform the same functions - hopefully, for small datasets at least, group is faster - e.g. for a handful of equivalent elements
group :: Eq a => [(a, b)] -> [(a, [b])]
group [] = []
group ((a,b):cx) = if null r then [ (a,s) ] else  (a,s) : group r
    where
    (s,r) = foldl acc ([b],[]) cx
    acc (u,v) (x,y) = if a == x then (y:u,v) else (u,(x,y):v)

--  group_ extends group over a Maybe key, discarding the Nothing values
group_ :: Eq a => [(Maybe a, b)] -> [(a, [b])]
group_ [] = []
group_ ((Nothing ,_):cx) = group_ cx
group_ ((Just a,b):cx) = if null r then [ (a,s) ] else  (a,s) : group_ r
    where
    (s,r) = foldl acc ([b],[]) cx
    acc (u,v) (Nothing ,_) = (u,v)
    acc (u,v) (Just x,y) = if a == x then (y:u,v) else (u,(Just x,y):v)
