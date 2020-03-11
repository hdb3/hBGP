module BGPRib.Common (module BGPRib.Common, module Data.IP) where
import Data.IP
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable(Hashable)
import Data.List(foldl')

toHex :: C8.ByteString -> String
toHex = C8.unpack . Base16.encode

toHex' :: L.ByteString -> String
toHex' = toHex . L.toStrict

-- this is a (hopefully) effiecent function in cae of large list based maps
-- which partitions values with a common key into distinct lists
groupBy_ :: (Eq k,Hashable k) => [(k, a)] -> [(k, [a])]
groupBy_ = HashMap.toList . fromList where
           fromList = foldl (\m (k,v) -> HashMap.alter (Just . maybe [v] (v :)) k m) HashMap.empty

-- ugly old definition...
--groupBy_ = HashMap.toList . HashMap.fromListWith (++) . Prelude.map (\(a,b) -> (a,[b]))

-- TODO prove this function
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p zx@(z:_) = g zx where
    f = foldl' (\(ax,bx) c -> if p z c then (c:ax,bx) else (ax,c:bx)) ([],[])
    g ax = let (bx,cx) = f ax
        in bx : g cx

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
