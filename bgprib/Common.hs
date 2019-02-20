{-# LANGUAGE FlexibleInstances #-}
module Common (module Common, module Data.IP) where
import Data.IP -- from package iproute
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict
import Data.Hashable

import qualified Data.ByteString.Base16 as Base16 -- from package base16-bytestring

toHex = C8.unpack . Base16.encode
toHex' = toHex . L.toStrict

groupBy_ :: (Eq k,Hashable k) => [(k, a)] -> [(k, [a])]
groupBy_ = Data.HashMap.Strict.toList . Data.HashMap.Strict.fromListWith (++) . Prelude.map (\(a,b) -> (a,[b]))
