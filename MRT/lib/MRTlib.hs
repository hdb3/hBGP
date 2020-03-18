{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module MRTlib ( module MRTformat
              , PeerIndex
              , IP4Prefix,IP6Prefix
              , IP4PrefixList, IP6PrefixList
              , IPPrefix(..)
              , PrefixListHash, PrefixListHashList
              , PrefixHash(..)
              , v4hash, v4unhash, v6hash, v6unhash, IP4PrefixHash, IP6PrefixHash ) where

import Data.Bits
import Data.IP
import Data.Word 
import GHC.Generics (Generic)
import qualified Data.Hashable
import MRTformat
import MRTPrefixes


data IPPrefix = IP4Prefix IP4Prefix | IP6Prefix IP6Prefix deriving (Show,Generic)
instance Data.Hashable.Hashable IPPrefix
instance Data.Hashable.Hashable IPv4
instance Data.Hashable.Hashable IPv6

type IP4PrefixHash = Int
type IP6PrefixHash = Int

v4hash :: IP4Prefix -> IP4PrefixHash
v4hash (ip,l) = let w64 x = fromIntegral x :: Word64 in fromIntegral $ unsafeShiftL (w64 l) 32 .|. w64 ( byteSwap32 $ toHostAddress ip)

v4unhash :: IP4PrefixHash -> IP4Prefix
v4unhash h = ( fromHostAddress $ byteSwap32 $ fromIntegral $ 0xffffffff .&. h' , fromIntegral $ unsafeShiftR h' 32 ) where h' = fromIntegral h :: Word64

v6unhash :: IP6PrefixHash -> IP6Prefix
v6unhash h = ( fromHostAddress64 $ 0xffffffffffffff .&. h' , fromIntegral $ unsafeShiftR h' 56)
    where
    h' = fromIntegral h :: Word64
    fromHostAddress64 x = fromHostAddress6 (w0,w1,0,0) where
        w0 = fromIntegral $ unsafeShiftR x 32
        w1 = fromIntegral $ x .&. 0xffffffff

v6hash :: IP6Prefix -> IP6PrefixHash
v6hash (ip,l) | l < 57 = fromIntegral $ unsafeShiftL (w64 l) 56 .|. toHostAddress64 ip
            -- | otherwise = fromIntegral $ asWord64 $ hash64WithSeed (w64 l) (toHostAddress64 ip) 
              | otherwise = error $ "cant' handle IPv6 > /56: " ++ show l ++ "/" ++ show ip
    where
    w64 x = fromIntegral x :: Word64
    toHostAddress64 x = let (w0,w1,_,_) = toHostAddress6 x in unsafeShiftL (w64 w0) 32 .|. w64 w1

class PrefixHash a where
    prefixHash :: a -> Int
    prefixShow :: a -> String

instance PrefixHash [IP4Prefix] where
    prefixHash = Data.Hashable.hash
    prefixShow = show

instance PrefixHash [IP6Prefix] where
    prefixHash = Data.Hashable.hash
    prefixShow = show


type IP4Prefix = (IPv4,Word8)
type IP4PrefixList = [IP4Prefix]
type IP6Prefix = (IPv6,Word8)
type IP6PrefixList = [IP6Prefix]
type BGPAttributeHash = Int
type PrefixListHash = Int
type PrefixListHashList = [PrefixListHash]
type PeerIndex = Word16
