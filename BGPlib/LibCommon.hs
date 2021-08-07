module BGPlib.LibCommon (module BGPlib.LibCommon, module GHC.Generics, module Control.DeepSeq, module Data.IP) where

import Control.DeepSeq
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Data.IP
import Data.Word
import GHC.Generics (Generic)

instance NFData IPv4

instance Hashable IPv4

class Enum e => EnumWord8 e where
  decode8 :: Word8 -> e
  decode8 = toEnum . fromIntegral
  {-# INLINE decode8 #-}
  encode8 :: e -> Word8
  encode8 = fromIntegral . fromEnum
  {-# INLINE encode8 #-}

toHex :: C8.ByteString -> String
toHex = C8.unpack . Base16.encode

toHex' :: L.ByteString -> String
toHex' = toHex . L.toStrict
