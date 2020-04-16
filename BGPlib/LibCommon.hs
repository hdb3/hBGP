module BGPlib.LibCommon (module BGPlib.LibCommon, module GHC.Generics, module Control.DeepSeq, module Data.IP)  where
import GHC.Generics(Generic)
import Control.DeepSeq
import Data.Word
import Data.IP
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Base16

instance NFData IPv4

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