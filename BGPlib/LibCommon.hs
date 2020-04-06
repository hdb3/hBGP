module BGPlib.LibCommon (module BGPlib.LibCommon, module GHC.Generics, module Control.DeepSeq, module Data.IP)  where
import GHC.Generics(Generic)
import Control.DeepSeq
import Data.Binary
import Data.Binary.Get
import Data.IP
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as Base16

instance NFData IPv4
-- todo - make this a method of Binary by hiding the default method on import.....
putn :: Binary b => [b] -> Put
putn pfxs | null pfxs =  return ()
          | otherwise =  do put (head pfxs)
                            putn ( tail pfxs)
getn :: Binary b => Get [b]
getn = do
    empty <- isEmpty
    if empty
    then return []
    else do b <- get
            bs <- getn
            return (b:bs)

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