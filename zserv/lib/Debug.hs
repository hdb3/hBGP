module Debug where
import Data.Word
import Numeric(showHex)
import qualified Data.ByteString.Base16 as Base16 -- from package base16-bytestring
import Hexdump -- from package pretty-hex
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8

zpad n s | n <= length s = s
         | otherwise = zpad n $ '0' : s
hex8 :: Word8 -> String
hex8 x = zpad 2 $ Numeric.showHex x ""
hex16 :: Word16 -> String
hex16 x = zpad 4 $ Numeric.showHex x ""
hex32 :: Word32 -> String
hex32 x = zpad 8 $ Numeric.showHex x ""
hex :: Int -> String
hex x = Numeric.showHex x' "" where
  x' = fromIntegral x :: Word64
fromHex = fst . Base16.decode
fromHex' = L.fromStrict . fst . Base16.decode
toHex = C8.unpack . Base16.encode
toHex' = toHex . L.toStrict
simpleHex' = simpleHex . L.toStrict
prettyHex' = prettyHex . L.toStrict
