module BGPlib.GetBGPMsg where
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L

wireFormat :: L.ByteString -> L.ByteString
wireFormat bs = toLazyByteString $ lazyByteString (L.replicate 16 0xff) <> word16BE (fromIntegral $ 18 + L.length bs) <> lazyByteString bs