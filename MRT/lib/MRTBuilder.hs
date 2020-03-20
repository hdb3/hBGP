module MRTBuilder where

import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
-- import qualified Data.ByteString.Lazy as L

import Data.IP (toHostAddress)
import Data.Monoid ((<>))
import Data.Word
--
import MRTPrefixes
import MRTlib
import System.IO (Handle)

hPutUpdate :: Handle -> BGPAttributes -> IP4PrefixList -> IO ()
hPutUpdate handle attrs pfxs = hPutBuilder handle $ updateBuilder attrs pfxs

updateBuilder :: BGPAttributes -> IP4PrefixList -> Builder
updateBuilder attrs pfxs =
  marker
    <> word16BE (fromIntegral payloadLength)
    <> word8 2
    <> word16BE 0
    <> word16BE (fromIntegral attributeLength)
    <> byteStringCopy (fromBGPAttributes attrs)
    <> prefixesBuilder pfxs
  where
    payloadLength = 16 + 1 + 2 + 2 + 2 + attributeLength + nlriLength
    nlriLength = encodedPrefixesLength pfxs
    attributeLength = fromIntegral $ B.length $ fromBGPAttributes attrs
    --
    {-# INLINE marker #-}
    marker :: Builder
    marker = byteStringCopy $ B.replicate 16 0xff
    --
    {-# INLINE encodedPrefixesLength #-}
    encodedPrefixesLength :: IP4PrefixList -> Int
    encodedPrefixesLength = fromIntegral . foldr f 0
      where
        f a b = div (15 + snd a) 8 + b
    --
    {-# INLINE prefixesBuilder #-}
    prefixesBuilder :: IP4PrefixList -> Builder
    prefixesBuilder = foldr (\a b -> prefixBuilder a <> b) mempty
    --
    {-# INLINE prefixBuilder #-}
    prefixBuilder :: IP4Prefix -> Builder
    prefixBuilder (ipV4, subnet)
      --
      | subnet == 0 =
        word8 0
      --
      | subnet < 9 =
        word8 subnet <> word8 (fromIntegral $ unsafeShiftR ip 24)
      --
      | subnet < 17 =
        word8 subnet <> word16BE (fromIntegral $ unsafeShiftR ip 16)
      --
      | subnet < 25 =
        word8 subnet <> word16BE (fromIntegral $ unsafeShiftR ip 16)
          <> word8
            (fromIntegral $ unsafeShiftR ip 8)
      --
      | subnet < 33 =
        word8 subnet <> word32BE ip
      --
      | otherwise =
        error "unreasonable subnet length > 32"
      where
        --
        ip = byteSwap32 $ toHostAddress ipV4
