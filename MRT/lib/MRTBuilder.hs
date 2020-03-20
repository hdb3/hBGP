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

hPutUpdates :: Handle -> [(BGPAttributes, IP4PrefixList)] -> IO ()
hPutUpdates handle routes = hPutBuilder handle $ updatesBuilder routes

updatesBuilder :: [(BGPAttributes, IP4PrefixList)] -> Builder
updatesBuilder = foldr (\(a1, a2) b -> (updateBuilder a1 a2) <> b) mempty

updateBuilder :: BGPAttributes -> IP4PrefixList -> Builder
updateBuilder attrs pfxs =
  if 4096 < payloadLength
    then extendedUpdateBuilder attrs pfxs
    else
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

--
{-# INLINE encodedPrefixesLength #-}
encodedPrefixesLength :: IP4PrefixList -> Int
encodedPrefixesLength = foldr f 0
  where
    f a b = b + fromIntegral (div (15 + snd a) 8)

--

{-
the fly in the simple ointment is this: a few updates are too large to fit in one message.  A very few.
But one fly is one too many.
The soft size limit is the standard BGP messsage limit of 4096.
Extended limits are allowed by agreement, either explicit (capability...), or implicit (configuration...).
The extended limit is bounded by the 16 bit size field, i.e. 65535.
The implmentation can either be generic, or an escape, when the limit is hit, leaving the standard encoding in place.
I start with the latter.  It can still use the base framework for the fine detail.
-}

extendedUpdateBuilder :: BGPAttributes -> IP4PrefixList -> Builder
extendedUpdateBuilder attrs pfxs = mempty
