module MRTBuilder (hPutUpdates) where

import Data.Bits (unsafeShiftR)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra (byteStringCopy)
import Data.IP (toHostAddress)
import Data.Monoid ((<>))
import Data.Word
import MRTlib
import System.IO (Handle)

hPutUpdates :: Handle -> [(BGPAttributes, IP4PrefixList)] -> IO ()
hPutUpdates handle routes = hPutBuilder handle $ updatesBuilder routes

updatesBuilder :: [(BGPAttributes, IP4PrefixList)] -> Builder
updatesBuilder = foldr (\(a1, a2) b -> updateBuilder 4096 a1 a2 <> b) mempty

simpleUpdateBuilder :: BGPAttributes -> IP4PrefixList -> Builder
simpleUpdateBuilder attrs pfxs =
  if 4096 < payloadLength
    then error "can't build such an update simply"
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

{-
the fly in the simple ointment is this: a few updates are too large to fit in one message.  A very few.
But one fly is one too many.
The soft size limit is the standard BGP messsage limit of 4096.
Extended limits are allowed by agreement, either explicit (capability...), or implicit (configuration...).
The extended limit is bounded by the 16 bit size field, i.e. 65535.
The implmentation can either be generic, or an escape, when the limit is hit, leaving the standard encoding in place.
I start with the latter.  It can still use the base framework for the fine detail.

Design

Break the prefix list in chunks, using a size derived from the limit less the size of the encoded attributes.
Its a kind of nested fold over a list of prefixes.....the inner fold accumulates prefixes until it reaches
the given limit, at which point it emits tuple of encoded length and prefix bloc, and carries on processing the remaining prefixes.
The main function folds over this list, building a new builder and combining with the same attributes and Update body/envelope to make complete
Update builder which is then apppended to the aggregate.
-}

updateBuilder :: Int -> BGPAttributes -> IP4PrefixList -> Builder
updateBuilder limit attrs pfxs =
  foldr (\a b -> buildMsg a <> b) mempty (enBloc (limit - basePayloadLength) pfxs)
  where
    buildMsg (len, bloc) =
      marker
        <> word16BE (fromIntegral $ basePayloadLength + len)
        <> word8 2
        <> word16BE 0
        <> word16BE (fromIntegral attributeLength)
        <> byteStringCopy (fromBGPAttributes attrs)
        <> prefixesBuilder bloc
    basePayloadLength = 16 + 1 + 2 + 2 + 2 + attributeLength
    attributeLength = fromIntegral $ B.length $ fromBGPAttributes attrs
    --
    enBloc :: Int -> [IP4Prefix] -> [(Int, [IP4Prefix])]
    enBloc limit = go 0 []
      where
        go n z [] = [(n, z)]
        go n z (a : ax)
          | limit < n' = (n, z) : go 0 [] (a : ax)
          | otherwise = go n' (a : z) ax
          where
            n' = n + encodedPrefixLength a

{-# INLINE marker #-}
marker :: Builder
marker = byteStringCopy $ B.replicate 16 0xff

{-# INLINE prefixesBuilder #-}
prefixesBuilder :: IP4PrefixList -> Builder
prefixesBuilder = foldr (\a b -> prefixBuilder a <> b) mempty

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

{-# INLINE encodedPrefixesLength #-}
encodedPrefixesLength :: IP4PrefixList -> Int
encodedPrefixesLength = foldr (\a b -> encodedPrefixLength a + b) 0
  where
    f a b = b + encodedPrefixLength a

{-# INLINE encodedPrefixLength #-}
encodedPrefixLength :: IP4Prefix -> Int
encodedPrefixLength prefix = fromIntegral (div (15 + snd prefix) 8)
