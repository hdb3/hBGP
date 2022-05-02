{-# LANGUAGE OverloadedStrings #-}

module BGPlib.PrefixBuilder
  ( prefixBuilder,
    updateBuilder,
  )
where

import qualified BGPlib.Prefixes
import ByteString.StrictBuilder
import Data.Bits (unsafeShiftR)
import qualified Data.ByteString as B
import Data.IP
import Data.Word

{-
Using StrictBuilder sidesteps the problem of having to efficiently accumul;late the size of aggregates being built
for the purpose of building container TLVs.  prefix Builders have another problem - the need to segment large lists to prevent them from
exceeding protocl defined size limits.
The interface to this buildr therefore includes a target string byte length.
The output could have variant forms - either a list of conforming Builders, or a single Builder and a remnant list of prefixes.
The second form is more general and can be used to optimally build Updates which pack both advertsied and withdranw prefixes.
If this particular optimisation is not supported then the firs form is sufficient, and the resulting signature is:

builder :: Int -> [Prefix] -> [Builder]

The implmentation approach is simple - a fold which accumulates fragments of Builders until the next fragment would exceed the bounds,
when a new builder is started.

The kernel function assuming that the limit value is in scope, has signature

  go :: accumulator -> Int -> [Prefix] -> Builders
  go acc freespace prefixes = builders

The terminating case is:
  go acc _ [] = [acc]

The test condition is applied to the non-empty head of the prefix list:

  go acc freespace (prefix:prefixes)
    | byteLength prefix > freespace = acc : go mempty limit (prefix:prefixes) -- note, never calls go with empty prefix list AND mempty accumulator!
    | otherwise = go ( acc <> encode prefix ) ( freespace - byteLength prefix ) prefixes
( It is worth asking if the order of acc and emcoded prefix in ( acc <> encode prefix) affects performance...... intuitively ( encode prefix <> acc ) is better, but reverses the 'natutrral' order)

The seed invocation of go is:

  go mempty limit prefixes

-}

type IP4Prefix = (IPv4, Word8)

-- type Prefix = IP4Prefix
type Prefix = BGPlib.Prefixes.Prefix

prefixBuilder :: Int -> [Prefix] -> [Builder]
prefixBuilder limit prefixes = go mempty limit prefixes
  where
    go :: Builder -> Int -> [Prefix] -> [Builder]
    go acc freespace prefixes
      | null prefixes = [acc]
      | byteLength (head prefixes) > freespace = acc : go mempty limit prefixes -- note, never calls go with empty prefix list AND mempty accumulator!
      | otherwise = go (acc <> encode (head prefixes)) (freespace - byteLength (head prefixes)) (tail prefixes)
    encode = encodeIntPrefix
    byteLength = byteLengthIntPrefix

-- encode = encodeTuplePrefix
-- byteLength = byteLengthTuplePrefix

{-# INLINE encodeIntPrefix #-}
encodeIntPrefix :: BGPlib.Prefixes.Prefix -> Builder
encodeIntPrefix prefix = encode (BGPlib.Prefixes.addressPrefix prefix) (BGPlib.Prefixes.lengthPrefix prefix)

{-# INLINE byteLengthIntPrefix #-}
byteLengthIntPrefix :: BGPlib.Prefixes.Prefix -> Int
byteLengthIntPrefix = byteLength . BGPlib.Prefixes.lengthPrefix

{-# INLINE encodeTuplePrefix #-}
encodeTuplePrefix :: IP4Prefix -> Builder
encodeTuplePrefix prefix = encode (byteSwap32 $ toHostAddress $ fst prefix) (snd prefix)

{-# INLINE byteLengthTuplePrefix #-}
byteLengthTuplePrefix :: IP4Prefix -> Int
byteLengthTuplePrefix = byteLength . snd

{-# INLINE byteLength #-}
byteLength :: Word8 -> Int
byteLength length = fromIntegral (div (15 + length) 8)

{-# INLINE encode #-}
encode :: Word32 -> Word8 -> Builder
encode ip subnet
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

{-

  updateBuilder - use subsidiary strict builders to assemble one or more complete, wire-format, Update messages
  the function accepts witdrawn and advertsied nlri which may be assembled into a common or discrete messages depending on circumstances

-}

updateBuilder :: [Prefix] -> Builder -> [Prefix] -> Builder
updateBuilder = extUpdateBuilder 4096

extUpdateBuilder :: Int -> [Prefix] -> Builder -> [Prefix] -> Builder
extUpdateBuilder limit withdrawn attributeBuilder nlri =
  let baseSize = (16 + 1 + 2 + 2 + 2)
      maxPayLoad = limit - baseSize
      attributeLength = builderLength attributeBuilder
      withdrawnBuilders = prefixBuilder maxPayLoad withdrawn
      withdrawnLength = if null withdrawnBuilders then 0 else builderLength (head withdrawnBuilders)
      nlriBuilders = prefixBuilder (maxPayLoad - attributeLength) nlri
      nlriLength = if null nlriBuilders then 0 else builderLength (head nlriBuilders)
      marker = bytes $ B.replicate 16 0xff
      -- marker = byteStringCopy ( "/255/255/255/255/255/255/255/255/255/255/255/255/255/255/255/255"  :: B.ByteString)
      buildNLRI nlriBuilder =
        marker
          <> word16BE (fromIntegral $ baseSize + attributeLength + builderLength nlriBuilder)
          <> word8 2
          <> word16BE 0
          <> word16BE (fromIntegral attributeLength)
          <> attributeBuilder
          <> nlriBuilder
      buildWithdraw withdrawBuilder =
        marker
          <> word16BE (fromIntegral $ baseSize + builderLength withdrawBuilder)
          <> word8 2
          <> word16BE (fromIntegral $ builderLength withdrawBuilder)
          <> withdrawBuilder
          <> word16BE 0
   in if limit >= baseSize + attributeLength + withdrawnLength + nlriLength
        then -- common / simple case  - all will fit in one message......
        -- NB - this inlcludes the null case (EndOfRIB), and multiprtotocol cases which have only attributes
        --      though the latter will likely require a similar segmentation capability to ensure that the attribute
        --      bytesize remains under the message limit.

          marker
            <> word16BE (fromIntegral (baseSize + attributeLength + withdrawnLength + nlriLength))
            <> word8 2
            -- <> word16BE (fromIntegral withdrawnLength)
            <> ( if null withdrawnBuilders
                   then word16BE 0
                   else
                     word16BE (fromIntegral withdrawnLength)
                       <> head withdrawnBuilders
               ) -- there can be at most one withdrawn Builder in this case
            <> word16BE (fromIntegral attributeLength)
            <> attributeBuilder
            <> if null nlriBuilders then mempty else head nlriBuilders
        else -- there is enough stuff that more than one message is needed
        -- most likely not both nlri and withdrawn, but even if both present it is near enough optimal to simply split the two types and send seprately rather than attempt to cram both sorts in a single message
          foldMap buildWithdraw withdrawnBuilders <> foldMap buildNLRI nlriBuilders
