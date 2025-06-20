{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module BGPlib.Prefixes
  ( Prefix,
    toAddrRange,
    mkPrefix,
    toPrefix,
    fromPrefix,
    fromAddrRange,
    lengthPrefix,
    addressPrefix,
  )
where

import BGPlib.LibCommon
import Data.Bits
import Data.Hashable
import Data.String (IsString, fromString)
import Data.Word

-- IMPORTANT Note re byte ordering
-- x86 byte ordering is inverted w.r.t. network order,
-- The get/put 16/32/64be primitive
-- operations mask this truth, and at register level big endianness behaviour is observed
-- The 'be' ~ Big Endian refers to the external representation, not the internal one!
-- So for example the type Prefix holds IPv4 addresses as 32 bit words in little endian form
-- internally on x86 but get/putWord32be is required to interwork with network order protocols

-- *** HOWEVER *** ...

-- The 'HostAddress' type from Network.Socket, via Data.IP, is a 32 bit word holding IPv4
-- addresses IN REVERSE ORDER !!!
-- Therefore conversions between our Prefix type and HostAddress require byteSwap32
--
-- representation of prefixes as 64 bit words: this mapping allows prefixes to be treated as Ints where useful

-- todo make this Word64 not Int?
newtype Prefix = Prefix Int deriving (Eq, Generic)

{-# INLINE toPrefix #-}
toPrefix :: Int -> Prefix
toPrefix = Prefix

{-# INLINE mkPrefix #-}
mkPrefix :: Word8 -> Word32 -> Prefix
mkPrefix l v = Prefix $! unsafeShiftL (fromIntegral l) 32 .|. fromIntegral v

{-# INLINE fromPrefix #-}
fromPrefix :: Prefix -> Int
fromPrefix (Prefix pfx) = pfx

{-# INLINE lengthPrefix #-}
lengthPrefix :: Prefix -> Word8
lengthPrefix (Prefix pfx) = fromIntegral $ 0x000000ff .&. unsafeShiftR pfx 32

{-# INLINE addressPrefix #-}
addressPrefix :: Prefix -> Word32
addressPrefix (Prefix pfx) = fromIntegral $ 0xffffffff .&. pfx

instance IsString Prefix where
  fromString = read

instance Read Prefix where
  readsPrec _ = readSpfx
    where
      -- readSpfx s = let (a, s') = head $ reads s in [(fromAddrRange a, s')]
      -- refactored to eliminate head, untested...
      readSpfx s = case reads s of
        [] -> []
        ((a, s') : _) -> [(fromAddrRange a, s')]

instance Hashable Prefix

instance Hashable IPv4

instance Hashable IPv6

instance {-# INCOHERENT #-} Show [Prefix] where
  show = shorten

instance Show Prefix where
  show = show . toAddrRange

realShow = show . map toAddrRange

shortenLim l pfxs = if length pfxs < (l + 1) then realShow pfxs else show (take l pfxs) ++ "(+" ++ show (length pfxs - l) ++ ")"

shorten = shortenLim 4

{-# INLINE toAddrRange #-}
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange pfx = makeAddrRange (fromHostAddress $ byteSwap32 $ addressPrefix pfx) (fromIntegral $ lengthPrefix pfx)

{-# INLINE fromAddrRange #-}
fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = mkPrefix (fromIntegral subnet) (byteSwap32 $ toHostAddress ip)
  where
    (ip, subnet) = addrRangePair ar

-- binary format for attributes is 1 byte flags, 1 byte type code, 1 or 2 byte length value depending on a flag bit, then payload
{-RFC4271 page 20:
 -
 -          Reachability information is encoded as one or more 2-tuples of
         the form <length, prefix>, whose fields are described below:

                  +---------------------------+
                  |   Length (1 octet)        |
                  +---------------------------+
                  |   Prefix (variable)       |
                  +---------------------------+

         The use and the meaning of these fields are as follows:

         a) Length:

            The Length field indicates the length in bits of the IP
            address prefix.  A length of zero indicates a prefix that
            matches all IP addresses (with prefix, itself, of zero
            octets).

         b) Prefix:

            The Prefix field contains an IP address prefix, followed by
            enough trailing bits to make the end of the field fall on an
            octet boundary.  Note that the value of the trailing bits is
            irrelevant.
-}
