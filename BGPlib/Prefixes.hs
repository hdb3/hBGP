{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
module BGPlib.Prefixes(XPrefix(..),Prefix(..),chunkPrefixes,toAddrRange,pathID,mkzXPrefix,mkXPrefix,mkPrefix,prefix,toPrefix,fromPrefix,fromAddrRange,lengthPrefix) where
import Data.Binary
import Data.Hashable
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)

import BGPlib.LibCommon

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

newtype Prefix = Prefix Int deriving (Eq,Generic)

{-# INLINE toPrefix #-}
-- TODO - enquire why this function need exist...
toPrefix :: Int -> Prefix
toPrefix = Prefix

{- # INLINE _mkPrefix # -}
{-
_mkPrefix :: Word8 -> Word32 -> Prefix
_mkPrefix l v = Prefix $! unsafeShiftL (fromIntegral l) 32 .|. fromIntegral v
-}
{-# INLINE mkPrefix #-}
mkPrefix :: Word8 -> Word32 -> Prefix
mkPrefix l v = Prefix $! unsafeShiftL (fromIntegral l) 32 .|. fromIntegral v

{-# INLINE fromPrefix #-}
fromPrefix :: Prefix -> Int
fromPrefix (Prefix pfx) = pfx

{-# INLINE lengthPrefix #-}
lengthPrefix :: Prefix -> Int
lengthPrefix (Prefix pfx) = fromIntegral $ unsafeShiftR pfx 32

{-# INLINE addressPrefix #-}
addressPrefix :: Prefix -> Word32
addressPrefix (Prefix pfx) = fromIntegral $ 0xffffffff .&. pfx

instance IsString Prefix where
    fromString = read

instance Read Prefix where
    readsPrec _ = readSpfx where
        readSpfx s = let (a,s') = head $ reads s in [(fromAddrRange a,s')]

instance Hashable Prefix
instance Hashable IPv4
instance Hashable IPv6

instance {-# INCOHERENT #-} Show [Prefix] where
    show = shorten

instance Show Prefix where
    show = show . toAddrRange

realShow = show . map toAddrRange
shortenLim l pfxs = if length pfxs < (l+1) then realShow pfxs else show (take l pfxs) ++ "(+" ++ show (length pfxs - l) ++ ")"
-- shorten pfxs = if length pfxs < 3 then realShow pfxs else show (take 2 pfxs) ++ "(+" ++ show (length pfxs - 2) ++ ")"
shorten = shortenLim 4

{-# INLINE toAddrRange #-}
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange pfx = makeAddrRange (fromHostAddress $ byteSwap32 $ addressPrefix pfx) (lengthPrefix pfx)

{-# INLINE fromAddrRange #-}
fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = mkPrefix (fromIntegral subnet) (byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar

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

    RFC 7911 - Advertisement of Multiple Paths in BGP

    3.  Extended NLRI Encodings

   In order to carry the Path Identifier in an UPDATE message, the NLRI
   encoding MUST be extended by prepending the Path Identifier field,
   which is of four octets.

   For example, the NLRI encoding specified in [RFC4271] is extended as
   the following:

                  +--------------------------------+
                  | Path Identifier (4 octets)     |
                  +--------------------------------+
                  | Length (1 octet)               |
                  +--------------------------------+
                  | Prefix (variable)              |
                  +--------------------------------+
-}

-- XPrefix definitions follow
-- XPrefix encapsulates the wire format ('nlri')
-- and supports the PathID augmented representation
-- In a basic implementation XPrefix is a null wrapper of Prefix
-- It is expected that ADDPATH aware code would operate correctly over a non-addpath session
-- by using zero pathID - diveregnt behaviour is signalled by use of 

data XPrefix = XPrefix Word32 Prefix deriving Eq
instance Show XPrefix where
    show (XPrefix a b) = show (a,b)

{-# INLINE mkXPrefix #-}
mkXPrefix :: Word32 -> Prefix -> XPrefix
mkXPrefix = XPrefix

{-# INLINE mkzXPrefix #-}
mkzXPrefix :: Prefix -> XPrefix
mkzXPrefix = mkXPrefix 0

{-# INLINE prefix #-}
prefix :: XPrefix -> Prefix
prefix (XPrefix _ pfx) = pfx

{-# INLINE pathID #-}
pathID :: XPrefix -> Word32
pathID (XPrefix pid _) = pid

chunkPrefixes :: Int64 -> [XPrefix] -> [[XPrefix]]
chunkPrefixes n pfxs = let (xl,l,_) = chunkPrefixes' pfxs in (l : xl)

    where
    chunkPrefixes' = chunkEnumeratedPrefixes . enumeratePrefixes

    chunkEnumeratedPrefixes = foldl f ([],[],0) where
        f (xl,l,accSize) (size,pfx) | accSize + size <= n = (xl,pfx : l, accSize + size)
                                    | otherwise = (l:xl,[pfx],size)

    enumeratePrefixes = map (\pfx -> (getLength pfx, pfx)) where
        getLength (XPrefix _ pfx) = fromIntegral $ 6 + (lengthPrefix pfx - 1) `div` 8

instance Binary XPrefix where

    put (XPrefix pathID pfx)
            | subnet == 0 = do putWord32be pathID
                               putWord8 0
            | subnet < 9  = do putWord32be pathID
                               putWord8 subnet
                               putWord8 (fromIntegral $ unsafeShiftR ip 24)
            | subnet < 17 = do putWord32be pathID
                               putWord8 subnet
                               putWord16be  (fromIntegral $ unsafeShiftR ip 16)
            | subnet < 25 = do putWord32be pathID
                               putWord8 subnet
                               putWord16be  (fromIntegral $ unsafeShiftR ip 16)
                               putWord8 (fromIntegral $ unsafeShiftR ip 8)
            | otherwise   = do putWord32be pathID
                               putWord8 subnet
                               putWord32be  ip
        where subnet = fromIntegral $ lengthPrefix pfx
              ip = addressPrefix pfx

    get = undefined

instance {-# OVERLAPPING #-} Binary [XPrefix] where
    put = putn
    get = getn
