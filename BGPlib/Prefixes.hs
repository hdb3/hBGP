{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module BGPlib.Prefixes(IPrefix,chunkPrefixes,toAddrRange,toIPrefix,fromIPrefix,fromAddrRange,lengthIPrefix) where
import Data.Binary
import Data.Hashable
import GHC.Generics(Generic)
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)
import Control.Monad(liftM)

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
newtype IPrefix = IPrefix Int deriving (Eq,Generic)

toIPrefix :: Int -> IPrefix
toIPrefix = IPrefix

mkIPrefix :: Word8 -> Word32 -> IPrefix
mkIPrefix l v = IPrefix $! unsafeShiftL (fromIntegral l) 32 .|. fromIntegral v

fromIPrefix :: IPrefix -> Int
fromIPrefix (IPrefix ipfx) = ipfx

lengthIPrefix :: IPrefix -> Int
lengthIPrefix (IPrefix ipfx) = fromIntegral $ unsafeShiftR ipfx 32

addressIPrefix :: IPrefix -> Word32
addressIPrefix (IPrefix ipfx) = fromIntegral $ 0xffffffff .&. ipfx

instance IsString IPrefix where
    fromString = read

instance Read IPrefix where
    readsPrec _ = readSipfx where
        readSipfx s = let (a,s') = head $ reads s in [(fromAddrRange a,s')]

instance Hashable IPrefix
instance Hashable IPv4
instance Hashable IPv6

instance {-# INCOHERENT #-} Show [IPrefix] where
    show = shorten

instance Show IPrefix where
    show = show.toAddrRange

realShow = show . map toAddrRange
shortenLim l pfxs = if length pfxs < (l+1) then realShow pfxs else show (take l pfxs) ++ "(+" ++ show (length pfxs - l) ++ ")"
-- shorten pfxs = if length pfxs < 3 then realShow pfxs else show (take 2 pfxs) ++ "(+" ++ show (length pfxs - 2) ++ ")"
shorten = shortenLim 4

toAddrRange :: IPrefix -> AddrRange IPv4
toAddrRange ipfx = makeAddrRange (fromHostAddress $ byteSwap32 $ addressIPrefix ipfx) (lengthIPrefix ipfx)

fromAddrRange :: AddrRange IPv4 -> IPrefix
fromAddrRange ar = mkIPrefix (fromIntegral subnet) (byteSwap32 $ toHostAddress ip) where
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
-}


-- segmentBinaryPrefix :: Int -> L.ByteString -> [L.ByteString]
-- segmentBinaryPrefix n lbs | L.length lbs <= n = [lbs]

-- encodedChunkPrefixes n = map encode . chunkPrefixes n . reverse
chunkPrefixes :: Int64 -> [IPrefix] -> [[IPrefix]]
chunkPrefixes n pfxs = let (xl,l,_) = chunkPrefixes' n pfxs in (l : xl)
chunkPrefixes' n = chunkEnumeratedPrefixes n . enumeratePrefixes

-- chunkEnumeratedPrefixes :: Int -> [(Int,Prefix)] -> ([[Prefix]],[Prefix],Int)
chunkEnumeratedPrefixes n = foldl f ([],[],0) where
    f (xl,l,accSize) (size,pfx) | accSize + size <= n = (xl,pfx : l, accSize + size)
                                | otherwise = (l:xl,[pfx],size)

-- enumeratePrefixes :: [Prefix] -> [(Int,Prefix)]
enumeratePrefixes = map (\pfx -> (fromIntegral $ lengthIPrefix pfx, pfx))




instance {-# OVERLAPPING #-} Binary [IPrefix] where
    put = putn
    get = getn

instance Binary IPrefix where

    put ipfx | subnet == 0 = putWord8 0
             | subnet < 9  = do putWord8 subnet
                                putWord8 (fromIntegral $ unsafeShiftR ip 24)
             | subnet < 17 = do putWord8 subnet
                                putWord16be  (fromIntegral $ unsafeShiftR ip 16)
             | subnet < 25 = do putWord8 subnet
                                putWord16be  (fromIntegral $ unsafeShiftR ip 16)
                                putWord8 (fromIntegral $ unsafeShiftR ip 8)
             | otherwise   = do putWord8 subnet
                                putWord32be  ip
        where subnet = fromIntegral $ lengthIPrefix ipfx
              ip = addressIPrefix ipfx

    get = label "Prefix" $ do
        subnet <- getWord8
        if subnet == 0
        then return $ mkIPrefix 0 0
        else if subnet < 9
        then do
            w8 <- getWord8
            let ip = unsafeShiftL (fromIntegral w8 :: Word32) 24
            return $ mkIPrefix subnet ip
        else if subnet < 17
        then do
            w16  <- getWord16be
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16
            return $ mkIPrefix subnet ip
        else if subnet < 25
        then do
            w16  <- getWord16be
            w8  <- getWord8
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16 .|.
                     unsafeShiftL (fromIntegral w8 :: Word32) 8
            return $ mkIPrefix subnet ip
        else mkIPrefix subnet <$> getWord32be
