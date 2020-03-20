{-# LANGUAGE FlexibleInstances #-}

module MRTPrefixes where
import Data.Word
import Data.IP
import Data.String(IsString,fromString)


-- derived from the 'bgplib' file of the same name (Prefixes.hs)
-- it is made standalone and removes the Data.Binary encoding functions, arguably a proper sepration of concern
-- the immediate purpose is to support seprate compilation of the 'Overlap' functions

data Prefix = Prefix !(Word8,Word32) deriving Eq
--newtype Prefix = Prefix (Word8,Word32) deriving Eq
--type Prefix = (Word8,Word32)
toPrefix :: Word8 -> Word32 -> Prefix
toPrefix a b = Prefix (a,b)

fromPrefix :: Prefix -> (Word8, Word32) 
fromPrefix (Prefix (a,b)) = (a, b) 

{-# INLINE subnet #-}
subnet :: Prefix -> Word8 
subnet (Prefix (a,_)) = a

instance {-# INCOHERENT #-} IsString Prefix where
    fromString = read

instance Read Prefix where
    readsPrec _ = readSpfx where
        readSpfx s = let (a,s') = head $ reads s in [(fromAddrRange a,s')]

instance {-# INCOHERENT #-} Show Prefix where
    show = show.fromPrefix

--subnetOf :: Prefix -> Word8
--subnetOf (Prefix (s,_)) = s

--ipOf :: Prefix -> Word32
--ipOf (Prefix (_,i)) = i
 
toAddrRange :: Prefix -> AddrRange IPv4
toAddrRange (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)
--fromPrefix (Prefix (subnet,ip)) = makeAddrRange (fromHostAddress $ byteSwap32 ip) (fromIntegral subnet)

--toPrefix ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
fromAddrRange :: AddrRange IPv4 -> Prefix
fromAddrRange ar = Prefix (fromIntegral subnet, byteSwap32 $ toHostAddress ip) where
                   (ip,subnet) = addrRangePair ar
