{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IP4Prefix where
import Data.Word
import Data.Bits
import Data.IP
import Data.String(IsString,fromString)
import Prefix

instance Prefix IP4Prefix where
    fromInt = fromIntegral
    toInt = fromIntegral

fromInt :: Int -> IP4Prefix
fromInt = fromIntegral

toInt :: IP4Prefix -> Int
toInt = fromIntegral


ipFromInt :: Word64 -> Word32
ipFromInt w64 = fromIntegral $ unsafeShiftR w64 32
subnetFromInt :: Word64 -> Word8
subnetFromInt w64 = fromIntegral $ 0xff .&. w64
intFromIpSubnet ip subnet = unsafeShiftL ip 32 .|. subnet
 
toAddrRange :: IP4Prefix -> AddrRange IPv4
toAddrRange (IP4Prefix w64) = makeAddrRange (fromHostAddress $ ipFromInt w64) (fromIntegral $ subnetFromInt w64)

fromAddrRange :: AddrRange IPv4 -> IP4Prefix
fromAddrRange ar = IP4Prefix $ intFromIpSubnet (fromIntegral $ toHostAddress ip) (fromIntegral subnet) where
                   (ip,subnet) = addrRangePair ar
 
newtype IP4Prefix = IP4Prefix Word64 deriving (Eq,Enum,Ord,Num,Real,Integral)

instance Show IP4Prefix where
    show = show . toAddrRange

instance Read IP4Prefix where
    readsPrec _ = readSpfx where
        readSpfx s = let (a,s') = head $ readsPrec 0 s in [(fromAddrRange a,s')]

instance IsString IP4Prefix where
    fromString = read
