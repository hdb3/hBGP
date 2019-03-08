{-# LANGUAGE OverloadedStrings #-}
module PrefixSource where
import Data.IP
import Data.Word
import Data.Bits
import Control.Concurrent
import Control.Monad

initSource :: AddrRange IPv4 -> Word32 -> Word32 -> IO (IO (Word32, Word32, [AddrRange IPv4]))
initSource startPrefix tableSize groupSize = do
    mv <- newMVar  0 -- (0 :: Word32)
    print $ addrRangePair startPrefix
    let f mv = do
             n <- takeMVar mv
             putMVar mv $ n + 1
             return $ prefixes startPrefix tableSize groupSize n
    return (f mv)


prefixes :: AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> (Word32, Word32, [AddrRange IPv4])
prefixes startPrefix tableSize groupSize n = (n `div` tableSize, n `mod` tableSize, group startPrefix groupSize $ n `mod` tableSize)

    where
    group startPrefix groupSize index = map ip4 $ seeds (ip4' ip) groupSize index
    (ip,prefixLength) = addrRangePair startPrefix
    ip4  =  (flip makeAddrRange) prefixLength . fromHostAddress . byteSwap32 . (flip shiftL) (32-prefixLength)
    ip4' = (flip shiftR) (32-prefixLength) . byteSwap32 . toHostAddress
    seeds base groupSize index = map (base + index * groupSize +) [0..groupSize-1]

main = do
    s <- initSource "192.168.0.0/24" 8 3
    replicateM 10 s >>= print
