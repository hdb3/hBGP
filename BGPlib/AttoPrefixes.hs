module Prefixes where

import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString.Base16
import Data.Word

type Prefix = (Word8, Word32)

-- Attoparsec: Parse Update Prefixes
-- A list of prefixes is defined by its length in bytes (so misconstructed lists are feasible).
-- Thus a recursive parser must propagate the remaining buffer size.
-- Implementation
-- The signature of a recursive parser is thus: -}

parsePrefixes :: Int -> [Prefix] -> Parser [Prefix]
-- The parser is initialized with an empty list and the buffer size.
-- The recursion terminates when the available buffer is ==0 (or -ve as invalid).

parsePrefixes 0 prefixes = return prefixes
-- The normal path is:
parsePrefixes n prefixes = do
  prefixBitLen <- anyWord8
  let prefixByteLen = fromIntegral $ (prefixBitLen + 7) `div` 8
  prefix <- parse1 prefixByteLen
  parsePrefixes (n - prefixByteLen) ((prefixBitLen, prefix) : prefixes)

-- This leaves it to define parse1:
--     parse1 :: Int -> Parser Word32
-- An inelegant implementation is a case statement over the byte length size of the prefix.
-- An elegant solution is recursive: depending on the endianness the accumulator is formed with the next value shifted in some way using the bytecount.
-- The trick is to ensure that the full shift is applied to a single or short byte cases.
-- One way to do this is to apply more shifts after a short case accumulator.
-- Note: the elegant approach extends to IPv6 naturally...  (when 128 bit registers are available!!)

parse1a :: Word32 -> Int -> Parser Word32
--The first parmeter is the accumulator, the second reflects the remaining byte count.

parse1a acc 0 = return acc
parse1a acc byteIndex = do
  next <- fromIntegral <$> anyWord8
  parse1a (unsafeShiftL acc 8 .|. next) (byteIndex -1)

--The full prefix parser is thus
parse1 :: Int -> Parser Word32
parse1 byteLength = flip unsafeShiftL (8 * (4 - byteLength)) <$> parse1a 0 byteLength

fromHex = fst . Data.ByteString.Base16.decode

main = do
  parseTest (parse1 0) $ fromHex ""
  parseTest (parse1 1) $ fromHex "10"
