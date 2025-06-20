module BGPlib.AttoBGP where

import BGPlib.BGPMessage
import BGPlib.Capabilities (parseOptionalParameters)
import BGPlib.LibCommon (decode8, fromHostAddress)
import BGPlib.PathAttributeBuilder (attributesParser)
import BGPlib.Prefixes
import Control.Applicative (Alternative, liftA2, (<|>))
import Control.Monad (unless)
import qualified Data.Attoparsec.Binary as A
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Word

terminatingWireParser = wireParser1 <|> return B.empty

eosWireParser = B.null

wireParser :: Parser [B.ByteString]
wireParser = A.many' wireParser1 A.<?> "BGP wire format Parser"

wireParser1 :: Parser B.ByteString
wireParser1 = do
  A.string $ B.replicate 16 0xff
  length <- fromIntegral <$> A.anyWord16be
  typeCode <- A.peekWord8'
  if length < 19
    then fail "short length"
    else
      if length > 4096
        then fail $ "too long " ++ show length
        else
          if typeCode < 1 || typeCode > 4
            then fail $ "invalid type code (" ++ show typeCode ++ ")"
            else A.take (length - 18)

-- bgpParser = A.many1 bgpParser1 A.<?>  "BGP intermediate format Parser"

bgpParser :: Parser [BGPMessage]
bgpParser = many1Till bgpParser1 A.atEnd

{-# INLINE many1Till #-}
many1Till :: (Alternative f) => f a -> f b -> f [a]
many1Till p end = liftA2 (:) p scan
  where
    scan = (end *> pure []) <|> liftA2 (:) p scan

terminatingBGPParser = bgpParser1 <|> return BGPEndOfStream

eosBGPParser msg = BGPEndOfStream == msg

{-# INLINE bgpParser1 #-}
bgpParser1 :: Parser BGPMessage
bgpParser1 = do
  A.string $ B.replicate 16 0xff
  length <- fromIntegral <$> A.anyWord16be
  if length < 19
    then fail "short length"
    else
      if length > 4096
        then fail $ "too long " ++ show length
        else do
          typeCode <- A.anyWord8
          case typeCode of
            1 -> do
              msgVer <- A.anyWord8
              unless (msgVer == 4) (fail "Bad version(Open)")
              myAutonomousSystem <- A.anyWord16be
              holdTime <- A.anyWord16be
              bgpID <- A.anyWord32le
              optionalParametersLength <- A.anyWord8
              capabilities <- parseOptionalParameters optionalParametersLength
              return $ BGPOpen myAutonomousSystem holdTime (fromHostAddress bgpID) capabilities
            2 -> do
              withdrawnLength <- fromIntegral <$> A.anyWord16be
              withdrawn <- parsePrefixes withdrawnLength
              pathLength <- A.anyWord16be
              attributes <- attributesParser pathLength
              let nlriLength = length - withdrawnLength - fromIntegral pathLength - 23
              nlri <- parsePrefixes nlriLength
              return $ BGPUpdate withdrawn attributes nlri
            3 -> do
              errorCode <- A.anyWord8
              errorSubcode <- A.anyWord8
              errorData <- L.fromStrict <$> A.take (length - 21)
              return $ BGPNotify (decode8 errorCode) errorSubcode errorData
            4 -> if length == 19 then return BGPKeepalive else fail "invalid length in KeepAlive"
            _ -> fail $ "invalid type code (" ++ show typeCode ++ ")"

-- choosing different versions of the prefix parser via parseX:

-- parseX = inline parse1 -- this is the original recursive parser
parseX = parse2 -- this is the case statement parser
-- parseX = inline parse3 -- this is the simplified version of parse2

prefixBuilder (a, b) = mkPrefix a b

-- Attoparsec: Parse Update Prefixes
-- A list of prefixes is defined by its length in bytes (so misconstructed lists are feasible).
-- Thus a recursive parser must propagate the remaining buffer size.
-- Implementation
-- The signature of a recursive parser is thus: -}

-- parsePrefixes :: Int -> Parser [Prefix]
{-# INLINE parsePrefixes' #-}

parsePrefixes n = parsePrefixes' n []

-- parsePrefixes' :: Int -> [Prefix] -> Parser [Prefix]
-- The parser is initialized with an empty list and the buffer size.
-- The recursion terminates when the available buffer is ==0

parsePrefixes' 0 prefixes = return $ reverse prefixes
-- The normal path is:
parsePrefixes' n prefixes = do
  prefixBitLen <- A.anyWord8
  let prefixByteLen = fromIntegral $ (prefixBitLen + 7) `div` 8
  prefix <- parseX prefixByteLen
  parsePrefixes' (n - prefixByteLen - 1) (prefixBuilder (prefixBitLen, prefix) : prefixes)

-- This leaves it to define parse1:
--     parse1 :: Int -> Parser Word32
-- An inelegant implementation is a case statement over the byte length size of the prefix.
-- An elegant solution is recursive: depending on the endianness the accumulator is formed with the next value shifted in some way using the bytecount.
-- The trick is to ensure that the full shift is applied to a single or short byte cases.
-- One way to do this is to apply more shifts after a short case accumulator.
-- Note: the elegant approach extends to IPv6 naturally...  (when 128 bit registers are available!!)

{-# INLINE parse1a #-}
parse1a :: Word32 -> Int -> Parser Word32
-- The first parameter is the accumulator, the second reflects the remaining byte count.

parse1a acc 0 = return acc
parse1a acc byteIndex = do
  next <- fromIntegral <$> A.anyWord8
  parse1a (unsafeShiftL acc 8 .|. next) (byteIndex - 1)

-- The full prefix parser is thus
{-# INLINE parse1 #-}
parse1 :: Int -> Parser Word32
parse1 byteLength = flip unsafeShiftL (8 * (4 - byteLength)) <$> parse1a 0 byteLength

-- It is measured that when compared with merely returning ByteStrings for the prefixes this parser is significantly slower
--  - by about x10 - for 1,000,000 prefixes in 100,000 updates the respective performance was ~75mS vs ~750mS.
-- Alternates are possible, e.g. invoke a parser on a fixed length bytestring, or make the inner loop non-recursive (different action depeniding on the prefix len first byte).
-- Here is the implementation of the second of these:

{-# INLINE parse2 #-}
{- this INLINE has a big impact -}
parse2 :: Int -> Parser Word32
parse2 k = case k of
  0 -> return 0
  1 -> flip unsafeShiftL 24 . fromIntegral <$> A.anyWord8
  2 -> flip unsafeShiftL 16 . fromIntegral <$> A.anyWord16be
  3 -> flip unsafeShiftL 8 . fromIntegral <$> anyWord24be
  4 -> A.anyWord32be
  _ -> fail "impossible prefix length"
  where
    anyWord24be :: Parser Word32
    anyWord24be = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0 <$> A.take 3

-- Alternate encoding

{-# INLINE parse3 #-}
parse3 :: Int -> Parser Word32
parse3 k = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0 <$> A.take k
