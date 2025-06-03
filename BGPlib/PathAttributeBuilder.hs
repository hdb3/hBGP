{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module BGPlib.PathAttributeBuilder where

import BGPlib.ASPath
import BGPlib.Codes
import BGPlib.LibCommon
import BGPlib.PathAttributes
import BGPlib.Prefixes ()
import ByteString.StrictBuilder
import Control.Monad (unless)
import qualified Data.Attoparsec.Binary as A
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid (mempty)
import Data.Word
import Debug.Trace

decodePathAttributes :: B.ByteString -> [PathAttribute]
decodePathAttributes bs = let Right msgs = A.parseOnly (attributesParser (fromIntegral $ B.length bs)) bs in msgs

encodePathAttributes :: [PathAttribute] -> B.ByteString
encodePathAttributes = builderBytes . buildPathAttributes

buildPathAttributes = foldMap buildPathAttribute
  where
    buildCommon :: PathAttributeTypeCode -> Word8 -> Builder
    buildCommon code length = word8 (flagsOf code) <> word8 (encode8 code) <> word8 length
    buildExtended :: PathAttributeTypeCode -> Word16 -> Builder
    buildExtended code length = word8 (setExtended $ flagsOf code) <> word8 (encode8 code) <> word16BE length
    buildBuilder :: PathAttributeTypeCode -> Builder -> Builder
    buildBuilder code b = word8 (setExtended $ flagsOf code) <> word8 (encode8 code) <> word16BE (fromIntegral $ builderLength b) <> b
    buildAttributeNull :: PathAttributeTypeCode -> Builder
    buildAttributeNull code = buildCommon code 0
    buildAttributeWord8 :: PathAttributeTypeCode -> Word8 -> Builder
    buildAttributeWord8 code v = buildCommon code 1 <> word8 v
    buildAttributeWord32 :: PathAttributeTypeCode -> Word32 -> Builder
    buildAttributeWord32 code v = buildCommon code 4 <> word32BE v
    buildAttributeWords32 :: PathAttributeTypeCode -> [Word32] -> Builder
    buildAttributeWords32 code ws | 63 > length ws = buildCommon code (fromIntegral $ 4 * length ws) <> foldMap word32BE ws
    buildAttributeWords32 code ws = trace ("unexpected format for buildAttributeWords32 " ++ show code) mempty
    buildAttributeWords64 :: PathAttributeTypeCode -> [Word64] -> Builder
    buildAttributeWords64 code ws | 31 > length ws = buildCommon code (fromIntegral $ 8 * length ws) <> foldMap word64BE ws
    buildAttributeWords64 code ws = trace ("unexpected format for buildAttributeWords64 " ++ show code) mempty

    buildAttributeWord64 :: PathAttributeTypeCode -> Word64 -> Builder
    buildAttributeWord64 code v = buildCommon code 8 <> word64BE v
    buildAttributeAggregator :: PathAttributeTypeCode -> Word32 -> Word32 -> Builder
    buildAttributeAggregator code as bgpid = buildCommon code 8 <> word32BE as <> word32BE bgpid
    buildLargeCommunities :: [(Word32, Word32, Word32)] -> Builder
    buildLargeCommunities lgx =
      let word96 (a, b, c) = word32BE a <> word32BE b <> word32BE c
       in buildExtended TypeCodePathAttributeLargeCommunity (fromIntegral $ 12 * length lgx) <> foldMap word96 lgx
    buildPathAttribute (PathAttributeOrigin a) = buildAttributeWord8 TypeCodePathAttributeOrigin a
    buildPathAttribute (PathAttributeNextHop a) = buildAttributeWord32 TypeCodePathAttributeNextHop (byteSwap32 $ toHostAddress a)
    buildPathAttribute (PathAttributeMultiExitDisc a) = buildAttributeWord32 TypeCodePathAttributeMultiExitDisc a
    buildPathAttribute (PathAttributeLocalPref a) = buildAttributeWord32 TypeCodePathAttributeLocalPref a
    buildPathAttribute (PathAttributeASPath a) = buildBuilder TypeCodePathAttributeASPath (buildASPath a)
    buildPathAttribute PathAttributeAtomicAggregate = buildAttributeNull TypeCodePathAttributeAtomicAggregate
    buildPathAttribute (PathAttributeAggregator (as, bgpid)) = buildAttributeAggregator TypeCodePathAttributeAggregator as (toHostAddress bgpid)
    buildPathAttribute (PathAttributeCommunities a) = buildAttributeWords32 TypeCodePathAttributeCommunities a -- PathAttributeCommunities [Word32]
    buildPathAttribute (PathAttributeExtendedCommunities a) = buildAttributeWords64 TypeCodePathAttributeExtendedCommunities a -- PathAttributeExtendedCommunities [Word64]
    buildPathAttribute (PathAttributeAS4Path a) = buildBuilder TypeCodePathAttributeASPath (buildASPath a)
    buildPathAttribute (PathAttributeLargeCommunity a) = buildLargeCommunities a -- PathAttributeLargeCommunity [LargeCommunity] ~ [(Word32,Word32,Word32)]
    buildPathAttribute (PathAttributeAS4Aggregator (a, b)) = buildAttributeAggregator TypeCodePathAttributeAS4Aggregator a b -- PathAttributeAS4Aggregator (Word32,Word32)
    buildPathAttribute (PathAttributeASPathlimit a) = buildExtended TypeCodePathAttributeASPathlimit (fromIntegral $ B.length a) <> bytes a
    buildPathAttribute (PathAttributeAttrSet a) = buildExtended TypeCodePathAttributeAttrSet (fromIntegral $ B.length a) <> bytes a
    buildPathAttribute (PathAttributeConnector ax) = trace ("PathAttributeConnector: " ++ BS8.unpack (B16.encode ax)) mempty
    buildPathAttribute (PathAttributeUnknown a ax) = trace ("PathAttributeUnknown: " ++ BS8.unpack (B16.encode ax)) mempty
    buildPathAttribute x = error $ "Unexpected type code: " ++ show x

attributesParser :: Word16 -> Parser [PathAttribute]
attributesParser n
  | n == 0 = return []
  | n < 3 = error $ "attributesParser: invalid length: " ++ show n
  | otherwise = do
      flags <- A.anyWord8
      code' <- A.anyWord8
      let code = decode8 code'
      (hdrLen, len) <-
        if extendedBitTest flags
          then (4,) <$> A.anyWord16be
          else (3,) . fromIntegral <$> A.anyWord8
      unless (flagCheck flags code) (error $ "Bad Flags - flags=" ++ show flags ++ " code=" ++ show code' ++ " (" ++ show code ++ ")")
      if n < len + hdrLen
        then error $ "attributesParser: invalid length n=" ++ show n ++ " len=" ++ show len ++ " code=" ++ show code
        else do
          attr <- attributeParser len code code'
          attrs <- attributesParser (n - hdrLen - len)
          return $ attr : attrs
  where
    {-# INLINE attributeParser #-}
    attributeParser :: Word16 -> PathAttributeTypeCode -> Word8 -> Parser PathAttribute
    attributeParser len code code' =
      if
        | TypeCodePathAttributeOrigin == code -> do
            unless (len == 1) (error "Bad Length")
            v <- A.anyWord8
            unless (v < 3) (error "Bad Origin Code")
            return $ PathAttributeOrigin v
        | TypeCodePathAttributeASPath == code -> PathAttributeASPath <$> parseASPath len
        | TypeCodePathAttributeNextHop == code -> PathAttributeNextHop . fromHostAddress <$> A.anyWord32le
        | TypeCodePathAttributeMultiExitDisc == code ->
            PathAttributeMultiExitDisc <$> A.anyWord32be
        | TypeCodePathAttributeLocalPref == code ->
            PathAttributeLocalPref <$> A.anyWord32be
        | TypeCodePathAttributeAtomicAggregate == code -> return PathAttributeAtomicAggregate
        | TypeCodePathAttributeAggregator == code -> do
            as <-
              if
                | len == 6 -> fromIntegral <$> A.anyWord16be
                | len == 8 -> A.anyWord32be
                | otherwise -> error $ "Bad length in PathAttributeAggregator: " ++ show len
            bgpid <- A.anyWord32be
            return $ PathAttributeAggregator (as, fromHostAddress bgpid)
        | TypeCodePathAttributeCommunities == code -> PathAttributeCommunities <$> A.count (fromIntegral $ len `div` 4) A.anyWord32be
        | TypeCodePathAttributeMPREachNLRI == code -> PathAttributeMPREachNLRI <$> A.take (fromIntegral len)
        | TypeCodePathAttributeMPUnreachNLRI == code -> PathAttributeMPUnreachNLRI <$> A.take (fromIntegral len)
        | TypeCodePathAttributeExtendedCommunities == code -> PathAttributeExtendedCommunities <$> A.count (fromIntegral $ len `div` 8) A.anyWord64be
        | TypeCodePathAttributeAS4Path == code -> PathAttributeAS4Path <$> parseASPath len
        | TypeCodePathAttributeAS4Aggregator == code ->
            PathAttributeAS4Aggregator <$> do
              w0 <- A.anyWord32be
              w1 <- A.anyWord32be
              return (w0, w1)
        | TypeCodePathAttributeConnector == code -> PathAttributeConnector <$> A.take (fromIntegral len)
        | TypeCodePathAttributeASPathlimit == code -> PathAttributeASPathlimit <$> A.take (fromIntegral len)
        | TypeCodePathAttributeLargeCommunity == code -> PathAttributeLargeCommunity <$> A.count (fromIntegral $ len `div` 12) get3word32be
        | TypeCodePathAttributeAttrSet == code -> PathAttributeAttrSet <$> A.take (fromIntegral len)
        | TypeCodePathAttributeUnknown == code -> PathAttributeUnknown code' <$> A.take (fromIntegral len)
    get3word32be = do
      w0 <- A.anyWord32be
      w1 <- A.anyWord32be
      w2 <- A.anyWord32be
      return (w0, w1, w2)
