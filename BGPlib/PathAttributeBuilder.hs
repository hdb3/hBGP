{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module BGPlib.PathAttributeBuilder where

import BGPlib.ASPath
import BGPlib.Codes
import BGPlib.LibCommon
import BGPlib.PathAttributes
import BGPlib.Prefixes ()
import Data.ByteString.Builder
import Data.Word

buildCommon :: PathAttributeTypeCode -> Word8 -> Builder
buildCommon code length = word8 (flagsOf code) <> word8 (encode8 code) <> word8 length

buildExtended :: PathAttributeTypeCode -> Word16 -> Builder
buildExtended code length = word8 (setExtended $ flagsOf code) <> word8 (encode8 code) <> word16BE length

buildPathCommon :: PathAttributeTypeCode -> (Word16, Builder) -> Builder
buildPathCommon code (length, b) = word8 (setExtended $ flagsOf code) <> word8 (encode8 code) <> word16BE length <> b

buildAttributeNull :: PathAttributeTypeCode -> Builder
buildAttributeNull code = buildCommon code 0

buildAttributeWord8 :: PathAttributeTypeCode -> Word8 -> Builder
buildAttributeWord8 code v = buildCommon code 1 <> word8 v

buildAttributeWord32 :: PathAttributeTypeCode -> Word32 -> Builder
buildAttributeWord32 code v = buildCommon code 4 <> word32BE v

buildAttributeWords32 :: PathAttributeTypeCode -> [Word32] -> Builder
buildAttributeWords32 code ws | 63 > length ws = buildCommon code (fromIntegral $ 4 * length ws) <> foldr ((<>) . word32BE) mempty ws

buildAttributeWords64 :: PathAttributeTypeCode -> [Word64] -> Builder
buildAttributeWords64 code ws | 31 > length ws = buildCommon code (fromIntegral $ 8 * length ws) <> foldr ((<>) . word64BE) mempty ws

buildAttributeWord64 :: PathAttributeTypeCode -> Word64 -> Builder
buildAttributeWord64 code v = buildCommon code 8 <> word64BE v

buildAttributeAggregator :: PathAttributeTypeCode -> Word32 -> Word32 -> Builder
buildAttributeAggregator code as bgpid = buildCommon code 8 <> word32BE as <> word32BE bgpid

buildLargeCommunities :: [(Word32, Word32, Word32)] -> Builder
buildLargeCommunities lgx = buildExtended TypeCodePathAttributeLargeCommunity (fromIntegral $ 12 * length lgx) <> foldr ((<>) . word96) mempty lgx
  where
    word96 (a, b, c) = word32BE a <> word32BE b <> word32BE c

-- -- use for attributes upto permitted maximum of 65535 bytes in length
-- buildAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Builder
-- buildAttributeByteString code b = buildExtended code (fromIntegral $ L.length b) <> lazyByteString b

-- -- use for attributes known to be less than 256 bytes in length
-- buildShortAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Builder
-- buildShortAttributeByteString code b = buildCommon code (fromIntegral $ L.length b) <> lazyByteString b

-- use for attributes not known to be less than 256 bytes in length
-- buildFlexAttributeByteString :: PathAttributeTypeCode -> L.ByteString -> Builder
-- buildFlexAttributeByteString code b
--   | L.length b > 255 = buildAttributeByteString code b
--   | otherwise = buildShortAttributeByteString code b

buildPathAttribute (PathAttributeOrigin a) = buildAttributeWord8 TypeCodePathAttributeOrigin a
buildPathAttribute (PathAttributeNextHop a) = buildAttributeWord32 TypeCodePathAttributeNextHop (byteSwap32 $ toHostAddress a)
buildPathAttribute (PathAttributeMultiExitDisc a) = buildAttributeWord32 TypeCodePathAttributeMultiExitDisc a
buildPathAttribute (PathAttributeLocalPref a) = buildAttributeWord32 TypeCodePathAttributeLocalPref a
-- PathAttributeASPath ASPath
buildPathAttribute (PathAttributeASPath a) = buildPathCommon TypeCodePathAttributeASPath (buildASPath a)
buildPathAttribute (PathAttributeAtomicAggregate) = buildAttributeNull TypeCodePathAttributeAtomicAggregate
buildPathAttribute (PathAttributeAggregator (as, bgpid)) = buildAttributeAggregator TypeCodePathAttributeAggregator as (toHostAddress bgpid)
-- PathAttributeCommunities [Word32]
buildPathAttribute (PathAttributeCommunities a) = buildAttributeWords32 TypeCodePathAttributeCommunities a
--PathAttributeExtendedCommunities [Word64]
buildPathAttribute (PathAttributeExtendedCommunities a) = buildAttributeWords64 TypeCodePathAttributeExtendedCommunities a
-- PathAttributeAS4Path ASPath
buildPathAttribute (PathAttributeAS4Path a) = buildPathCommon TypeCodePathAttributeASPath (buildASPath a)
-- PathAttributeLargeCommunity [LargeCommunity] ~ [(Word32,Word32,Word32)]
buildPathAttribute (PathAttributeLargeCommunity a) = buildLargeCommunities a
-- PathAttributeAS4Aggregator (Word32,Word32)
buildPathAttribute (PathAttributeAS4Aggregator (a, b)) = buildAttributeAggregator TypeCodePathAttributeAS4Aggregator a b
-- -- PathAttributeASPathlimit B.ByteString
-- buildPathAttribute (PathAttributeASPathlimit a) = buildAttributeByteString TypeCodePathAttributeASPathlimit a

-- -- PathAttributeAttrSet B.ByteString
-- buildPathAttribute (PathAttributeAttrSet a) = buildAttributeByteString TypeCodePathAttributeAttrSet a

buildPathAttribute x = error $ "Unexpected type code: " ++ show x
-- --     get = label "PathAttribute" $ do
--              flags <- getWord8
--              code'  <- getWord8
--              let code = decode8 code'
--              len <- if extendedBitTest flags then do l <- getWord16be
--                                                      return (fromIntegral l :: Int)
--                                              else do l <- getWord8
--                                                      return (fromIntegral l :: Int)
--              unless (flagCheck flags code) (fail $ "Bad Flags - flags=" ++ show flags ++ " code=" ++ show code' ++ " (" ++ show code ++ ")")

--              if | TypeCodePathAttributeOrigin == code -> do
--                  unless (len == 1) (fail "Bad Length")
--                  v  <- getWord8
--                  unless (v < 3) (fail "Bad Origin Code")
--                  return $ PathAttributeOrigin v

--                 | TypeCodePathAttributeASPath == code -> do
--                     bs <- getLazyByteString (fromIntegral len)
--                     return $ PathAttributeASPath (decode bs)

--                 | TypeCodePathAttributeNextHop == code ->
--                     PathAttributeNextHop . fromHostAddress <$> getWord32le
--                   -- v <- getWord32le
--                   -- return $ PathAttributeNextHop (fromHostAddress v)

--                 | TypeCodePathAttributeMultiExitDisc == code ->
--                       PathAttributeMultiExitDisc <$> getWord32be

--                 | TypeCodePathAttributeLocalPref == code ->
--                       PathAttributeLocalPref <$> getWord32be

--                 | TypeCodePathAttributeAtomicAggregate == code -> return PathAttributeAtomicAggregate

--                 | TypeCodePathAttributeAggregator == code -> do
--                     as <- if len == 6 then
--                         fromIntegral <$> getWord32be
--                     else if len == 8 then
--                         getWord32be
--                     else fail $ "Bad length in PathAttributeAggregator: " ++ show len
--                     bgpid <- getWord32le
--                     return $ PathAttributeAggregator (as,fromHostAddress bgpid)

--                 | TypeCodePathAttributeCommunities == code -> do
--                     ws <- getMany ( len `div` 4)
--                     return $ PathAttributeCommunities ws

--                 | TypeCodePathAttributeMPREachNLRI == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     return $ PathAttributeMPREachNLRI bs

--                 | TypeCodePathAttributeMPUnreachNLRI == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     return $ PathAttributeMPUnreachNLRI bs

--                 | TypeCodePathAttributeExtendedCommunities == code -> do
--                     ws <- getMany ( len `div` 8)
--                     return $ PathAttributeExtendedCommunities ws

--                 | TypeCodePathAttributeAS4Path == code -> do
--                     bs <- getLazyByteString (fromIntegral len)
--                     return $ PathAttributeAS4Path (decode bs)

--                 | TypeCodePathAttributeAS4Aggregator == code -> do
--                     v1 <- getWord32be
--                     v2 <- getWord32be
--                     return $ PathAttributeAS4Aggregator (v1,v2)

--                 | TypeCodePathAttributeConnector == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     return $ PathAttributeConnector bs

--                 | TypeCodePathAttributeASPathlimit == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     return $ PathAttributeASPathlimit bs

--                 | TypeCodePathAttributeLargeCommunity == code -> do
--                     ws <- getMany ( len `div` 12)
--                     return $ PathAttributeLargeCommunity ws

--                 | TypeCodePathAttributeAttrSet == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     return $ PathAttributeAttrSet bs

--                 | TypeCodePathAttributeUnknown == code -> do
--                     bs <- getByteString (fromIntegral len)
--                     fail ("unknown type code: " ++ show code')
--                     return $ PathAttributeUnknown bs

-- instance {-# OVERLAPPING #-} Binary [PathAttribute] where

--     put = putn
--     get = getn
-- encodePathAttributes :: [PathAttribute] -> B.ByteString
-- encodePathAttributes = L.toStrict . encode

-- decodePathAttributes :: B.ByteString -> [PathAttribute]
-- decodePathAttributes =  decode . L.fromStrict

-- instance {-# OVERLAPPING #-} Binary [Word64] where

--     put = putn
--     get = getn

-- instance {-# OVERLAPPING #-} Binary [LargeCommunity] where

--     put = putn
--     get = getn

-- getMany :: Binary a => Int -> Get [a]
-- getMany n = go [] n
--      where
--      go xs 0 = return $! reverse xs
--      go xs i = x <- get
--                   x `seq` go (x:xs) (i-1)
