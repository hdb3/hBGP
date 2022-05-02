{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module BGPlib.ASPath where

-- this interface masks AS2/AS4 encoding - all AS numbers are simply Word32

import BGPlib.Codes
import BGPlib.LibCommon
import ByteString.StrictBuilder
import Control.Monad (when)
import qualified Data.Attoparsec.Binary as A
import qualified Data.Attoparsec.ByteString as A
import Data.Hashable
import Data.List (foldl')
import Data.Word

type ASNumber = Word32

type ASPath = [ASSegment]

data ASSegment = ASSet [ASNumber] | ASSequence [ASNumber] deriving (Show, Eq, Generic)

instance Hashable ASSegment

isASSet (ASSet _) = True
isASSet _ = False

isASSequence = not . isASSet

isSingletonASSet :: ASSegment -> Bool
isSingletonASSet (ASSet [_]) = True
isSingletonASSet _ = False

{-# INLINE asPrePend #-}
asPrePend :: ASNumber -> ASPath -> ASPath
asPrePend asn [] = [ASSequence [asn]]
asPrePend asn (ASSet sets : segs) = ASSequence [asn] : ASSet sets : segs
asPrePend asn (ASSequence seqs : segs)
  | length seqs < 255 = ASSequence (asn : seqs) : segs
  | otherwise = ASSequence [asn] : ASSequence seqs : segs

{-# INLINE asPathLength #-}
asPathLength :: ASPath -> Int
asPathLength = foldl' addSegLength 0
  where
    addSegLength acc (ASSet _) = acc + 1
    addSegLength acc (ASSequence ax) = acc + length ax

{-# INLINE parseASPath #-}
parseASPath :: Word16 -> A.Parser ASPath
parseASPath 0 = return []
parseASPath n
  | n < 2 = error $ "parseASPath: invalid length: " ++ show n
  | otherwise = do
      t <- A.anyWord8
      l <- A.anyWord8
      let calculatedByteCount = fromIntegral $ 2 + 4 * l
      when (n < calculatedByteCount) (error $ "parseASPath: invalid length n=" ++ show n ++ " l=" ++ show l ++ " t=" ++ show t)
      segment <-
        if
            | t == enumASSequence -> ASSequence <$> A.count (fromIntegral l) A.anyWord32be
            | t == enumASSet -> ASSet <$> A.count (fromIntegral l) A.anyWord32be
            | otherwise -> error $ "parseASPath: invalid type = " ++ show t
      segments <- parseASPath (n - calculatedByteCount)
      return $ segment : segments

-- binary format for AS path is a sequence of AS path segments
-- AS path segments are TLVs, however the 'length' is not a byte count
-- it is the number of included AS numbers
-- the type is 1 or two which codes either a Set or Sequence
-- note: 4 byte AS numbers may be used inthe AS PATH as well as in the AS4_PATH
-- therefore decoding AS_PATH requires to know whether 2 or 4 byte AS numbers are in use.

-- this function only encodes the path and returns the bytelength of the generated Builder
-- this length is easily calculated as sums over the number of segments and number of ASNs:
-- each segment envelope contributes 2 bytes - each ASN contributes 4 bytes
{-# INLINE buildASPath #-}
buildASPath :: ASPath -> Builder
buildASPath = foldMap segBuilder
  where
    segBuilder (ASSequence asnx) = word8 2 <> word8 (fromIntegral $ length asnx) <> foldMap word32BE asnx
    segBuilder (ASSet asnx) = word8 1 <> word8 (fromIntegral $ length asnx) <> foldMap word32BE asnx
