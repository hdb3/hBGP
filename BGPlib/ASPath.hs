{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module BGPlib.ASPath where

-- this interface masks AS2/AS4 encoding - all AS numbers are simply Word32

import BGPlib.Codes
import BGPlib.LibCommon
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Builder
import Data.Hashable
import Data.List (foldl')

-- binary format for AS path is a sequence of AS path segments
-- AS path segments are TLVs, however the 'length' is not a byte count
-- it is the number of included AS numbers
-- the type is 1 or two which codes either a Set or Sequence
-- note: 4 byte AS numbers may be used inthe AS PATH as well as in the AS4_PATH
-- therefore decoding AS_PATH requires to know whether 2 or 4 byte AS numbers are in use.

buildASPath :: ASPath -> (Word16, Builder)
buildASPath = error "undefined"

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

asPrePend :: ASNumber -> ASPath -> ASPath
asPrePend asn [] = [ASSequence [asn]]
asPrePend asn (ASSet sets : segs) = ASSequence [asn] : ASSet sets : segs
asPrePend asn (ASSequence seqs : segs)
  | length seqs < 255 = ASSequence (asn : seqs) : segs
  | otherwise = ASSequence [asn] : ASSequence seqs : segs

asPathLength :: ASPath -> Int
asPathLength = foldl' addSegLength 0
  where
    addSegLength acc (ASSet _) = acc + 1
    addSegLength acc (ASSequence ax) = acc + length ax

putASSegmentElement :: ASSegmentElementTypeCode -> [Word32] -> Put
putASSegmentElement code asns = do
  putWord8 (encode8 code)
  putWord8 (fromIntegral $ length asns)
  putn asns

instance Binary ASSegment where
  put (ASSet asns) = putASSegmentElement EnumASSet asns
  put (ASSequence asns) = putASSegmentElement EnumASSequence asns

  get = label "ASSegment" $ do
    code' <- getWord8
    let code = decode8 code'
    len <- getWord8
    asns <- getNasns len
    if  | code == EnumASSet -> return $ ASSet asns
        | code == EnumASSequence -> return $ ASSequence asns
        | otherwise -> fail "invalid code in ASpath"
    where
      getNasns :: (Binary asn) => Word8 -> Get [asn]
      getNasns n
        | n == 0 = return []
        | otherwise = do
          asn <- get
          asns <- getNasns (n -1)
          return (asn : asns)
