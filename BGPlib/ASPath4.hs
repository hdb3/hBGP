{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
module BGPlib.ASPath4 where

-- this interface masks AS2/AS4 encoding - all AS numbers are simply Word32

import Data.Binary
import Data.Binary.Get
import Data.List(foldl')
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Attoparsec.ByteString -- from package attoparsec
import Data.Attoparsec.Binary -- from package attoparsec-binary

import Data.Hashable
import GHC.Generics(Generic)

import BGPlib.Codes
import BGPlib.LibCommon

-- data ASPath = ASPath deriving (Show,Eq)
-- data Aggregator = Aggregator deriving (Show,Eq)
-- data Communities = Communities deriving (Show,Eq)
-- data ExtendedCommunities = ExtendedCommunities deriving (Show,Eq)

-- binary format for AS path is a sequence of AS path segments
-- AS path segments are TLVs, however the 'length' is not a byte count
-- it is the number of included AS numbers
-- the type is 1 or two which codes either a Set or Sequence
-- note: 4 byte AS numbers may be used inthe AS PATH as well as in the AS4_PATH
-- therefore decoding AS_PATH requires to know whether 2 or 4 byte AS numbers are in use.

type ASNumber = Word32
type ASPath = [ASSegment]

data ASSegment = ASSet [ASNumber] | ASSequence [ASNumber] deriving (Show,Eq,Generic)
--instance Hashable ASPath
instance Hashable ASSegment

isASSet (ASSet _) = True
isASSet _ = False

isASSequence = not . isASSet

isSingletonASSet :: ASSegment -> Bool
isSingletonASSet (ASSet [_]) = True
isSingletonASSet _ = False

--instance Functor ASSegment where
    --fmap f (ASSet asnx) = ASSet (map f asnx)
    --fmap f (ASSequence asnx) = ASSequence (map f asnx)

-- i really wanted fmap bu don't know how to make the list a functor....
-- also this looks like a fold, but.....
--gmap :: (a -> b) -> [ASSegment a] -> [ASSegment b]
--gmap _ [] = []
--gmap f (ASSet a : ax) = ASSet (fmap f a) : gmap f ax
--gmap f (ASSequence a : ax) = ASSequence (fmap f a) : gmap f ax

-- NOTE** the below 'raw' asPrePend function masks the complexity of mixing ASN2 and ASN4 operations
-- a complete version would have to operate on AS4 paths and ASPaths and use 'AS_TRANS'
-- however as long as the asnumber is below 2^16 there should be no problem...
-- note the guard function is null since the actual type of the ASPath will protect it.....
asPrePend :: ASNumber -> ASPath -> ASPath
--asPrePend asn (ASPath2 asp) | asn < 2^16 = ASPath2 (asPrePend' (fromIntegral asn) asp)
--asPrePend asn (ASPath4 asp) = ASPath4 (asPrePend' (fromIntegral asn) asp)
asPrePend = asPrePend'

asPrePend' asn [] = [ASSequence [asn]]
asPrePend' asn (ASSet sets : segs) = ASSequence [asn] : ASSet sets : segs
asPrePend' asn (ASSequence seqs : segs) | length seqs < 255 = ASSequence (asn:seqs) : segs
                                        | otherwise         = ASSequence [asn] : ASSequence seqs : segs

asPathLength :: ASPath -> Int
--asPathLength (ASPath2 asp) = asPathLength' asp
--asPathLength (ASPath4 asp) = asPathLength' asp
asPathLength = foldl' addSegLength 0 where
    addSegLength acc (ASSet _ ) = acc + 1
    addSegLength acc (ASSequence ax ) = acc + length ax

as2list :: Integral a => [a] -> [Word16]
as2list = map fromIntegral


as4list :: Integral a => [a] -> [Word32]
as4list = map fromIntegral

--instance Binary ASPath where
    --get = do
        --bytes <- getRemainingLazyByteString
        --return $ decodeAS4 bytes

    --put (ASPath2 asp) = put asp
    --put (ASPath4 asp) = put asp

decodeAsASPath2 :: L.ByteString -> ASPath
decodeAsASPath2 = decode

decodeAsASPath4 :: L.ByteString -> ASPath
decodeAsASPath4 = decode

putASSegmentElement :: ASSegmentElementTypeCode -> [Word32] -> Put
putASSegmentElement code asns = do putWord8 (encode8 code)
                                   putWord8 (fromIntegral $ length asns)
                                   putn asns

instance Binary ASSegment where

    put (ASSet asns) = putASSegmentElement EnumASSet asns
    put (ASSequence asns) = putASSegmentElement EnumASSequence asns

    get = label "ASSegment" $ do
             code'  <- getWord8
             let code = decode8 code'
             len <- getWord8
             asns <- getNasns len
             if | code == EnumASSet -> return $ ASSet asns
                | code == EnumASSequence -> return $ ASSequence asns
                | otherwise -> fail "invalid code in ASpath"
             where
             getNasns :: (Binary asn) => Word8 -> Get [asn]
             getNasns n | n == 0 = return []
                        | otherwise = do asn <- get
                                         asns <- getNasns (n-1)
                                         return (asn:asns)


decodeAS4 = fromRight' . parseOnly path . L.toStrict
    where
    fromRight' (Right b ) = b

path :: Parser ASPath
path = path4 <|> path2
path2 :: Parser ASPath
path2 = do
    segs <- many' asSetOrSeq2
    endOfInput
    return segs

asSetOrSeq2 :: Parser ASSegment
asSetOrSeq2 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    --rvals <- (fmap (map fromIntegral)) $ count (fromIntegral asCount) anyWord16be
    rvals <- map fromIntegral <$> count (fromIntegral asCount) anyWord16be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence

path4 :: Parser ASPath
path4 = do
    segs <- many' asSetOrSeq4
    endOfInput
    return segs

asSetOrSeq4 :: Parser ASSegment
asSetOrSeq4 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    rvals <- count (fromIntegral asCount) anyWord32be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence
