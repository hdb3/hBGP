{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
module BGPlib.ASPath where
import Data.Binary
import Data.Binary.Get
import Data.List(foldl',intercalate)
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Attoparsec.ByteString -- from package attoparsec
import Data.Attoparsec.Binary -- from package attoparsec-binary

import Data.Hashable

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

instance {-# OVERLAPPING #-}(ASNumber asn) =>  Binary [ASSegment asn] where
    put = putn
    get = getn

class (Eq a, Num a, Integral a, Show a, Read a, Binary a) => ASNumber a where
    putASSegmentElement :: ASSegmentElementTypeCode -> [a] -> Put
    putASSegmentElement code asns = do putWord8 (encode8 code)
                                       putWord8 (fromIntegral $ length asns)
                                       putn asns

instance ASNumber Word16 where
instance ASNumber Word32 where
data ASPath = ASPath2 [ASSegment Word16] | ASPath4 [ASSegment Word32] deriving (Eq,Generic)
instance Show ASPath where
    show (ASPath2 x) = if null x then "_" else concatMap show x
    show (ASPath4 x) = if null x then "_" else concatMap show x

instance Hashable ASPath
data ASSegment asn = ASSet [asn] | ASSequence [asn] deriving (Eq,Generic)

instance Show (ASSegment Word32) where
    show (ASSequence asn) = show asn -- the list braces are ‘free’.
    show (ASSet asn) = "{" ++ intercalate "," (map show asn) ++ "}"

instance Show (ASSegment Word16) where
    show _ = undefined

instance Hashable ( ASSegment Word16 )
instance Hashable ( ASSegment Word32 )

isASSet (ASSet _) = True
isASSet _ = False

isASSequence = not . isASSet

isSingletonASSet :: ASSegment t -> Bool
isSingletonASSet (ASSet [_]) = True
isSingletonASSet _ = False

instance Functor ASSegment where
    fmap f (ASSet asnx) = ASSet (map f asnx)
    fmap f (ASSequence asnx) = ASSequence (map f asnx)

-- i really wanted fmap bu don't know how to make the list a functor....
-- also this looks like a fold, but.....
gmap :: (a -> b) -> [ASSegment a] -> [ASSegment b]
gmap _ [] = []
gmap f (ASSet a : ax) = ASSet (fmap f a) : gmap f ax
gmap f (ASSequence a : ax) = ASSequence (fmap f a) : gmap f ax

-- note that the inverse to 'toASPath4' would potentially truncate 32 bit ASNs so unless there is a valid
-- reason for allowing this, i won't write it...
toASPath4 :: ASPath -> ASPath
toASPath4 (ASPath2 aspath) = ASPath4 (gmap fromIntegral aspath)
toASPath4 (ASPath4 aspath) = ASPath4 aspath

-- NOTE** the below 'raw' asPrePend function masks the complexity of mixing ASN2 and ASN4 operations
-- a complete version would have to operate on AS4 paths and ASPaths and use 'AS_TRANS'
-- however as long as the asnumber is below 2^16 there should be no problem...
-- note the guard function is null since the actual type of the ASPath will protect it.....
asPrePend :: ASNumber a => a -> ASPath -> ASPath
asPrePend asn (ASPath2 asp) | asn < 2^16 = ASPath2 (asPrePend' (fromIntegral asn) asp)
asPrePend asn (ASPath4 asp) = ASPath4 (asPrePend' (fromIntegral asn) asp)

asPrePend' asn [] = [ASSequence [asn]]
asPrePend' asn (ASSet sets : segs) = ASSequence [asn] : ASSet sets : segs
asPrePend' asn (ASSequence seqs : segs) | length seqs < 255 = ASSequence (asn:seqs) : segs
                                        | otherwise         = ASSequence [asn] : ASSequence seqs : segs

asPathLength :: ASPath -> Int
asPathLength (ASPath2 asp) = asPathLength' asp
asPathLength (ASPath4 asp) = asPathLength' asp
asPathLength' = foldl' addSegLength 0 where
    addSegLength acc (ASSet _ ) = acc + 1
    addSegLength acc (ASSequence ax ) = acc + length ax

as2list :: Integral a => [a] -> [Word16]
as2list = map fromIntegral


as4list :: Integral a => [a] -> [Word32]
as4list = map fromIntegral

instance Binary ASPath where
    get = decodeAS4 <$> getRemainingLazyByteString

    put (ASPath2 asp) = put asp
    put (ASPath4 asp) = put asp

decodeAsASPath2 :: L.ByteString -> ASPath
decodeAsASPath2 bytes = ASPath2 (decode bytes)

decodeAsASPath4 :: L.ByteString -> ASPath
decodeAsASPath4 bytes = ASPath4 (decode bytes)

instance (ASNumber asn) => Binary (ASSegment asn) where

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

type ASSegment2 = ASSegment Word16
type ASSegment4 = ASSegment Word32

path :: Parser ASPath
path = path4 <|> path2
path2 :: Parser ASPath
path2 = do
    segs <- many' asSetOrSeq2
    endOfInput
    return $ ASPath2 segs

asSetOrSeq2 :: Parser ASSegment2
asSetOrSeq2 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    rvals <- count (fromIntegral asCount) anyWord16be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence

path4 :: Parser ASPath
path4 = do
    segs <- many' asSetOrSeq4
    endOfInput
    return $ ASPath4 segs

asSetOrSeq4 :: Parser ASSegment4
asSetOrSeq4 = do
    segType <- satisfy isSetOrSeq
    asCount <- anyWord8
    rvals <- count (fromIntegral asCount) anyWord32be
    return $ if segType == enumASSet then ASSet rvals else ASSequence rvals where
        isSetOrSeq b = b == enumASSet || b == enumASSequence
