{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BGPlib.Capabilities where

import BGPlib.LibCommon
import qualified Data.Attoparsec.Binary as A
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import Data.Binary
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L

type CapCode = Word8

--
-- ref https://www.iana.org/assignments/capability-codes/capability-codes.xml
--
_CapCodeMultiprotocol = 1 :: CapCode

_CapCodeRouteRefresh = 2 :: CapCode

_CapCodeExtendedLength = 6 :: CapCode

_CapCodeGracefulRestart = 64 :: CapCode

_CapCodeAS4 = 65 :: CapCode

_CapCodeAddPath = 69 :: CapCode

_CapCodeEnhancedRouteRefresh = 70 :: CapCode

_CapCodeLLGR = 71 :: CapCode

_CapCodeCiscoRefresh = 128 :: CapCode

{-
RFC5492:
   The parameter contains one or more triples <Capability Code,
   Capability Length, Capability Value>, where each triple is encoded as
   shown below:

       +------------------------------+
       | Capability Code (1 octet)    |
       +------------------------------+
       | Capability Length (1 octet)  |
       +------------------------------+
       | Capability Value (variable)  |
       +------------------------------+

   The use and meaning of these fields are as follows:

      Capability Code:

         Capability Code is a one octet field that unambiguously
         identifies individual capabilities.

      Capability Length:

         Capability Length is a one octet field that contains the length
         of the Capability Value field in octets.

      Capability Value:

         Capability Value is a variable length field that is interpreted
         according to the value of the Capability Code field.

An additional complexity for parsing is that an Open message may contain more than one Capability Optional Parameter.
So an outer parser for Optional Parameters must collect Capabilities and assemple them into a single list.
In the first instance this parser need only return Capabilities, even though in principle other parameters are possible.
This should reflect the structure used for the Open variant of the (parsed) BGPMessage

-}

-- graceful restart
-- see RFC 4724
--
--this is a complex capability in theory however the simple instance is very simple
-- we only implement the basic verion
--
--AS4 - 32bit ASNs
--see RFC6793
--the capability is just the local 32bit ASN
--
data Capability
  = CapMultiprotocol Word16 Word8
  | CapGracefulRestart Bool Word16
  | CapAS4 Word32
  | CapAddPath Word16 Word8 Word8 -- in future the encoding of Send / Receive / Both could be made symbolic / typed
  | CapRouteRefresh
  | CapLLGR
  | CapCiscoRefresh
  | CapEnhancedRouteRefresh
  | CapExtendedLength
  | CapUnknown Word8 B.ByteString
  deriving (Show, Eq, Read, Generic, NFData)

eq_ :: Capability -> Capability -> Bool
eq_ (CapMultiprotocol _ _) (CapMultiprotocol _ _) = True
eq_ (CapGracefulRestart _ _) (CapGracefulRestart _ _) = True
eq_ (CapAS4 _) (CapAS4 _) = True
eq_ CapAddPath {} CapAddPath {} = True
eq_ CapRouteRefresh CapRouteRefresh = True
eq_ CapLLGR CapLLGR = True
eq_ CapCiscoRefresh CapCiscoRefresh = True
eq_ CapEnhancedRouteRefresh CapEnhancedRouteRefresh = True
eq_ CapExtendedLength CapExtendedLength = True
-- need to consider if/when comparing CapUnknown returns True
eq_ (CapUnknown _ _) (CapUnknown _ _) = True
eq_ _ _ = False

capCode :: Capability -> CapCode
capCode (CapMultiprotocol _ _) = _CapCodeMultiprotocol
capCode (CapGracefulRestart _ _) = _CapCodeGracefulRestart
capCode (CapAS4 _) = _CapCodeAS4
capCode CapAddPath {} = _CapCodeAddPath
capCode CapRouteRefresh = _CapCodeRouteRefresh
capCode CapLLGR = _CapCodeLLGR
capCode CapCiscoRefresh = _CapCodeCiscoRefresh
capCode CapEnhancedRouteRefresh = _CapCodeEnhancedRouteRefresh
capCode CapExtendedLength = _CapCodeExtendedLength

capCodes = map capCode

putCap :: Capability -> Put
putCap = put

getCap :: Get Capability
getCap = error "Binary get deprecated"
  
capsEncode :: [Capability] -> L.ByteString
capsEncode = encode

instance {-# OVERLAPPING #-} Binary [Capability] where
  put = putn
  get = error "Binary get deprecated"
  
instance Binary Capability where
  put CapRouteRefresh = do
    putWord8 _CapCodeRouteRefresh
    putWord8 0
  put CapCiscoRefresh = do
    putWord8 _CapCodeCiscoRefresh
    putWord8 0
  put CapEnhancedRouteRefresh = do
    putWord8 _CapCodeEnhancedRouteRefresh
    putWord8 0
  put (CapAS4 as4) = do
    putWord8 _CapCodeAS4
    putWord8 4
    putWord32be as4
  put (CapAddPath afi safi bits) = do
    putWord8 _CapCodeAddPath
    putWord8 4
    putWord16be afi
    putWord8 safi
    putWord8 bits
  put (CapGracefulRestart rFlag restartTime) = do
    putWord8 _CapCodeGracefulRestart
    putWord8 2
    putWord16be $ if rFlag then setBit restartTime 15 else restartTime
  put (CapMultiprotocol afi safi) = do
    putWord8 _CapCodeMultiprotocol
    putWord8 4
    putWord16be afi
    putWord8 0
    putWord8 safi
  put CapExtendedLength = do
    putWord8 _CapCodeExtendedLength
    putWord8 0
  put (CapUnknown t bs) = do
    putWord8 t
    putWord8 $ fromIntegral (B.length bs)
    putByteString bs

  get = error "Binary get deprecated"

buildOptionalParameters :: [Capability] -> ByteString
buildOptionalParameters capabilities
  | not $ null capabilities =
    let caps = L.concat $ map encode capabilities
     in L.toStrict $ toLazyByteString $ word8 2 <> word8 (fromIntegral $ L.length caps) <> lazyByteString caps
  | otherwise = B.empty

-- need to parse multiple parameter blocks to cater for case whwere each capability is sent in a  singleton parameter
--
parseOptionalParameters' :: L.ByteString -> [Capability]
parseOptionalParameters' bs = concatMap (decode . value) capabilityParameters
  where
    parameters = decode bs :: [TLV]
    capabilityParameters = filter ((2 ==) . typeCode) parameters

data TLV = TLV {typeCode :: Word8, value :: L.ByteString}

instance Binary TLV where
  put TLV {..} = putWord8 typeCode <> putWord8 (fromIntegral (L.length value)) <> putLazyByteString value

  get = error "Binary get deprecated"
  
instance {-# OVERLAPPING #-} Binary [TLV] where
  put = putn
  get = error "Binary get deprecated"
  
-- ###############################
{-
  this is another instance of a parser which should consume a defined length fragment of a larger string,
  with a possibility that the content is mal-structured.
  The most general solution takes a specific parser which takes the available buffer length and returns its remaining buffer length.
  However, where a TLV structure is given then we can separate the specifics of encoding from the surraoung scaffold.
  An elegant solution would allow the differing requirements (fixed or variable length to be encoded in the specilaisations,
  so that exceptions can be raised in the framework rather than the implmentation.
-}

parseOptionalParameters :: Word8 -> Parser [Capability]
parseOptionalParameters n
  | n == 0 = return []
  | n == 1 = error "parseOptionalParameters: invalid length: 1"
  | otherwise = do
    t <- A.anyWord8
    l <- A.anyWord8
    if n < l + 2
      then error $ "parseOptionalParameters: invalid length n=" ++ show n ++ " l=" ++ show l ++ " t=" ++ show t
      else do
        p <-
          if 2 == t
            then parseCaps l
            else do
              _ <- A.take (fromIntegral l)
              return []
        px <- parseOptionalParameters (n -2 - l)
        return (p ++ px)
  where
    parseCaps :: Word8 -> Parser [Capability]
    parseCaps 0 = return []
    parseCaps n
      | n == 1 = error "parseCaps: invalid length: 1"
      | otherwise = do
        t <- A.anyWord8
        l <- A.anyWord8
        if n < l + 2
          then error $ "parseCaps: invalid length n=" ++ show n ++ " l=" ++ show l ++ " t=" ++ show t
          else do
            cap <- parseCapability t l
            caps <- parseCaps (n -2 - l)
            return $ cap : caps
    parseCapability :: Word8 -> Word8 -> Parser Capability
    parseCapability t l =
      if  | t == _CapCodeMultiprotocol -> do
            afi <- A.anyWord16be
            _ <- A.anyWord8
            CapMultiprotocol afi <$> A.anyWord8
          | t == _CapCodeGracefulRestart -> do
            word0 <- A.anyWord16be
            let rFlag = testBit word0 15
                restartTime = word0 .&. 0x0fff
            return (CapGracefulRestart rFlag restartTime)
          | t == _CapCodeAS4 -> CapAS4 <$> A.anyWord32be
          | t == _CapCodeAddPath -> do
            afi <- A.anyWord16be
            safi <- A.anyWord8
            CapAddPath afi safi <$> A.anyWord8
          | t == _CapCodeRouteRefresh -> return CapRouteRefresh
          | t == _CapCodeEnhancedRouteRefresh -> return CapEnhancedRouteRefresh
          | t == _CapCodeCiscoRefresh -> return CapCiscoRefresh
          | t == _CapCodeLLGR -> if l == 0 then return CapLLGR else error "LLGR with non null payload not handled"
          | t == _CapCodeExtendedLength -> return CapExtendedLength
          | otherwise -> CapUnknown t <$> A.take (fromIntegral l)
