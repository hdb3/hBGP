{-# LANGUAGE MultiWayIf #-}

module BGPlib.Capabilities where

import qualified Data.Attoparsec.Binary as A
import qualified Data.Attoparsec.ByteString as A
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder


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

data Capability
  = CapMultiprotocol Word16 Word8
  | CapGracefulRestart Bool Word16
  | CapAS4 Word32
  | CapAddPath Word16 Word8 Word8
  | CapRouteRefresh
  | CapLLGR
  | CapCiscoRefresh
  | CapEnhancedRouteRefresh
  | CapExtendedLength
  | CapUnknown Word8 B.ByteString
  deriving (Show, Eq, Read)

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
  
capsEncode :: [Capability] -> L.ByteString
capsEncode = toLazyByteString . snd . parameterBuilder

buildOptionalParameters :: [Capability] -> B.ByteString
buildOptionalParameters = L.toStrict . capsEncode 
{-
  Builder: the envelope for a capability list is an 'optional parameter TLV', holding a 'capability TLV'.
  Both TLVs are limited to 255 byte payloads, but the scope to enode multiple parameters upto the BGP message size limit is available for very long sequences.
  Absent this need, a single nested structure is sufficient.
  The inner TLV is built by folding over each capability.
  A builder for each capability shoudl return the encoded length to allow the TLV to be correctly built. 
-}


parameterBuilder ::[Capability] -> ( Word8, Builder)
parameterBuilder caps = let (length,b) = capabilitiesBuilder caps in (length+2, word8 2 <> word8 length <> b)

capabilitiesBuilder ::[Capability] -> ( Word8, Builder)
capabilitiesBuilder = foldr acc (0,mempty) where
  acc cap (w,b) = let (w',b') = capabilityBuilder cap in (w+w'+2, b' <> b)

capabilityBuilder ::Capability -> ( Word8, Builder)

capabilityBuilder CapRouteRefresh = (0, word8 _CapCodeRouteRefresh)
capabilityBuilder CapCiscoRefresh = ( 0 , word8 _CapCodeCiscoRefresh <> word8 0)
capabilityBuilder CapEnhancedRouteRefresh = ( 0 , word8 _CapCodeEnhancedRouteRefresh <> word8 0)
capabilityBuilder (CapAS4 as4) = ( 4 , word8 _CapCodeAS4 <> word8 4 <> word32BE as4)
capabilityBuilder (CapAddPath afi safi bits) = ( 4 , word8 _CapCodeAddPath <> word8 4 <> word16BE afi <> word8 safi <> word8 bits)
capabilityBuilder (CapGracefulRestart rFlag restartTime) = ( 2 , word8 _CapCodeGracefulRestart <> word8 2 <> word16BE (if rFlag then setBit restartTime 15 else restartTime))
capabilityBuilder (CapMultiprotocol afi safi) = ( 4 , word8 _CapCodeMultiprotocol <> word8 4 <> word16BE afi <> word8 0 <> word8 safi)
capabilityBuilder CapExtendedLength = ( 0 , word8 _CapCodeExtendedLength <> word8 0)
capabilityBuilder (CapUnknown t bs) = ( fromIntegral (B.length bs), word8 t <> (word8 $ fromIntegral (B.length bs)) <> byteString bs)


parseOptionalParameters :: Word8 -> A.Parser [Capability]
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
    parseCaps :: Word8 -> A.Parser [Capability]
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
    parseCapability :: Word8 -> Word8 -> A.Parser Capability
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
