{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Capabilities where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.ByteString(ByteString)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Builder
import Data.Monoid((<>))

import LibCommon

--
-- ref https://www.iana.org/assignments/capability-codes/capability-codes.xml
--
_CapCodeMultiprotocol = 1
_CapCodeRouteRefresh = 2
_CapCodeGracefulRestart = 64
_CapCodeAS4 = 65
_CapCodeLLGR = 71
_CapCodeCiscoRefresh = 128
{-
 The Capability Value field is defined as:

                     0       7      15      23      31
                     +-------+-------+-------+-------+
                     |      AFI      | Res.  | SAFI  |
                     +-------+-------+-------+-------+

   The use and meaning of this field is as follow:

      AFI  - Address Family Identifier (16 bit), encoded the same way as
          in the Multiprotocol Extensions

      Res. - Reserved (8 bit) field.  SHOULD be set to 0 by the sender
          and ignored by the receiver.

          Note that not setting the field value to 0 may create issues
          for a receiver not ignoring the field.  In addition, this
          definition is problematic if it is ever attempted to redefine
          the field.

      SAFI - Subsequent Address Family Identifier (8 bit), encoded the
          same way as in the Multiprotocol Extensions.

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
data Capability = CapMultiprotocol Word16 Word8 
                | CapGracefulRestart Bool Word16
                | CapAS4 Word32
                | CapRouteRefresh
                | CapLLGR
                | CapCiscoRefresh
                  deriving (Show,Eq,Read)

eq_ :: Capability -> Capability -> Bool
eq_ (CapMultiprotocol _ _) (CapMultiprotocol _ _) = True
eq_ (CapGracefulRestart _ _) (CapGracefulRestart _ _) = True
eq_ (CapAS4 _) (CapAS4 _) = True
eq_ CapRouteRefresh CapRouteRefresh = True
eq_ CapLLGR CapLLGR = True
eq_ CapCiscoRefresh CapCiscoRefresh = True
eq_ _ _ = False

putCap :: Capability -> Put
putCap = put
getCap :: Get Capability
getCap = get

capsEncode :: [Capability] -> L.ByteString
capsEncode = encode

instance {-# OVERLAPPING #-} Binary [Capability] where

    put = putn
    get = getn

instance Binary Capability where

    put CapRouteRefresh = do
        putWord8 _CapCodeRouteRefresh
        putWord8 0

    put CapCiscoRefresh = do
        putWord8 _CapCodeCiscoRefresh
        putWord8 0

    put (CapAS4 as4) = do
        putWord8 _CapCodeAS4
        putWord8 4
        putWord32be as4

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

    get = label "Capability" $ do
        t <- getWord8
        l <- getWord8
        -- this is not very fail safe, e.g. length should allow passing over unknown codes
        -- and also validating known ones for their length (which could be variable in more than one case!!!!
        if | t == _CapCodeMultiprotocol -> do
                      afi <- getWord16be
                      _ <- getWord8
                      safi <- getWord8
                      return (CapMultiprotocol afi safi)
           | t == _CapCodeGracefulRestart -> do
                      word0 <- getWord16be
                      let rFlag = testBit word0 15
                          restartTime = word0 .&. 0x0fff
                      return (CapGracefulRestart rFlag restartTime)
           | t == _CapCodeAS4 -> do as <- getWord32be
                                    return $ CapAS4 as -- surely not the most elegant way to say this!!!!
           | t == _CapCodeRouteRefresh -> return CapRouteRefresh
           | t == _CapCodeCiscoRefresh -> return CapCiscoRefresh
           | t == _CapCodeLLGR -> if (l == 0) then return CapLLGR else error "LLGR with non null payload not handled"
           | otherwise        -> do error $ "Unexpected type code: " ++ show t
                                    return undefined

buildOptionalParameters :: [ Capability ] -> ByteString
buildOptionalParameters capabilities | not $ null capabilities = let caps = L.concat $ map encode capabilities in
                                                                 L.toStrict $ toLazyByteString $ word8 2 <>  word8 (fromIntegral $ L.length caps) <> lazyByteString caps
                                     | otherwise = B.empty

-- need to parse multiple parameter blocks to cater for case whwere each capability is sent in a  singleton parameter
--
parseOptionalParameters :: L.ByteString -> [ Capability ]

parseOptionalParameters bs = concatMap (decode . value) capabilityParameters where
                                 parameters = decode bs :: [TLV] 
                                 capabilityParameters = filter ((2 ==) . typeCode) parameters

data TLV = TLV { typeCode :: Word8 , value :: L.ByteString }
instance Binary TLV where
    put TLV{..} = putWord8 typeCode <> putWord8 (fromIntegral (L.length value)) <> putLazyByteString value

    get = label "TLV" $ do
             typeCode <- get
             n <- get :: Get Word8
             bs <- getLazyByteString (fromIntegral n)
             return $ TLV typeCode bs

instance {-# OVERLAPPING #-} Binary [TLV] where
    put = putn
    get = getn
