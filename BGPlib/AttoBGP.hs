{-# LANGUAGE FlexibleInstances #-}
module BGPlib.AttoBGP where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as A
import Control.Monad(unless)

import qualified BGPlib.RFC4271
import BGPlib.Capabilities(parseOptionalParameters)
import BGPlib.LibCommon(decode8,fromHostAddress)
import BGPlib.Prefixes

import BGPlib.BGPparse

wireParser :: A.Parser [ B.ByteString ]
wireParser = A.many' wireParser1 A.<?>  "BGP wire format Parser"

wireParser1 :: A.Parser B.ByteString
wireParser1 = do
    A.string $ B.replicate 16 0xff
    length <- fromIntegral <$> A.anyWord16be
    typeCode <- A.peekWord8'
    if length < 19 then fail "short length"
    else if length > 4096 then fail $ "too long " ++ show length
    else if typeCode < 1 || typeCode > 4 then fail $ "invalid type code (" ++ show typeCode ++ ")"
    else A.take (length - 18)

bgpParser :: A.Parser [ BGPMessage ]
bgpParser = A.many' bgpParser1 A.<?>  "BGP intermediate format Parser"

bgpParser1 :: A.Parser BGPMessage
bgpParser1 = do
    A.string $ B.replicate 16 0xff
    length <- fromIntegral <$> A.anyWord16be
    if length < 19 then fail "short length"
    else if length > 4096 then fail $ "too long " ++ show length
    else do
        typeCode <-A.anyWord8
        case typeCode of
            1 -> do
               msgVer <- A.anyWord8
               unless (msgVer == 4) (fail "Bad version(Open)")
               myAutonomousSystem <- A.anyWord16be
               holdTime <- A.anyWord16be
               bgpID <- A.anyWord32be
               optionalParametersLength <- fromIntegral <$> A.anyWord8
               optionalParameters <- L.fromStrict <$> A.take optionalParametersLength
               -- TODO implement a native parser for optional parameters
               return $ BGPOpen myAutonomousSystem holdTime (fromHostAddress bgpID)  ( parseOptionalParameters optionalParameters )
            2 -> do
               -- todo implment the parsers for prefixes and attributes (prefixes may be already done in zmesg)
               withdrawnRoutesLength <- fromIntegral <$> A.anyWord16be
               withdrawnRoutes <- L.fromStrict <$> A.take withdrawnRoutesLength
               pathAttributesLength <- fromIntegral <$> A.anyWord16be
               pathAttributes <- L.fromStrict <$> A.take pathAttributesLength
               nlri <- L.fromStrict <$> A.take ( length - withdrawnRoutesLength - pathAttributesLength - 23 )
               return $ BGPUpdate withdrawnRoutes pathAttributes nlri
            3 -> do
               errorCode <- A.anyWord8
               errorSubcode <- A.anyWord8
               errorData <- L.fromStrict <$> A.take ( length - 21 )
               return $ BGPNotify (decode8 errorCode) errorSubcode errorData
            4 -> if length == 19 then return BGPKeepalive else fail "invalid length in KeepAlive"
            _ -> fail $ "invalid type code (" ++ show typeCode ++ ")"

{-
parsePrefixes 0 = []

parsePrefixes length = do
    plen <- anyWord8
    prefix' <-
        if | plen == 0  -> ( Prefix (0,0) ) : parsePrefixes (length-1)
           | plen < 9   -> readPrefix1Byte  : parsePrefixes (length-2)
           | plen < 17  -> readPrefix2Byte  : parsePrefixes (length-3)
           | plen < 25  -> readPrefix3Byte  : parsePrefixes (length-4)
           | plen < 33  -> readPrefix4Byte  : parsePrefixes (length-5)
           | otherwise  -> fail $ "plen > " ++ show plen
    let v4address = fromHostAddress $ byteSwap32 prefix'
    return ZPrefixV4{..}
    where
        readPrefix1Byte = do
            b0 <- anyWord8
            
            return (unsafeShiftL (fromIntegral b0) 24)
        readPrefix2Byte = do
            b0 <- anyWord16be
            return (unsafeShiftL (fromIntegral b0) 16)
        readPrefix3Byte = do
            b0 <- anyWord16be
            b1 <- anyWord8
            return (unsafeShiftL (fromIntegral b1) 8 .|. unsafeShiftL (fromIntegral b0) 16)
        readPrefix4Byte = anyWord32be

    get = label "Prefix" $ do
        subnet <- getWord8
        if subnet == 0
        then return $ Prefix (0,0)
        else if subnet < 9
        then do
            w8 <- getWord8
            let ip = unsafeShiftL (fromIntegral w8 :: Word32) 24
            return $ Prefix (subnet,ip)
        else if subnet < 17
        then do
            w16  <- getWord16be
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16
            return $ Prefix (subnet,ip)
        else if subnet < 25
        then do
            w16  <- getWord16be
            w8  <- getWord8
            let ip = unsafeShiftL (fromIntegral w16  :: Word32) 16 .|.
                     unsafeShiftL (fromIntegral w8 :: Word32) 8
            return $ Prefix (subnet,ip)
        else do ip <- getWord32be
                return $ Prefix (subnet,ip)

-}
