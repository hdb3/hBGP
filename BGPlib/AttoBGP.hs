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


import BGPlib.BGPparse

wireParser :: A.Parser [ B.ByteString ]
wireParser = A.many' wireParser1 A.<?>  "BGP wire format Parser"

wireParser1 :: A.Parser B.ByteString
wireParser1 = do
    A.string $ B.replicate 16 0xff
    length <- fromIntegral <$> A.anyWord16be
    typeCode <- A.peekWord8'
    if length < 19 then error "short length"
    else if length > 4096 then error $ "too long " ++ show length
    else if typeCode < 1 || typeCode > 4 then error $ "invalid type code (" ++ show typeCode ++ ")"
    else A.take (length - 18)

bgpParser :: A.Parser [ BGPMessage ]
bgpParser = A.many' bgpParser1 A.<?>  "BGP intermediate format Parser"

bgpParser1 :: A.Parser BGPMessage
bgpParser1 = do
    A.string $ B.replicate 16 0xff
    length <- fromIntegral <$> A.anyWord16be
    if length < 19 then error "short length"
    else if length > 4096 then error $ "too long " ++ show length
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
               nlri <- L.fromStrict <$> A.take ( length - withdrawnRoutesLength - withdrawnRoutesLength - 23 )
               return $ BGPUpdate withdrawnRoutes pathAttributes nlri
            3 -> do
               errorCode <- A.anyWord8
               errorSubcode <- A.anyWord8
               errorData <- L.fromStrict <$> A.take ( length - 21 )
               return $ BGPNotify (decode8 errorCode) errorSubcode errorData
            4 -> if length == 19 then return BGPKeepalive else error "invalid length in KeepAlive"
            _ -> error $ "invalid type code (" ++ show typeCode ++ ")"
