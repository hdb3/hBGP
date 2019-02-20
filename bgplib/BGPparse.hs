{-# LANGUAGE MultiWayIf #-}
module BGPparse where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.IP
import Control.Monad(unless)

import RFC4271
import Capabilities
import LibCommon
import GetBGPMsg(BGPByteString(..),RcvStatus(..))

_BGPOpen = 1 :: Word8
_BGPUpdate = 2 :: Word8
_BGPNotify = 3 :: Word8
_BGPKeepalive = 4 :: Word8
_BGPVersion = 4 :: Word8

decodeBGPByteString (BGPByteString (Left Timeout)) = BGPTimeout
decodeBGPByteString (BGPByteString (Left EndOfStream)) = BGPEndOfStream
decodeBGPByteString (BGPByteString (Left (Error s))) = BGPError s
decodeBGPByteString (BGPByteString (Right lbs)) = decode lbs :: BGPMessage

data BGPMessage = BGPOpen { myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: IPv4, caps :: [ Capability ] }
                  | BGPKeepalive
                  | BGPNotify { code :: EnumNotificationCode, subCode :: NotificationSubcode, errorData :: L.ByteString }
                  -- | BGPNotify { code :: EnumNotificationCode, subCode :: NotificationSubcode, caps :: [ Capability ] }
                  | BGPUpdate { withdrawn :: L.ByteString, attributes :: L.ByteString, nlri :: L.ByteString }
                  | BGPTimeout
                  | BGPError String
                  | BGPEndOfStream
                    deriving (Show,Eq)

toAS2 :: Word32 -> Word16
toAS2 as | as < 0x10000 = fromIntegral as
         | otherwise = 23456

display :: BGPMessage -> String
display BGPOpen{} = "BGPOpen"
display BGPKeepalive{} = "BGPKeepalive"
display BGPNotify{} = "BGPNotify"
display BGPUpdate{} = "BGPUpdate"
display BGPTimeout{} = "BGPTimeout"
display BGPError{} = "BGPError"
display BGPEndOfStream{} = "BGPEndOfStream"
isKeepalive :: BGPMessage -> Bool
isKeepalive BGPKeepalive = True
isKeepalive _ = False

isOpen :: BGPMessage -> Bool
isOpen BGPOpen{} = True
isOpen _ = False

isUpdate :: BGPMessage -> Bool
isUpdate BGPUpdate{} = True
isUpdate _ = False

instance Binary BGPMessage where

    put (BGPOpen myAutonomousSystem holdTime bgpID caps) = do putWord8 _BGPOpen
                                                              putWord8 _BGPVersion
                                                              putWord16be myAutonomousSystem
                                                              putWord16be holdTime
                                                              putWord32le $ toHostAddress bgpID
                                                              let optionalParameters = buildOptionalParameters caps
                                                              putWord8 $ fromIntegral $ B.length optionalParameters
                                                              putByteString optionalParameters

    put (BGPUpdate withdrawnRoutes pathAttributes nlri) = do
                                                               putWord8 _BGPUpdate
                                                               putWord16be withdrawnRoutesLength
                                                               putLazyByteString withdrawnRoutes
                                                               putWord16be pathAttributesLength
                                                               putLazyByteString pathAttributes
                                                               putLazyByteString nlri
                                                               where
                                                                   withdrawnRoutesLength = fromIntegral $ L.length withdrawnRoutes
                                                                   pathAttributesLength = fromIntegral $ L.length pathAttributes
                                                                   nlriLength = fromIntegral $ L.length nlri
                                                                   nonPrefixLength = 16 + 2 + 1 + 2 + 2 + pathAttributesLength
                                                                   availablePrefixSpace = 4096 - nonPrefixLength

    put (BGPNotify code subCode caps) = do putWord8 _BGPNotify
                                           putWord8 $ encode8 code
                                           putWord8 subCode
                                           putLazyByteString $ encode caps

    put BGPKeepalive                                = putWord8 _BGPKeepalive

    get = label "BGPMessage" $ do
             msgType <- getWord8
             if | _BGPOpen == msgType -> do
                                           msgVer  <- getWord8
                                           unless (msgVer == _BGPVersion) (fail "Bad version(Open)")
                                           myAutonomousSystem <- getWord16be
                                           holdTime <- getWord16be
                                           bgpID <- getWord32le
                                           optionalParametersLength <- getWord8
                                           optionalParameters <- getRemainingLazyByteString
                                           unless (optionalParametersLength == fromIntegral (L.length optionalParameters))
                                                  (fail "optional parameter length wrong (Open)")
                                           return $ BGPOpen myAutonomousSystem holdTime (fromHostAddress bgpID)  ( parseOptionalParameters optionalParameters )
                | _BGPUpdate == msgType -> do
                                           withdrawnRoutesLength <- getWord16be
                                           withdrawnRoutes <- getLazyByteString $ fromIntegral withdrawnRoutesLength
                                           pathAttributesLength <- getWord16be
                                           pathAttributes <- getLazyByteString $ fromIntegral pathAttributesLength
                                           nlri <- getRemainingLazyByteString
                                           return $ BGPUpdate withdrawnRoutes pathAttributes nlri
                | _BGPNotify == msgType -> do
                                           errorCode <- getWord8
                                           errorSubcode <- getWord8
                                           errorData <- getRemainingLazyByteString
                                           -- return $ BGPNotify (decode8 errorCode) errorSubcode (decode errorData)
                                           -- decoding the error data depends on the type of notification!
                                           -- e.g. Bad Peer AS contains just the unwanted (?) peer AS number
                                           return $ BGPNotify (decode8 errorCode) errorSubcode errorData
                | _BGPKeepalive == msgType -> return BGPKeepalive
                | otherwise -> fail "Bad type code"
