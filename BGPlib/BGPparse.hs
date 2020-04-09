{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module BGPlib.BGPparse where
import Debug.Trace
import BGPlib.BGPMessage
import BGPlib.Capabilities
import BGPlib.LibCommon
import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

decodeBGPByteString :: BGPByteString -> BGPMessage
decodeBGPByteString (BGPByteString (Left Timeout)) = BGPTimeout
decodeBGPByteString (BGPByteString (Left EndOfStream)) = BGPEndOfStream
decodeBGPByteString (BGPByteString (Left (Error s))) = BGPError s
decodeBGPByteString (BGPByteString (Right lbs)) = decode lbs :: BGPMessage

instance {-# OVERLAPPING #-} Binary [BGPMessage] where
  put _ = error "it would not make sense to encode BGPmessage list without a wireformat envelope"

encodeBGPMessage :: BGPMessage -> L.ByteString
encodeBGPMessage = encode

instance Binary BGPMessage where
  put (BGPOpen myAutonomousSystem holdTime bgpID caps) = do
    putWord8 _BGPOpen
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
    putLazyByteString withdrawnRoutesBS
    putWord16be pathAttributesLength
    putByteString pathAttributes
    putLazyByteString nlriBS
    where
      withdrawnRoutesBS = encode withdrawnRoutes
      nlriBS = encode nlri
      withdrawnRoutesLength = fromIntegral $ L.length withdrawnRoutesBS
      pathAttributesLength = fromIntegral $ B.length pathAttributes
  put (BGPNotify code subCode errData) = do
    putWord8 _BGPNotify
    putWord8 $ encode8 code
    putWord8 subCode
    putLazyByteString errData
  put BGPKeepalive = putWord8 _BGPKeepalive

  get = label "BGPMessage" $ trace "***TRACE BGPparse get" $ do
    msgType <- getWord8
    if  | _BGPOpen == msgType -> do
          msgVer <- getWord8
          unless (msgVer == _BGPVersion) (fail "Bad version(Open)")
          myAutonomousSystem <- getWord16be
          holdTime <- getWord16be
          bgpID <- getWord32le
          optionalParametersLength <- getWord8
          optionalParameters <- getRemainingLazyByteString
          unless
            (optionalParametersLength == fromIntegral (L.length optionalParameters))
            (fail "optional parameter length wrong (Open)")
          return $ BGPOpen myAutonomousSystem holdTime (fromHostAddress bgpID) [] -- really this is an error because we are now using atto
        | _BGPUpdate == msgType -> do
          withdrawnRoutesLength <- getWord16be
          withdrawnRoutes <- getLazyByteString $ fromIntegral withdrawnRoutesLength
          pathAttributesLength <- getWord16be
          pathAttributes <- getByteString $ fromIntegral pathAttributesLength
          nlri <- getRemainingLazyByteString
          return $ BGPUpdate (decode withdrawnRoutes) pathAttributes (decode nlri)
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
