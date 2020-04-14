{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module BGPlib.BGPMessage where

import BGPlib.Capabilities
import BGPlib.LibCommon
import BGPlib.Prefixes
import BGPlib.RFC4271
import ByteString.StrictBuilder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Word

_BGPOpen = 1 :: Word8

_BGPUpdate = 2 :: Word8

_BGPNotify = 3 :: Word8

_BGPKeepalive = 4 :: Word8

_BGPVersion = 4 :: Word8

-- BGPMessage encodes network received inouts only, not messages which are intended for output
-- this is beceause a) it wraps input exceptions as record variants and b) it carries partially deparsed messages
-- TODO - consider whether preserving the bytestring format for undecoded attributes is the best approach,
-- rather than simply decoding at point of reception (and potentially improving parallelisation)
data BGPMessage
  = BGPOpen {myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: IPv4, caps :: [Capability]}
  | BGPKeepalive
  | BGPNotify {code :: EnumNotificationCode, subCode :: NotificationSubcode, errorData :: L.ByteString}
  | BGPUpdate {withdrawn :: [Prefix], attributes :: B.ByteString, nlri :: [Prefix]}
  | BGPTimeout
  | BGPError String
  | BGPEndOfStream
  deriving (Eq)

instance Show BGPMessage where
  show BGPOpen {..} = "Open {AS: " ++ show myAutonomousSystem ++ " Hold-time: " ++ show holdTime ++ " BGPID: " ++ show bgpID ++ " Caps: " ++ show caps ++ "}"
  show BGPUpdate {..} =
    if  | B.null attributes -> "Update {}"
        | null nlri -> "Update {attributes = " ++ toHex attributes ++ "}"
        | null withdrawn -> "Update {nlri = " ++ show nlri ++ ", attributes = " ++ toHex attributes ++ "}"
        | otherwise -> "Update {withdrawn = " ++ show withdrawn ++ ", nlri = " ++ show nlri ++ ", attributes = " ++ toHex attributes ++ "}"
  show BGPNotify {..} = "Notify: " ++ show code ++ " / " ++ show subCode ++ " errorData " ++ toHex' errorData ++ "}"
  show BGPKeepalive = "Keepalive"
  show BGPTimeout = "Timeout"
  show BGPEndOfStream = "BGPEndOfStream"
  show (BGPError s) = "Error: " ++ s

display :: BGPMessage -> String
display BGPOpen {} = "BGPOpen"
display BGPKeepalive {} = "BGPKeepalive"
display BGPNotify {} = "BGPNotify"
display BGPUpdate {} = "BGPUpdate"
display BGPTimeout {} = "BGPTimeout"
display BGPError {} = "BGPError"
display BGPEndOfStream {} = "BGPEndOfStream"

isKeepalive :: BGPMessage -> Bool
isKeepalive BGPKeepalive = True
isKeepalive _ = False

isOpen :: BGPMessage -> Bool
isOpen BGPOpen {} = True
isOpen _ = False

isUpdate :: BGPMessage -> Bool
isUpdate BGPUpdate {} = True
isUpdate _ = False

-- level 0 objects
-- TODO - why defined _here_?
data RcvStatus = Timeout | EndOfStream | Error String deriving (Eq, Show)

newtype BGPByteString = BGPByteString (Either RcvStatus L.ByteString) deriving (Eq)

wireFormat :: Builder -> Builder
wireFormat bldr = marker <> word16BE (fromIntegral $ builderLength bldr) <> bldr where marker = bytes $ B.replicate 16 0xff

builder :: BGPMessage -> Builder
builder BGPTimeout {} = error "should not try to send an input condition"
builder BGPError {} = error "should not try to send an input condition"
builder BGPEndOfStream {} = error "should not try to send an input condition"
builder BGPKeepalive =
  wireFormat $
    word8 _BGPKeepalive
builder (BGPOpen myAutonomousSystem holdTime bgpID caps) =
  wireFormat $
    word8 _BGPOpen
      <> word8 _BGPVersion
      <> word16BE myAutonomousSystem
      <> word16BE holdTime
      <> word32BE (byteSwap32 $ toHostAddress bgpID)
      <> parameterBuilder caps
builder (BGPNotify code subCode errData) =
  wireFormat $
    word8 _BGPNotify
      <> word8 (encode8 code)
      <> word8 subCode
      <> lazyBytes errData
  where
    wireFormat :: Builder -> Builder
    wireFormat bldr = marker <> word16BE (fromIntegral $ builderLength bldr) <> bldr where marker = bytes $ B.replicate 16 0xff
