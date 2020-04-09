{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module BGPlib.BGPMessage where

import BGPlib.Capabilities
import BGPlib.LibCommon
-- import BGPlib.PathAttributes
import BGPlib.Prefixes
import BGPlib.RFC4271
-- import Control.Monad (unless)
import Data.Binary
-- import Data.Binary.Get
-- import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- import Data.IP

_BGPOpen = 1 :: Word8

_BGPUpdate = 2 :: Word8

_BGPNotify = 3 :: Word8

_BGPKeepalive = 4 :: Word8

_BGPVersion = 4 :: Word8

data BGPMessage
  = BGPOpen {myAutonomousSystem :: Word16, holdTime :: Word16, bgpID :: IPv4, caps :: [Capability]}
  | BGPKeepalive
  | BGPNotify {code :: EnumNotificationCode, subCode :: NotificationSubcode, errorData :: L.ByteString}
  | BGPUpdate {withdrawn :: [Prefix], attributes :: B.ByteString, nlri :: [Prefix]}
  | BGPTimeout
  | BGPError String
  | BGPEndOfStream
  deriving (Eq, Generic)

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

toAS2 :: Word32 -> Word16
toAS2 as
  | as < 0x10000 = fromIntegral as
  | otherwise = 23456

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

data RcvStatus = Timeout | EndOfStream | Error String deriving (Eq, Show)

newtype BGPByteString = BGPByteString (Either RcvStatus L.ByteString) deriving (Eq)
