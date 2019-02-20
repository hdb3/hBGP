{-# LANGUAGE OverloadedStrings #-}

{-
 unit test framework for the BGPMessage parser/deparser BGPparse.hs
-}

module Main where
import qualified Data.ByteString.Lazy as L
import Data.Int(Int64)
import Data.Binary

import BGPparse hiding (encodeBGPByteString,decodeBGPByteString)
import GetBGPMsg
import Common
import RFC4271
import Capabilities

decodeBGPMessage :: L.ByteString -> BGPMessage
decodeBGPMessage = decode
decodeOrFailBGPMessage :: L.ByteString -> Either (L.ByteString, Int64, String) (L.ByteString, Int64, BGPMessage)
decodeOrFailBGPMessage = decodeOrFail
encodeBGPMessage :: BGPMessage -> L.ByteString
encodeBGPMessage = encode

wireEncodeBGPMessage :: BGPMessage -> L.ByteString
wireEncodeBGPMessage m = encode $ BGPByteString $ Right $ encode m
wireDecodeBGPMessage :: L.ByteString -> BGPMessage
wireDecodeBGPMessage m = decode m' where (BGPByteString (Right m')) = decode m

wireDecodeOrFail :: L.ByteString -> Either (L.ByteString, Int64, String) (L.ByteString, Int64, BGPMessage)
wireDecodeOrFail msg = either
                           (Left)
                           (\(_,_,(BGPByteString (Right m'))) -> decodeOrFail m')
                           (decodeOrFail msg)



-- myDecodeOrFail = decodeOrFailBGPMessage
-- myEncode = encodeBGPMessage
myDecodeOrFail = wireDecodeOrFail
myEncode = wireEncodeBGPMessage

messages = [ BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
           , BGPKeepalive
           , BGPNotify NotificationOPENMessageError (encode8 BadBGPIdentifier) L.empty
           , BGPNotify  NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) (capsEncode [ CapAS4 65520,  CapGracefulRestart False 0])
           , BGPUpdate "Withdrawn routes" "Path Attributes" "nlri"
           ]

main = do
     identityCheck "BGPOpen" $ BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0]
     identityCheck "BGPKeepalive" BGPKeepalive
     identityCheck "BGPNotify - empty data" $ BGPNotify NotificationOPENMessageError (encode8 BadBGPIdentifier) L.empty
     identityCheck "BGPNotify" $ BGPNotify  NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) (capsEncode [ CapAS4 65520,  CapGracefulRestart False 0])
     -- identityCheck "BGPNotify" $ BGPNotify  NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) [ CapAS4 65520,  CapGracefulRestart False 0]
     -- this version of BGPNotify does not explicitly support the multiple possible error data types....
     -- hence uses a bytsring instaed
     identityCheck "BGPUpdate" $ BGPUpdate "Withdrawn routes" "Path Attributes" "nlri"

identityCheck :: String -> BGPMessage -> IO ()
identityCheck name bgpMsg = do putStrLn $ "identityCheck on " ++ name
                               let encMsg = myEncode bgpMsg
                               print $ simpleHex $ L.toStrict encMsg
                               let decMsg = myDecodeOrFail encMsg
                               case decMsg of (Left (_,_,s)) -> do print $ "failed to decode message:" ++ s
                                                                   print $ simpleHex $ L.toStrict encMsg
                                              (Right ( _,_,decMsg')) -> if decMsg' == bgpMsg then do
                                                                                print "success"
                                                                                -- print $ simpleHex $ L.toStrict encMsg
                                                                        else do print "recoded message is not identical"
                                                                                print bgpMsg
                                                                                print $ simpleHex $ L.toStrict encMsg
                                                                                print decMsg'
                               putStrLn ""
