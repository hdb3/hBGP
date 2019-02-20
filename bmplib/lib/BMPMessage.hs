{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
module BMPMessage where
import Data.Word
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import Data.Binary.Put  ( putByteString, putWord8, putWord32be )
import Data.Binary.Get  ( getWord8, getWord32be, getByteString )
import Data.Monoid((<>))
import Control.Monad(when,unless)
import Control.Applicative((<|>))
import qualified Data.IP as IP
import qualified Data.Bits as Bits
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Base16


data BMPMsg = BMPRouteMonitoring RouteMonitoring
            | BMPPeerDown BMPPeerDownMsg
            | BMPPeerStats BMPPeerStatsMsg
            | BMPPeerUP BMPPeerUPMsg
            | BMPInitiation InitiationMessages
            | BMPTermination
            | BMPRouteMirroring deriving Show

data PerPeerHeader = PerPeerHeader { pphType :: Word8
                                   , pphFlags :: Word8
                                   , pphDistinguisher :: BS.ByteString
                                   , pphAddress :: IP.IP
                                   , pphAS :: Word32
                                   , pphBGPID :: IP.IPv4
                                   , pphTimeStampSecs :: Word32
                                   , pphTimeStampMicroSecs :: Word32
                                   , vFlag, lFlag, aFlag :: Bool
                                   }

data TLV = TLV { tlvType :: Word16
               , tlvBody :: BS.ByteString
               }

getTLV = do
    tlvType<- anyWord16be
    tlvLength <- anyWord16be
    tlvBody  <- Data.Attoparsec.ByteString.take (fromIntegral tlvLength)
    return TLV {..}

instance Show TLV where
    show TLV{..} = "TLV { type = " ++ show tlvType ++ ", body = " ++ toHex tlvBody ++ " }"

 
instance Show PerPeerHeader where
    show PerPeerHeader{..} = "PerPeerHeader { pphAddress = " ++ show pphAddress
                             ++ ", pphAS = " ++ show pphAS
                             ++ ", pphBGPID = " ++ show pphBGPID 
                             ++ ", pphTimeStampSecs = " ++ show pphTimeStampSecs
                             ++ ", flags = "
                             ++ if vFlag then "V" else "v"
                             ++ if lFlag then "L" else "l"
                             ++ if aFlag then "A" else "a"
                             ++ "}"

getPerPeerHeader = do
    pphType <- anyWord8
    pphFlags <- anyWord8
    let vFlag = Bits.testBit pphFlags 7  
        lFlag = Bits.testBit pphFlags 6  
        aFlag = Bits.testBit pphFlags 5  
    pphDistinguisher <- Data.Attoparsec.ByteString.take 8
    pphAddress <- getIPv4IPv6 vFlag
    pphAS <- anyWord32be
    pphBGPID <- zIPv4
    pphTimeStampSecs <- anyWord32be
    pphTimeStampMicroSecs <- anyWord32be
    return PerPeerHeader{..}


getIPv4IPv6 ip6Flag = if ip6Flag then ipIPv6 else Data.Attoparsec.ByteString.take 12 >> ipIPv4

atto p s = ( p' <|> return Nothing ) <?> s where
    p' = do tmp <- p
            return (Just tmp)
     
bmpMessageParser :: Parser (Maybe BMPMsg)
bmpMessageParser = atto bmpMessageParser' "BMP payload parser"

bmpMessageParser' :: Parser BMPMsg
bmpMessageParser' = do
    msgType <- anyWord8
    case msgType of 
        0 -> getBMPRouteMonitoring
        1 -> getBMPPeerStats
        2 -> getBMPPeerDown
        3 -> getBMPPeerUP
        4 -> getBMPInitiation
        5 -> return BMPTermination
        6 -> return BMPRouteMirroring
        _ -> fail "invalid message type"


-- -----------------------
-- Peer Monitoring
-- -----------------------

data RouteMonitoring = RouteMonitoring PerPeerHeader BGPByteString deriving Show

getBMPRouteMonitoring = do
    perPeerHeader <- getPerPeerHeader
    bGPMessage <- getBGPByteString
    return $ BMPRouteMonitoring $ RouteMonitoring perPeerHeader bGPMessage



-- -----------------------
-- Initiation
-- -----------------------

newtype InitiationMessages = InitiationMessages [TLV]
instance Show InitiationMessages where
    show (InitiationMessages tlvs) = "\n" ++ showInitiationTLVs tlvs

showInitiationTLVs :: [TLV] -> String
showInitiationTLVs = unlines . map showInitiationTLV where
    showInitiationTLV TLV{..} =
        ( case tlvType of 0 -> "String: "
                          1 -> "sysDescr: "
                          2 -> "sysName: "
                          _ -> "unknown type (" ++ show tlvType ++ "): "
        ) ++ Data.ByteString.Char8.unpack tlvBody

getBMPInitiation = do
    tlvs <- many1 getTLV
    return $ BMPInitiation $ InitiationMessages tlvs

-- -----------------------
-- Peer Down
-- -----------------------

data BMPPeerDownMsg = BMPPeerDownMsg { pph :: PerPeerHeader
                                     , reason :: Word8
                                     , notification :: Maybe BGPByteString
                                     , eventCode :: Maybe Word16
                                 }

instance Show BMPPeerDownMsg where
    show BMPPeerDownMsg{..} = "BMPPeerDown { pph = " ++ show pph
                            ++ "reason = "
                            ++ ( case reason of
                                   1 -> "local termination, BGP Notification: " ++ maybe "<empty>" show notification
                                   2 -> "local termination, FSM event code: " ++ maybe "<empty>" show eventCode
                                   3 -> "remote termination, BGP Notification: " ++ maybe "<empty>" show notification
                                   4 -> "remote peer disconnected"
                                   5 -> "reporting disabled"
                                   _ -> "unknown reason code: " ++ show reason
                               )
                            ++ " }"

getBMPPeerDown:: Parser BMPMsg
getBMPPeerDown = do
    pph <- getPerPeerHeader
    reason <- anyWord8
    (notification, eventCode) <-
        case reason of
            1 -> do bgpMsg <- getBGPByteString
                    return (Just bgpMsg, Nothing)
            2 -> do ec <- anyWord16be
                    return (Nothing, Just ec)
            3 -> do bgpMsg <- getBGPByteString
                    return (Just bgpMsg, Nothing)
            _ -> return (Nothing, Nothing)

    return $ BMPPeerDown BMPPeerDownMsg {..}

-- -----------------------
-- -----------------------
-- Peer UP
-- -----------------------

data BMPPeerUPMsg = BMPPeerUPMsg { pph :: PerPeerHeader
                                 , localAddress :: IP.IP
                                 , localPort, remotePort :: Word16
                                 , sentOpen, receivedOpen :: BGPByteString
                                 , information :: [TLV]
                                 }

instance Show BMPPeerUPMsg where
    show BMPPeerUPMsg{..} = "BMPPeerUPMsg { pph = " ++ show pph 
                            ++ "localAddress = " ++ show localAddress
                            ++ ", localPort = " ++ show localPort
                            ++ ", remotePort = " ++ show remotePort
                            ++ show information
                            ++ " }"
getBMPPeerUP:: Parser BMPMsg
getBMPPeerUP = do
    pph <- getPerPeerHeader
    localAddress <- getIPv4IPv6 (vFlag pph)
    localPort <- anyWord16be
    remotePort <- anyWord16be
    sentOpen <- getBGPByteString
    receivedOpen <- getBGPByteString
    information <- many' getTLV
    return $ BMPPeerUP BMPPeerUPMsg {..}


-- -----------------------
-- Per Peer Statistics
-- -----------------------

data BMPPeerStatsMsg = BMPPeerStatsMsg { pph :: PerPeerHeader
                                 , stats :: [TLV]
                                 }

instance Show BMPPeerStatsMsg where
    show BMPPeerStatsMsg{..} = "BMPPeerStatsMsg { pph = " ++ show pph 
                            ++ ", stats = " ++ show stats
                            ++ " }"

getBMPPeerStats:: Parser BMPMsg
getBMPPeerStats = do
    pph <- getPerPeerHeader
    statsCount <- anyWord32be
    stats <- many' getTLV
    unless (fromIntegral statsCount == length stats) ( fail "invalid BMP statsCount")
    return $ BMPPeerStats BMPPeerStatsMsg {..}


-- -----------------------

newtype BGPByteString = BGPByteString BS.ByteString
instance Show BGPByteString where
    show (BGPByteString bs) = toHex bs

getBGPByteString = do
    marker <- Data.Attoparsec.ByteString.take 16
    when (marker /= BS.replicate 16 0xff ) ( fail "invalid BGP message header")
    msgLen <- anyWord16be
    when (msgLen > 4096 ) ( fail "invalid BGP message length")
    bs <- Data.Attoparsec.ByteString.take (fromIntegral msgLen - 18)
    return $ BGPByteString bs

-- -----------------------

bmpParser :: Parser (Maybe BMPMsg)
bmpParser = ( do msg <- bsParser
                 return $ parse' bmpMessageParser' msg
            ) <|> return Nothing
    where
        -- NB without 'feed .. BS.empty' the parser will only return Partial
        -- because the syntax of BMP requires reading TLVs until the message is exhausted
        parse' p bs = maybeResult $ feed (parse p bs) BS.empty

bsParser :: Parser BS.ByteString
bsParser = do
    word8 0x03
    msgLen <- anyWord32be
    when (msgLen < 5 || msgLen > 0xffff) ( fail "invalid message length")
    Data.Attoparsec.ByteString.take (fromIntegral msgLen - 5)

-- HexByteString exists in order to implement a useful instance of 'show'

newtype HexByteString = HexByteString BS.ByteString deriving (Eq,Read)
instance Show HexByteString where
    show (HexByteString bs) = let toHex = Data.ByteString.Char8.unpack . Data.ByteString.Base16.encode
                              in "[" ++ toHex bs ++ "]"

hbLength (HexByteString hb) = BS.length hb

getHexByteString n = do bs <- getByteString n
                        return ( HexByteString bs)

instance Binary.Binary HexByteString where
    get = undefined
    put (HexByteString bs) = putByteString bs

-- AttoParsec parsers for IOStream adapters

newtype BMPMessageRaw = BMPMessageRaw BS.ByteString
extract (BMPMessageRaw bs) = bs

instance Show BMPMessageRaw where
    show (BMPMessageRaw bs) = toHex bs

instance Binary.Binary BMPMessageRaw where
    get = Binary.label "BMPMessageRaw" $ do
        version <- getWord8
        unless (version == 0x03) (fail $ "BMPMessageRaw: incorrect version - expected 3 got " ++ show version) -- version hardcoded
        msgLength <- getWord32be             
        payload <- getByteString (fromIntegral msgLength -5)
        return $ BMPMessageRaw payload
    put (BMPMessageRaw payload) = putWord8 0x03 <> putWord32be (fromIntegral $ 5 + BS.length payload) <> Binary.put payload 

rawBMPMessageParser :: Parser (Maybe BMPMessageRaw)
rawBMPMessageParser = atto rawBMPMessageParser' "BMP wire format parser"

rawBMPMessageParser' :: Parser BMPMessageRaw
rawBMPMessageParser' = do
-- TODO cal back to bsParse instead
    word8 0x03
    msgLen <- anyWord32be
    when (msgLen < 5 || msgLen > 0xffff) ( fail "invalid message length")
    payload <- Data.Attoparsec.ByteString.take (fromIntegral msgLen - 5)
    return $ BMPMessageRaw payload


-- copied from zserv - maybe need some common library for this stuff?

toHex = Data.ByteString.Char8.unpack . Data.ByteString.Base16.encode

zIPv4 = zIPv4Parser
zIPv4Parser :: Parser IP.IPv4
zIPv4Parser = do
    v4address <- anyWord32le
    return $ IP.fromHostAddress v4address

ipIPv4 :: Parser IP.IP
ipIPv4 = fmap IP.IPv4 zIPv4

ipIPv6 :: Parser IP.IP
ipIPv6 = fmap IP.IPv6 ipIPv6Parser
ipIPv6Parser = do
    v6address <- Data.Attoparsec.ByteString.take 16
    return $ (IP.toIPv6b . map fromIntegral . BS.unpack) v6address



{-
   Copyright 2018 Nicholas Hart

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
