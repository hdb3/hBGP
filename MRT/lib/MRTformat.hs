{-# LANGUAGE MultiWayIf,RecordWildCards #-}
module MRTformat where

--import qualified Data.ByteString as BS  -- import for Strict version
--import qualified Data.Attoparsec.ByteString as DAB  -- import for Strict version
--import Data.Attoparsec.ByteString(Parser,word8,anyWord8,count)  -- import for Strict version

import qualified Data.Attoparsec.Lazy as DAB  -- import for Lazy version
import Data.Attoparsec.Lazy(Parser,word8,anyWord8,count)  -- import for Lazy version
import qualified Data.ByteString.Lazy as BS  -- import for Lazy version

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base16
import Data.Attoparsec.Binary
import Control.Monad(unless)
import Data.IP hiding(ipv4,ipv6)
import Data.Bits
import Data.Word
import Data.List(isSuffixOf)
import System.Environment(getArgs)
import Codec.Compression.GZip(decompress)
import MRTPrefixes
import BogonFilter

newtype BGPMessage = BGPMessage SBS.ByteString
instance Show BGPMessage where
    show (BGPMessage bs) = "BGPMessage: " ++ toHex bs

newtype BGPAttributes = BGPAttributes SBS.ByteString
instance Show BGPAttributes where
    show (BGPAttributes bs) = "BGPAttributes: " ++ toHex bs

fromBGPAttributes (BGPAttributes bs) = bs

newtype HexByteString = HexByteString SBS.ByteString
instance Show HexByteString where
    show (HexByteString bs) = toHex bs

newtype AS4 = AS4 Word32
instance Show AS4 where
    show (AS4 w32) = "AS" ++ show w32

newtype Timestamp = Timestamp Word32
instance Show Timestamp where
    show (Timestamp w32) = show w32 ++ "UTC"

newtype BGPid = BGPid IPv4
instance Show BGPid where
    show (BGPid ip) = show ip

toHex :: Char8.ByteString -> String
toHex = Char8.unpack . Data.ByteString.Base16.encode

fromHex :: Char8.ByteString -> Char8.ByteString
fromHex = Data.ByteString.Base16.decodeLenient

data MRTRecord = MRTPeerIndexTable { tdBGPID :: BGPid , tdViewName :: String, peerTable :: [MRTPeer] } 
                 | RIBIPV4Unicast { re4SequenceNumber :: Word32 , re4Length :: Word8 , re4Address :: IPv4 , re4RIB :: [RIBEntry] }
                 | RIBIPV6Unicast { re6SequenceNumber :: Word32 , re6Length :: Word8 , re6Address :: IPv6 , re6RIB :: [RIBEntry] }
                 | MRTUnimplemented { xTimestamp :: Timestamp , xType, xSubtype :: Word16 , xMessage :: HexByteString }
                 | BGP4MPMessageAS4 { msgAS4PeerAS,msgAS4LocalAS :: AS4 , msgAS4IfIndex :: Word16, msgAS4PeerIP,msgAS4LocalIP :: IP, msgAS4Message :: BGPMessage }
                 | BGP4MPStateChangeAS4 { scAS4PeerAS,scAS4LocalAS :: AS4 , scAS4IfIndex :: Word16, scAS4PeerIP,scAS4LocalIP :: IP, scOldState, scNewState:: BGP4MPState }
                 | RIBv1IPv4 { r1v4ViewNumber, r1v4SeqNumber :: Word16, r1v4Prefix :: IPv4 , r1v4Length :: Word8, r1v4Timestamp :: Timestamp , r1v4PeerAddress :: IPv4, r1v4PeerAS :: AS4, r1v4Attributes :: BGPAttributes }
                 | RIBv1IPv6 { r1v6ViewNumber, r1v6SeqNumber :: Word16, r1v6Prefix :: IPv6 , r1v6Length :: Word8, r1v6Timestamp :: Timestamp , r1v6PeerAddress :: IPv6, r1v6PeerAS :: AS4, r1v6Attributes :: BGPAttributes }
                 deriving Show

data MRTPeer = MRTPeer { mrtPeerBGPID :: BGPid, mrtPeerASN :: AS4 , mrtPeerIPAddress :: IP } -- deriving Show
instance Show MRTPeer where
    show MRTPeer {..} = "Peer: " ++ show mrtPeerBGPID ++ "/" ++ show mrtPeerASN ++ "@" ++ show mrtPeerIPAddress
data RIBEntry = RIBEntry { rePeerIndex :: Word16 , reOriginatedTime :: Timestamp , reAttributes :: BGPAttributes } deriving Show
data BGP4MPState = BGP4MPIdle | BGP4MPConnect | BGP4MPActive | BGP4MPOpenSent | BGP4MPOpenConfirm | BGP4MPEstablished deriving Show

-- convenience function: getMRTTableDumpV2

mrtBogonFilter :: [MRTRecord] -> [MRTRecord]
mrtBogonFilter = filter p where
    p RIBIPV4Unicast{..} = bogonFilter (Prefix (re4Length,toHostAddress re4Address))
    p _ = True

getMRTTableDumpV2 :: IO [MRTRecord] -- first member is guaranteed to be MRTlib.MRTPeerIndexTable
getMRTTableDumpV2 = validate <$> getMRT
    where
    validate ( a@MRTPeerIndexTable{} : b) = a:b
    validate _ = error "expected MRTPeerIndexTable as first record in RIB file"
    getMRT = fmap mrtParse BS.getContents

getMRTTableDumps :: IO [[MRTRecord]] -- first members are guaranteed to be MRTlib.MRTPeerIndexTable
getMRTTableDumps = do
    args <- getArgs
    if null args
    then fmap (:[]) getMRTTableDumpV2
    else mapM fgetMRTTableDumpV2 args
    where
    fgetMRTTableDumpV2 fname = fmap ( mrtParse . decompress) (BS.readFile fname)
    
-- read only one MRT file from the argument list, or stdin        
getMRTTableDump :: IO [MRTRecord]
getMRTTableDump =
    do args <- getArgs
       mrtParse <$> if null args then BS.getContents
                    else if "gz" `isSuffixOf` head args then decompress <$> BS.readFile (head args)
                    else BS.readFile (head args)

               

--
-- Core attoparsec parser follows
--

-- this is the lazy version.....
mrtParse :: BS.ByteString -> [MRTRecord]
mrtParse bs = mrtParse' (parse' bs) where
    parse' = DAB.parse rawMRTParse
    mrtParse' (DAB.Done _ r) = r
    mrtParse' (DAB.Fail _ sx s) = error $ show (s,sx)

{-
-- this is the strict version.....
mrtParse :: BS.ByteString -> [MRTRecord]
mrtParse bs = mrtParse' (parse' bs) where
    parse' bs' = DAB.feed (DAB.parse rawMRTParse bs') BS.empty
    mrtParse' (DAB.Done _ r) = r
    mrtParse' (DAB.Fail _ sx s) = error $ show (s,sx)
    mrtParse' (DAB.Partial _ ) = error "Partial unexpected!"
-}

rawMRTParse :: Parser [MRTRecord]
rawMRTParse = DAB.many1 rawMRTParser

rawMRTParser :: Parser MRTRecord
rawMRTParser = do
    ts <- timestamp
    t  <- anyWord16be
    st <- anyWord16be
    l  <- anyWord32be
    case (t,st) of
        (12,1) -> parseRIBv1IPv4
        (12,2) -> parseRIBv1IPv6
        (13,1) -> parseMRTPeerIndexTable
        (13,2) -> parseRIBIPV4Unicast
        (13,4) -> parseRIBIPV6Unicast
        (16,4) -> parseBGP4MPMessageAS4
        (16,5) -> parseBGP4MPStateChangeAS4
        (_,_)  -> do m  <- DAB.take (fromIntegral l )
                     return $ MRTUnimplemented ts t st (HexByteString m)

bs16 :: Parser Char8.ByteString
bs16 = do
    l <- anyWord16be
    DAB.take (fromIntegral l )

bgpAttributes :: Parser BGPAttributes
bgpAttributes = fmap BGPAttributes bs16
bgpMessage :: Parser BGPMessage
bgpMessage = do
    marker <- DAB.take 16
    unless ( marker == SBS.replicate 16 0xff ) (fail "BGP marker synchronisation error")
    l <- anyWord16be
    BGPMessage <$> DAB.take (fromIntegral l - 18)

string16 :: Parser String
string16 = fmap Char8.unpack bs16

timestamp :: Parser Timestamp
timestamp = fmap Timestamp anyWord32be

as4 :: Parser AS4
as4 = fmap AS4 anyWord32be

as2 :: Parser AS4
as2 = fmap (AS4 . fromIntegral) anyWord16be

bgpid :: Parser BGPid
bgpid = fmap BGPid ipv4

ipv4 :: Parser IPv4
ipv4 = fmap fromHostAddress anyWord32le

ipv6 :: Parser IPv6
ipv6 = do
    b1 <- anyWord32be
    b2 <- anyWord32be
    b3 <- anyWord32be
    b4 <- anyWord32be
    return $ fromHostAddress6 (b1,b2,b3,b4)

parseIPv4 :: Parser IP
parseIPv4 = fmap IPv4 ipv4

parseIPv6 :: Parser IP
parseIPv6 = fmap IPv6 ipv6

parseMRTPeerIndexTable :: Parser MRTRecord
parseMRTPeerIndexTable = do
    tdBGPID <- bgpid
    tdViewName <- string16
    c <- anyWord16be
    peerTable <- count (fromIntegral c) parseMRTPeer
    return MRTPeerIndexTable{..}
    where
    parseMRTPeer = do
        peerType <- anyWord8
        let isV6 = testBit peerType 0
            isAS4 = testBit peerType 1
        mrtPeerBGPID <- bgpid
        mrtPeerIPAddress <- if isV6 then parseIPv6 else parseIPv4
        mrtPeerASN <- if isAS4 then as4 else as2
        return MRTPeer{..}

parseRIBv1IPv4 :: Parser MRTRecord
parseRIBv1IPv4 = do
    r1v4ViewNumber <- anyWord16be
    r1v4SeqNumber <- anyWord16be
    r1v4Prefix <- ipv4
    r1v4Length <- anyWord8
    _ <- word8 1
    r1v4Timestamp <- timestamp
    r1v4PeerAddress <- ipv4
    r1v4PeerAS <- as2
    r1v4Attributes <- bgpAttributes
    return RIBv1IPv4{..}

parseRIBv1IPv6 :: Parser MRTRecord
parseRIBv1IPv6 = do
    r1v6ViewNumber <- anyWord16be
    r1v6SeqNumber <- anyWord16be
    r1v6Prefix <- ipv6
    r1v6Length <- anyWord8
    _ <- word8 1
    r1v6Timestamp <- timestamp
    r1v6PeerAddress <- ipv6
    r1v6PeerAS <- as2
    r1v6Attributes <- bgpAttributes
    return RIBv1IPv6{..}

parseRIBIPV4Unicast :: Parser MRTRecord
parseRIBIPV4Unicast = do
    re4SequenceNumber <- anyWord32be
    (re4Length,re4Address) <- parsePrefixV4
    re4RIB <- parseRIB
    return RIBIPV4Unicast{..}

parseRIBIPV6Unicast :: Parser MRTRecord
parseRIBIPV6Unicast = do
    re6SequenceNumber <- anyWord32be
    (re6Length,re6Address) <- parsePrefixV6
    re6RIB <- parseRIB
    return RIBIPV6Unicast{..}

parseRIB :: Parser [RIBEntry]
parseRIB = do
    c <- fmap fromIntegral anyWord16be
    count c parseRIBEntry
    where
    parseRIBEntry = do
        rePeerIndex <- anyWord16be
        reOriginatedTime <- timestamp
        reAttributes <- bgpAttributes
        return RIBEntry{..}

-- shamelessly copied from my zserv code for zvPrefixIPv4Parser
parsePrefixV4 :: Parser (Word8,IPv4) 
parsePrefixV4 = do
    plen <- anyWord8
    pfx <- if | plen == 0  -> return 0
              | plen < 9   -> readPrefix1Byte
              | plen < 17  -> readPrefix2Byte
              | plen < 25  -> readPrefix3Byte
              | plen < 33  -> readPrefix4Byte
    return (plen, fromHostAddress $ byteSwap32 pfx)
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

parsePrefixV6 :: Parser (Word8,IPv6)
parsePrefixV6 = do
    pLen <- anyWord8
    let bytePLen = ((fromIntegral pLen - 1) `div` 8) + 1 -- DANGER Will Robinson (-ve arithmetic on unsigned words will wrap around badly, so rearrange at your peril...)
    bytes <- count bytePLen anyWord8
    let extendedBytes = bytes ++ replicate (16-bytePLen) 0
    return (pLen,toIPv6b $ map fromIntegral extendedBytes)

parseBGP4MPMessageAS4 :: Parser MRTRecord
parseBGP4MPMessageAS4 = do
        msgAS4PeerAS <- as4
        msgAS4LocalAS <- as4
        msgAS4IfIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = afi == 2
        msgAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        msgAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        msgAS4Message <- bgpMessage
        return BGP4MPMessageAS4{..}  

parseBGP4MPStateChangeAS4 :: Parser MRTRecord
parseBGP4MPStateChangeAS4 = do
        scAS4PeerAS <- as4
        scAS4LocalAS <- as4
        scAS4IfIndex <- anyWord16be
        afi <- anyWord16be
        let isV6 = afi == 2
        scAS4PeerIP <- if isV6 then parseIPv6 else parseIPv4
        scAS4LocalIP <- if isV6 then parseIPv6 else parseIPv4
        scOldState <- fmap toBGP4MPState anyWord16be
        scNewState <- fmap toBGP4MPState anyWord16be
        return BGP4MPStateChangeAS4{..}  

toBGP4MPState :: Word16 -> BGP4MPState
toBGP4MPState 1 = BGP4MPIdle
toBGP4MPState 2 = BGP4MPConnect
toBGP4MPState 3 = BGP4MPActive
toBGP4MPState 4 = BGP4MPOpenSent
toBGP4MPState 5 = BGP4MPOpenConfirm
toBGP4MPState 6 = BGP4MPEstablished
toBGP4MPState _ = error "invalid BGP FSM state"

