{-# LANGUAGE RecordWildCards,MultiWayIf,OverloadedStrings #-}
module WireFormat where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when)
import Data.IP
import Data.Bits
import Data.Word

import ZMsg
import ZSpec

zFlowParser :: Parser [ZMsg]
zFlowParser = many1 (zMessageParser' ZClient)


zRawFlowParser :: Parser [(Word16,BS.ByteString)]
zRawFlowParser = many1 zRawMessageParser
zRawMessageParser :: Parser (Word16,BS.ByteString)
zRawMessageParser = do
    msgLen <- anyWord16be
    word8 0xff
    word8 0x03
    word16be 0x0000 -- VRF ID
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    -- cmd <- anyWord16be
    -- pl <- DAB.take (fromIntegral msgLen - 8)
    -- return (cmd,pl)

    -- this version replaces the command word in the bytestring to allow another parser to be run over it
    (cmdBS,cmd) <- match anyWord16be
    pl <- DAB.take (fromIntegral msgLen - 8)
    return (cmd,BS.append cmdBS pl)

-- This is for a dumb passthrough stream
zDumbParser :: Parser (Maybe BS.ByteString)
zDumbParser = do
    msgLen <- anyWord16be
    word8 0xff
    word8 0x03
    word16be 0x0000 -- VRF ID
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    pl <- DAB.take (fromIntegral msgLen - 6)
    return (Just pl)

zMessageParser :: ZRole -> Parser (Maybe ZMsg)
zMessageParser role = ( zMessageParser'' role <|> return Nothing ) <?> "zserv wire format parser"
zMessageParser'' role = do
    tmp <- zMessageParser' role
    return $ Just tmp

zMessageParser' role = do
    msgLen <- anyWord16be
    word8 0xff -- 'marker'
    word8 0x03 -- version - earlier than three has incompatible format - later is unknown
    word16be 0x0000 -- VRF ID - for now we would not want to know about multiple VRFs - the default value is zero
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    -- msg <- Data.Attoparsec.ByteString.take $ fromIntegral (msgLen - 6) 
    -- return $ Just msg
    zParser role $ fromIntegral (msgLen - 6)


zParser :: ZRole -> Int -> Parser ZMsg
zParser role n' = do
   let n = n'-2
   cmd' <- anyWord16be
   let cmd = cmd'
   --let cmd = assert (cmd `elem` zKnownCommands) cmd'
   if | cmd == _ZEBRA_HELLO && n == 1 -> do protocol <- anyWord8
                                            return $ ZMHello protocol


      | cmd == _ZEBRA_INTERFACE_ADD ->
          if n == 0 then return ZMQInterfaceAdd
          else do interface <- zInterfaceParser
                  return $ ZMInterfaceAdd interface

      | cmd == _ZEBRA_INTERFACE_DELETE ->
          do interface <- zInterfaceParser
             return $ ZMInterfaceDelete interface

      | cmd == _ZEBRA_INTERFACE_ADDRESS_ADD ->
          do zia <- zInterfaceAddressParser
             return $ ZMInterfaceAddressAdd zia

      | cmd == _ZEBRA_INTERFACE_ADDRESS_DELETE ->
          do zia <- zInterfaceAddressParser
             return $ ZMInterfaceAddressDelete zia

      | cmd == _ZEBRA_INTERFACE_UP ->
          do interface <- zInterfaceParser
             return $ ZMInterfaceUp interface

      | cmd == _ZEBRA_INTERFACE_DOWN ->
          do interface <- zInterfaceParser
             return $ ZMInterfaceDown interface

      | cmd == _ZEBRA_ROUTER_ID_UPDATE && n == 6 ->
              do prefix <- zPrefix8Parser
                 return $ ZMRouterIDUpdate prefix

      | cmd == _ZEBRA_IPV4_ROUTE_ADD ->
          case role of ZClient -> do route <- zRouteParser
                                     return $ ZMIPV4RouteAdd route
                       ZServer -> do route <- zServerRouteParser
                                     return $ ZMIPV4ServerRouteAdd route

      | cmd == _ZEBRA_IPV4_ROUTE_DELETE ->
          case role of ZClient -> do route <- zRouteParser
                                     return $ ZMIPV4RouteDelete route
                       ZServer -> do route <- zServerRouteParser
                                     return $ ZMIPV4ServerRouteDelete route

      | cmd == _ZEBRA_REDISTRIBUTE_ADD ->
          do routeType <- anyWord8
             return $ ZMRedistributeAdd routeType

      | cmd == _ZEBRA_REDISTRIBUTE_DELETE ->
          do routeType <- anyWord8
             return $ ZMRedistributeDelete routeType

      | cmd == _ZEBRA_REDISTRIBUTE_DEFAULT_ADD ->
          do -- routeType <- anyWord8
             return $ ZMRedistributeDefaultAdd

      | cmd == _ZEBRA_REDISTRIBUTE_DEFAULT_DELETE ->
          do -- routeType <- anyWord8
             return $ ZMRedistributeDefaultDelete

      | cmd == _ZEBRA_NEXTHOP_REGISTER ->
          do reg <- zNextHopRegisterParser
             return $ ZMNextHopRegister reg

      | cmd == _ZEBRA_NEXTHOP_UNREGISTER ->
          do reg <- zNextHopRegisterParser
             return $ ZMNextHopUnregister reg

      | cmd == _ZEBRA_NEXTHOP_UPDATE ->
          do nh <- zNextHopUpdateParser
             return $ ZMNextHopUpdate nh

      | cmd == _ZEBRA_ROUTER_ID_ADD  && n == 0 -> return ZMQRouterIdAdd -- I suspect that the zero length version is a query...

      | otherwise -> do
            payload <- DAB.take n
            return $ ZMUnknown cmd (HexByteString payload)

-- zRouteNextHopParser - specialised from zNextHopParser for use in zRouteParser
-- in this message version the format is hardwired (address+ifindex) and each hop has a dummy NH count byte preceding

zRouteNextHopParser :: Parser ZNextHop
zRouteNextHopParser = do
    _ <- anyWord8
    ipv4 <- zIPv4
    word8 0x01
    w32 <- anyWord32be
    return $ ZNHIPv4Ifindex ipv4 w32

zNextHopParser :: Parser ZNextHop
zNextHopParser = do
    nextHopType <- anyWord8
    if | nextHopType == _ZEBRA_NEXTHOP_BLACKHOLE -> return ZNHBlackhole
       | nextHopType == _ZEBRA_NEXTHOP_IPV4 -> do
             ipv4 <- zIPv4
             return $ ZNHIPv4 ipv4
       | nextHopType == _ZEBRA_NEXTHOP_IFINDEX -> do
             w32 <- anyWord32be
             return $ ZNHIfindex w32

zNextHopRegisterParser :: Parser ZNextHopRegister
zNextHopRegisterParser = do
    w8 <- anyWord8
    let connected = w8 /= 0
    prefix <- zPrefix16Parser
    return ZNextHopRegister{..}
    

zNextHopUpdateParser :: Parser ZNextHopUpdate
zNextHopUpdateParser = do
    let flags = 0x00
    prefix <- zPrefix16Parser
    metric <- anyWord32be
    nextHopCount <- anyWord8
    nexthops <- count (fromIntegral nextHopCount) zNextHopParser
    return ZNextHopUpdate{..}

zInterfaceAddressParser :: Parser ZInterfaceAddress
zInterfaceAddressParser = do
    ifindex <- anyWord32be
    flags <- anyWord8
    afi  <- anyWord8
    if | afi == _AF_INET  -> zInterfaceAddressParserV4 ifindex flags
       | afi == _AF_INET6 -> zInterfaceAddressParserV6 ifindex flags

zInterfaceAddressParserV4 ifindex flags = do
    addressA <- zIPv4
    plen <- anyWord8
    addressB <- zIPv4
    return ZInterfaceAddressV4{..}

zInterfaceAddressParserV6 ifindex flags = do
    v6addressA <- zIPv6
    plen <- anyWord8
    v6addressB <- zIPv6
    return ZInterfaceAddressV6{..}

zIPv4 = zIPv4Parser
zIPv4Parser :: Parser IPv4
zIPv4Parser = do
    v4address <- anyWord32le
    return $ fromHostAddress v4address

zIPv6 = zIPv6Parser
zIPv6Parser :: Parser IPv6
zIPv6Parser = do
    v6address <- DAB.take 16
    return $ (toIPv6b . map fromIntegral . BS.unpack) v6address

-- 2018/10/08 - fixing up this function
--            - I'm not clear if this is/was used elsewhere/before
--            - BUT - it is not currently correct/consistent with zebra/zserv.c
--            - the changes now being made

zServerRouteParser :: Parser ZServerRoute
zServerRouteParser = do
    zrType <- anyWord8
    zrFlags <- anyWord8
    zrMsg <- anyWord8
    -- for zserv -> client there is no SAFI filed it seems...
    zrPrefix <-  zvPrefixIPv4Parser
    zrNextHops <- if testBit zrMsg _ZAPI_MESSAGE_NEXTHOP then do nextHopCount <- peekWord8' -- not sure if the route delete message will be parsed like this..
                                                                 count (fromIntegral nextHopCount) zRouteNextHopParser
                                                         else return []
    zrDistance <- if testBit zrMsg _ZAPI_MESSAGE_DISTANCE then fmap Just anyWord8 else return Nothing
    zrMetric <- if testBit zrMsg _ZAPI_MESSAGE_METRIC then fmap Just anyWord32be else return Nothing
    zrMtu <- if testBit zrMsg _ZAPI_MESSAGE_MTU then fmap Just anyWord32be else return Nothing
    zrTag <- if testBit zrMsg _ZAPI_MESSAGE_TAG then fmap Just anyWord32be else return Nothing
    return ZServerRoute{..}

zRouteParser :: Parser ZRoute
zRouteParser = do
    zrType <- anyWord8
    zrFlags <- anyWord8
    zrMsg <- anyWord8
    zrSafi <- anyWord16be
    zrPrefix <-  zvPrefixIPv4Parser
    zrNextHops <- if testBit zrMsg _ZAPI_MESSAGE_NEXTHOP then do nextHopCount <- anyWord8
                                                                 count (fromIntegral nextHopCount) zNextHopParser
                                                         else return []
    zrDistance <- if testBit zrMsg _ZAPI_MESSAGE_DISTANCE then fmap Just anyWord8 else return Nothing
    zrMetric <- if testBit zrMsg _ZAPI_MESSAGE_METRIC then fmap Just anyWord32be else return Nothing
    zrMtu <- if testBit zrMsg _ZAPI_MESSAGE_MTU then fmap Just anyWord32be else return Nothing
    zrTag <- if testBit zrMsg _ZAPI_MESSAGE_TAG then fmap Just anyWord32be else return Nothing
    return ZRoute{..}

-- refer to zclient.c for the specification of this structure

zInterfaceParser :: Parser ZInterface
zInterfaceParser = do
    ifname' <- DAB.take 20
    let ifname = BS.takeWhile ( 0 /= ) ifname'
    ifindex <- anyWord32be 
    status <- anyWord8
    ifFlags <- anyWord64be
    metric <- anyWord32be
    ifmtu <- anyWord32be
    ifmtu6 <- anyWord32be
    bandwidth <- anyWord32be
    linkLayerType <- anyWord32be
    hardwareAddressLength <- anyWord32be
    hardwareAddress' <- DAB.take (fromIntegral hardwareAddressLength)
    let hardwareAddress = HexByteString hardwareAddress'
    word8 0x00
    return ZInterface {..} 

-- Prefix parsers
--
-- there are multiple wireformats for prefixes - afi can be 8 or 16 bits
--                                             - prefix can be compressed or not
--                                             - prefix length can come before or after the prefix
--                                             - can be IPv4 or IPv6
--
-- However, they all have the same semantics
-- the challenge is that if they are transformed into a single canonical form then the subsequent encoder has to be selected
-- rather than simply writing the prefixes as different types
-- in order to simplify the external API I choose a common type and accept that there must be explicit encoding.
-- this implies that the prefixes SHOULD NOT be made instances of Binary - rather explicit named put instances should be defined over the common types


-- this does not parse the AFI, and parses a variable length prefix, prefix length first (obvs!)
zvPrefixIPv4Parser :: Parser ZPrefix
zvPrefixIPv4Parser = do
    plen <- anyWord8
    prefix' <- 
        if | plen == 0  -> return 0
           | plen < 9   -> readPrefix1Byte 
           | plen < 17  -> readPrefix2Byte 
           | plen < 25  -> readPrefix3Byte 
           | plen < 33  -> readPrefix4Byte 
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

-- this parses 16 bit AFI, fixed length prefix, prefix last
zPrefix16Parser :: Parser ZPrefix
zPrefix16Parser = do
    afi16 <- anyWord16be
    plen <- anyWord8
    let afi = fromIntegral afi16 :: Word8
    if | afi == _AF_INET  -> do v4address <- zIPv4
                                return ZPrefixV4{..}
       | afi == _AF_INET6 -> do v6address <- zIPv6
                                return ZPrefixV6{..}
       | otherwise -> error $ "zPrefix16Parser - invalid AFI - " ++ show afi

-- this parses 8 bit AFI, fixed length prefix, prefix length last
zPrefix8Parser :: Parser ZPrefix
zPrefix8Parser = do
    afi <- anyWord8
    if | afi == _AF_INET -> do v4address <- zIPv4
                               plen <- anyWord8
                               return ZPrefixV4{..}
       | afi == _AF_INET6 -> do v6address <- zIPv6
                                plen <- anyWord8
                                return ZPrefixV6{..}
       | otherwise -> error $ "zPrefix8Parser - invalid AFI - " ++ show afi
