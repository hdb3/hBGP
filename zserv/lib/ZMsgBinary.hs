{-# LANGUAGE MultiWayIf,RecordWildCards,FlexibleInstances #-}
module ZMsgBinary where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.IP
import Data.Bits
import Data.Monoid((<>))
import Data.Maybe(isJust)
import Control.Monad(unless)
import Data.Foldable(forM_)

import ZMsg
import ZSpec

-- TODO!!!!
-- make all of the implicit put word16/32 explicit for endianess!!!

-- Entry points / public interface

encodeRawZMsg :: BS.ByteString -> L.ByteString
encodeRawZMsg = runPut . putRawZMsg

putRawZMsg :: BS.ByteString -> Put
putRawZMsg zmsg = putWord16be msgLen <> putWord8 0xff <> putWord8 0x03 <> putWord16be 0x0000 <> putByteString zmsg
        where msgLen = fromIntegral $ 6 + BS.length zmsg

instance Binary ZMsgRaw where
    get = undefined
    put (ZMsgRaw vrf zmsg) = putWord16be msgLen <> putWord8 0xff <> putWord8 0x03 <> putWord16be vrf <> putByteString zmsgBS
        where zmsgBS = L.toStrict $ encode zmsg
              msgLen = fromIntegral $ 6 + BS.length zmsgBS


instance Binary ZMsg where
    get = undefined
    put ( ZMHello w8 ) = put _ZEBRA_HELLO <> put w8
    put ( ZMQRouterIdAdd ) = put _ZEBRA_ROUTER_ID_ADD
    put ( ZMQInterfaceAdd ) = put _ZEBRA_INTERFACE_ADD
    put ( ZMRouterIDUpdate prefix ) = put _ZEBRA_ROUTER_ID_UPDATE <> putZPrefix8 prefix
    put ( ZMNextHopRegister reg ) = put _ZEBRA_NEXTHOP_REGISTER <> put reg
    put ( ZMNextHopUnregister reg ) = put _ZEBRA_NEXTHOP_UNREGISTER <> put reg
    put ( ZMInterfaceAdd interface ) = put _ZEBRA_INTERFACE_ADD <> put interface
    put ( ZMInterfaceDelete interface ) = put _ZEBRA_INTERFACE_DELETE <> put interface
    put ( ZMInterfaceUp interface ) = put _ZEBRA_INTERFACE_UP <> put interface
    put ( ZMInterfaceDown interface ) = put _ZEBRA_INTERFACE_DOWN <> put interface
    put ( ZMInterfaceAddressAdd intAddr ) = put _ZEBRA_INTERFACE_ADDRESS_ADD <> put intAddr
    put ( ZMInterfaceAddressDelete intAddr ) = put _ZEBRA_INTERFACE_ADDRESS_DELETE <> put intAddr
    put ( ZMIPV4RouteAdd route ) = put _ZEBRA_IPV4_ROUTE_ADD <> put route
    put ( ZMIPV4RouteDelete route ) = put _ZEBRA_IPV4_ROUTE_DELETE <> put route
    put ( ZMIPV4ServerRouteAdd route ) = put _ZEBRA_IPV4_ROUTE_ADD <> put route
    put ( ZMIPV4ServerRouteDelete route ) = put _ZEBRA_IPV4_ROUTE_DELETE <> put route
    put ( ZMRedistributeAdd routeType ) = put _ZEBRA_REDISTRIBUTE_ADD <> put routeType
    put ( ZMRedistributeDelete routeType ) = put _ZEBRA_REDISTRIBUTE_DELETE <> put routeType
    put ( ZMRedistributeDefaultAdd ) = put _ZEBRA_REDISTRIBUTE_DEFAULT_ADD
    put ( ZMRedistributeDefaultDelete ) = put _ZEBRA_REDISTRIBUTE_DEFAULT_DELETE
    put ( ZMNextHopUpdate update ) = put _ZEBRA_NEXTHOP_UPDATE <> put update
    put ( ZMUnknown cmd (HexByteString bs) ) = put cmd <> putByteString bs
    -- put z = error $ "put ZMsg failed for ZMsg: " ++ show z 
    -- the ZMUnknown encoder mask gaps which might be interesting to capture via exception

-- **********************************************************************************

instance Binary IPv4 where

    get = undefined
    put = putWord32le . toHostAddress -- note le not be because of the way Data.IP represents IPv4 internally!

instance Binary IPv6 where

    get = undefined
    -- this would be the most direct form but Data.IP hides this constructor so we have to be indirect for simplicity
    -- put (IP6 (w1, w2, w3, w4)) = put w1 <> put w2 <> put w3 <> put w4
    put ipV6 = mapM_ (putWord8 . fromIntegral) (fromIPv6b ipV6)


-- this puts 16 bit AFI, fixed length prefix, prefix last
putZPrefix16 :: ZPrefix -> Put
putZPrefix16 ZPrefixV4{..} = put (fromIntegral _AF_INET :: Word16 ) <> put plen <> put v4address
putZPrefix16 ZPrefixV6{..} = put (fromIntegral _AF_INET6 :: Word16 ) <> put plen <> put v6address

-- this puts 8 bit AFI, fixed length prefix, prefix length last
putZPrefix8 :: ZPrefix -> Put
putZPrefix8 ZPrefixV4{..} = put _AF_INET  <> put v4address <> put plen
putZPrefix8 ZPrefixV6{..} = put _AF_INET6 <> put v6address <> put plen

-- placeholder for fixing ZRoute to hold v4 and v6...
putzvPrefix ZPrefixV6{..} = undefined
putzvPrefix ZPrefixV4{..} = do
    put plen
    let address = byteSwap32 $ toHostAddress v4address
    -- note byteSwap because of the way Data.IP represents IPv4 internally!
    if | plen == 0  -> return ()
       | plen < 9   -> putWord8 (fromIntegral (unsafeShiftR address 24))
       | plen < 17  -> putWord16be ( fromIntegral (unsafeShiftR address 16))
       | plen < 25  -> putWord16be ( fromIntegral (unsafeShiftR address 16)) >> putWord8 (fromIntegral (unsafeShiftR address 8))
       | plen < 33  -> put v4address
       | otherwise -> error $ "putzvPrefix: invalid plen - " ++ show plen

-- putRoutePrefixV4 ZPrefixV6{..} = undefined -- TODO
-- putRoutePrefixV4 pfx@ZPrefixV4{..} = putWord16be 0x01 <> -- 'SAFI' for IPv4!?
--                                  putzvPrefix pfx 

instance Binary ZInterfaceAddress where
    get = undefined
    put ZInterfaceAddressV4{..} = put ifindex <> put flags <> put _AF_INET <> put addressA <> put plen <> put addressB
    put ZInterfaceAddressV6{..} = put ifindex <> put flags <> put _AF_INET6 <> put v6addressA <> put plen <> put v6addressB

instance {-# OVERLAPPING #-} Binary [ZNextHop] where
    get = undefined
    put nexthops = put (fromIntegral (length nexthops) :: Word8 ) <> mapM_ put nexthops

instance Binary ZNextHop where
    get = undefined
    put ZNHBlackhole = put _ZEBRA_NEXTHOP_BLACKHOLE
    put ( ZNHIPv4 ip) = put _ZEBRA_NEXTHOP_IPV4 <> put ip
    put ( ZNHIfindex ifindex) = put _ZEBRA_NEXTHOP_IFINDEX <> put ifindex
    put ( ZNHIPv4Ifindex ipv4 ifindex) = put _ZEBRA_NEXTHOP_IPV4_IFINDEX <> put ipv4 <> putWord8 0x01 <> put ifindex
    --TODO .....
    put ( ZNHIPv6 _ ) = undefined
    put ( ZNHIPv6Ifindex _ _) = undefined


instance Binary ZNextHopRegister where
    get = undefined
    put ZNextHopRegister{..} = putWord8 connectedW8 <> putZPrefix16 prefix where
        connectedW8 = if connected  then 0x01 else 0x00

instance Binary ZNextHopUpdate where
    get = undefined
    put ZNextHopUpdate{..} = putZPrefix16 prefix <> put metric <> put (fromIntegral (length nexthops) :: Word8 )<> mapM_ put nexthops


instance Binary ZInterface where
    get = undefined
    put ZInterface{..} = putByteString (pad 20 ifname) <> put ifindex <> put status <> put ifFlags <> put metric <> put ifmtu <> put ifmtu6 <> put bandwidth <> put linkLayerType <> putCountedByteString hardwareAddress <> putWord8 0x00
putCountedByteString (HexByteString bs) = putWord32be (fromIntegral (BS.length bs)) <> putByteString bs
pad n bs = BS.take n (BS.append bs (BS.replicate n 0x00))


instance Binary ZRoute where
    get = undefined
    put ZRoute{..} =
        do
        let zrMsg      = if null zrNextHops then 0 else bit _ZAPI_MESSAGE_NEXTHOP :: Word8
            zrMsg'     = zrMsg      .|. if isJust zrDistance then bit _ZAPI_MESSAGE_DISTANCE else 0
            zrMsg''    = zrMsg'     .|. if isJust zrMetric then bit _ZAPI_MESSAGE_METRIC else 0
            zrMsg'''   = zrMsg''    .|. if isJust zrMtu then bit _ZAPI_MESSAGE_MTU else 0
            zrMsg''''  = zrMsg'''   .|. if isJust zrTag then bit _ZAPI_MESSAGE_TAG else 0
        put zrType <> put zrFlags <> put zrMsg'''' <> putWord16be 0x01 <> putzvPrefix zrPrefix
                                                      -- client side has to sent 'safi'
        unless (null zrNextHops) (put zrNextHops)
        forM_ zrDistance put
        forM_ zrMetric put
        forM_ zrMtu put
        forM_ zrTag put

instance Binary ZServerRoute where
    get = undefined
    put ZServerRoute{..} =
        do
        let zrMsg      = if null zrNextHops then 0 else bit _ZAPI_MESSAGE_NEXTHOP .|. bit _ZAPI_MESSAGE_IFINDEX :: Word8
            zrMsg'     = zrMsg      .|. if isJust zrDistance then bit _ZAPI_MESSAGE_DISTANCE else 0
            zrMsg''    = zrMsg'     .|. if isJust zrMetric then bit _ZAPI_MESSAGE_METRIC else 0
            zrMsg'''   = zrMsg''    .|. if isJust zrMtu then bit _ZAPI_MESSAGE_MTU else 0
            zrMsg''''  = zrMsg'''   .|. if isJust zrTag then bit _ZAPI_MESSAGE_TAG else 0
            -- this is an ugly hack but then so is zserv.c: the documentation is entirely obscure
            -- my interprettaion is that the format for multiple hops is (1byte hop count) followed by (n x (ipv4, 0x01,ifindex) in the case of ipv4+ifindex
            -- a cleaner read would allow alternative nexthop formats, which would be flagged by the API_MESSAGE_NEXTHOP / ZAPI_MESSAGE_IFINDEX flag bits
            putServerNextHop ( ZNHIPv4Ifindex ipv4 ifindex) = put ipv4 <> putWord8 0x01 <> put ifindex
            putServerNextHops hops = putWord8 (fromIntegral $ length hops) <> mapM_ putServerNextHop hops

        put zrType <> put zrFlags <> put zrMsg'''' <> putzvPrefix zrPrefix
                                                      -- server side has not to sent 'safi'
        unless (null zrNextHops) (putServerNextHops zrNextHops)
        forM_ zrDistance put
        forM_ zrMetric put
        forM_ zrMtu put
        forM_ zrTag put
