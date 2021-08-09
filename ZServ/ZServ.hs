{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZServ.ZServ
  ( module ZServ.ZServ,
    module ZServ.ZMsg,
    module ZServ.ZMsgBinary,
    module ZServ.ZSpec,
    module ZServ.WireFormat,
    module ZServ.Debug,
  )
where

-- consider exporting some functions from System.IO.Streams, (as Streams?)

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.IP
import Network.Socket
import System.IO
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec.ByteString
import ZServ.Debug
import ZServ.WireFormat
import ZServ.ZMsg
import ZServ.ZMsgBinary
import ZServ.ZSpec

getZStreamInet address = getZStream ZClient (SockAddrInet 2600 (toHostAddress address), AF_INET)

getZStreamUnix path = getZStream ZClient (SockAddrUnix path, AF_UNIX)

getZServerStreamInet address = getZStream ZServer (SockAddrInet 2600 (toHostAddress address), AF_INET)

getZServerStreamUnix path = getZStream ZServer (SockAddrUnix path, AF_UNIX)

getZStream role (address, family) = do
  sock <- socket family Stream defaultProtocol
  connect sock address
  putStrLn "connected"
  handle <- socketToHandle sock ReadWriteMode
  inputStream <- Streams.handleToInputStream handle
  zStream <- parserToInputStream (zMessageParser role) inputStream
  outputStream <- Streams.makeOutputStream $ \m -> case m of
    Just zmsg -> L.hPut handle $ encode (ZMsgRaw 0 zmsg)
    Nothing -> return () -- could close the handle/socket?
  return (zStream, outputStream)

toIPv4Range ZPrefixV4 {..} = makeAddrRange v4address (fromIntegral plen)
toIPv4Range other = error "toIPv4Range: can't convert other things"

fromIPv4Range ipv4range =
  let (v4address, plen') = addrRangePair ipv4range
      plen = fromIntegral plen'
   in ZPrefixV4 {..}

routeBase =
  ZRoute
    { zrType = 9,
      zrFlags = 9,
      zrSafi = 1,
      zrPrefix = undefined,
      zrNextHops = [],
      zrDistance = Nothing,
      zrMetric = Nothing,
      zrMtu = Nothing,
      zrTag = Nothing
    }

addRoute stream pfx nh =
  let route = routeBase {zrPrefix = fromIPv4Range pfx, zrNextHops = [ZNHIPv4 nh]} :: ZRoute
   in Streams.write (Just $ ZMIPV4RouteAdd route) stream

delRoute stream pfx =
  let route = routeBase {zrPrefix = fromIPv4Range pfx} :: ZRoute
   in Streams.write (Just $ ZMIPV4RouteDelete route) stream

zservRegister stream protocol = Streams.write (Just $ ZMHello protocol) stream

zservRequestRouterId = Streams.write (Just ZMQRouterIdAdd)

zservRequestInterface = Streams.write (Just ZMQInterfaceAdd)

zservRequestRedistributeSystem = Streams.write (Just $ ZMRedistributeAdd _ZEBRA_ROUTE_SYSTEM)

zservRequestRedistributeKernel = Streams.write (Just $ ZMRedistributeAdd _ZEBRA_ROUTE_KERNEL)

zservRequestRedistributeConnected = Streams.write (Just $ ZMRedistributeAdd _ZEBRA_ROUTE_CONNECT)

zservRequestRedistributeStatic = Streams.write (Just $ ZMRedistributeAdd _ZEBRA_ROUTE_STATIC)

zservRequestRedistributeAll stream =
  zservRequestRedistributeSystem stream
    >> zservRequestRedistributeKernel stream
    >> zservRequestRedistributeConnected stream
    >> zservRequestRedistributeStatic stream

getZRoute :: ZMsg -> Maybe (AddrRange IPv4, Maybe IPv4)
getZRoute (ZMIPV4ServerRouteAdd ZServerRoute {..}) = Just (toIPv4Range zrPrefix, nextHop zrNextHops)
  where
    nextHop ((ZNHIPv4Ifindex ip _) : _) = Just ip
    nextHop ([]) = Nothing
    nextHop other = error "getZRoute unexpected next hop"
getZRoute (ZMIPV4ServerRouteDelete ZServerRoute {..}) = Just (toIPv4Range zrPrefix, Nothing) where
getZRoute (ZMInterfaceAddressAdd ZInterfaceAddressV4 {..}) = Just (makeAddrRange addressA (fromIntegral plen), Just "127.0.0.1")
getZRoute _ = Nothing

zservReadLoop stream = do
  msg <- Streams.read stream
  maybe
    (putStrLn "end of messages")
    ( \zMsg -> do
        print zMsg
        zservReadLoop stream
    )
    msg
