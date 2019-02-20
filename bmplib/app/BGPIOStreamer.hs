{-# LANGUAGE RecordWildCards #-}
module Main where
import qualified System.Environment
import qualified Data.IP as IP
import qualified Text.Read
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as NS
import qualified System.IO
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams
import Control.Concurrent.MVar

import BMPlib
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)

main = do
    args <- System.Environment.getArgs
  
    source <- if null args
              then
                  return Streams.stdin
              else do let s = args !! 0
                          ip = IP.toHostAddress (Text.Read.read s :: IP.IPv4)
                      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                      NS.connect sock ( NS.SockAddrInet 5000 ip)
                      handle <- NS.socketToHandle sock System.IO.ReadWriteMode
                      Streams.handleToInputStream handle

    stream <- Streams.parserToInputStream bmpParser source
    bmpState <- newMVar (BMPState [] [] )
    loop stream bmpState where
    loop stream st = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do processBMPMsg st bmpMsg
                              -- action bmpMsg st
                              loop stream st )
              msg

action msg st = do
    putStrLn $ showBMPMsg msg

data BMPState = BMPState { peers :: [BMPPeerUPMsg] , rib :: [IP.AddrRange IP.IPv4] }

processBMPMsg :: MVar BMPState -> BMPMsg -> IO()
processBMPMsg m (BMPPeerUP msg@BMPPeerUPMsg{..}) = do
    putStrLn $ "BMP Peer Up from " ++ show msg
    bmpState <- takeMVar m
    let peers' = msg : (peers bmpState)
    putMVar m bmpState{peers=peers'}

processBMPMsg m (BMPRouteMonitoring (RouteMonitoring pph bgpMsg)) = do
    let updates = decodeAddrRange $ nlri $ fromBGP bgpMsg
    putStrLn $ "BMP RM " ++ show (pphBGPID pph)
             ++ " prefixes: " ++ show updates
    bmpState <- takeMVar m
    let rib' = (rib bmpState) ++ updates
    putStrLn $ "rIb size: " ++ show (length rib')
    putMVar m bmpState{rib=rib'}

processBMPMsg m bmpMsg = print bmpMsg

showBMPMsg :: BMPMsg -> String
showBMPMsg (BMPPeerUP x@BMPPeerUPMsg{..}) = show x ++ showBGPByteString sentOpen ++ showBGPByteString receivedOpen 
showBMPMsg (BMPRouteMonitoring ( RouteMonitoring perPeerHeader bGPMessage)) = "BMPRouteMonitoring { " ++ show perPeerHeader ++ showBGPByteString bGPMessage ++ " }"
showBMPMsg x = show x


showBGPByteString :: BGPByteString -> String
showBGPByteString = showBGP . fromBGP
showBGP BGPUpdate {..} = " BGPUpdate: "
                    ++ "\nNLRI:       " ++ show ( decodeAddrRange nlri )
                    ++ "\nWithdrawn:  " ++ show ( decodeAddrRange withdrawn )
                    ++ "\nAttributes: " ++ show ( decodeAttributes attributes )

showBGP x = show x

fromBGP :: BGPByteString -> BGPMessage
fromBGP (BGPByteString bs) = Binary.decode $ L.fromStrict bs



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
