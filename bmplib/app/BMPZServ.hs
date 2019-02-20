{-# LANGUAGE RecordWildCards #-}
{-#LANGUAGE OverloadedStrings #-}
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

import BMPlib
import BGPlib hiding (BGPByteString,TLV,getBGPByteString)
import ZServ

main = do
    args <- System.Environment.getArgs
  
    bmpStream <- if length args < 2
                 then getBMPStreamStdIn
                 else getBMPStreamInet (Text.Read.read (args !! 1) :: IP.IPv4)

    ( zStreamIn, ztreamOut ) <-
        do (inputStream,outputStream) <-
               if null args
               then getZStreamUnix "/var/run/quagga/zserv.api"
               else let s = head args
               in maybe ( getZStreamUnix s )
                        getZStreamInet
                        ( Text.Read.readMaybe s :: Maybe IP.IPv4)

           zservRegister outputStream _ZEBRA_ROUTE_BGP
           return (inputStream,outputStream)

    loop bmpStream ztreamOut -- read BMP until EOF
    zservReadLoop zStreamIn -- if EOF is seen then start monitoring zserv

    where

    loop input output = do
        msg <- Streams.read input
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do processBMPMsg output bmpMsg
                              loop input output )
              msg


--processBMPMsg :: MVar BMPState -> BMPMsg -> IO()
processBMPMsg _ (BMPPeerUP msg@BMPPeerUPMsg{..}) = do
    putStrLn $ "BMP Peer Up from " ++ show msg

processBMPMsg zStream (BMPRouteMonitoring (RouteMonitoring pph bgpMsg)) = do
    let updates = decodeAddrRange $ nlri $ fromBGP bgpMsg
        nextHop = getNextHop $ decodeAttributes $ attributes $ fromBGP bgpMsg
    let addRoute' pfx = addRoute zStream pfx nextHop

    mapM_ addRoute' updates 

    putStrLn $ "BMP RM " ++ show (pphBGPID pph)
             ++ " prefixes: " ++ show updates

-- fall through action for unhandled BMP messages
processBMPMsg _ bmpMsg = print bmpMsg

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
