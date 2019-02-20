module BMPlib (module BMPlib, module BMPMessage) where
import qualified System.Environment
import qualified Data.IP as IP
import qualified Text.Read
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as NS
import qualified System.IO
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams

import BMPMessage

getBMPStreamStdIn = Streams.parserToInputStream bmpParser Streams.stdin
getBMPStreamPath path = System.IO.openFile path System.IO.ReadMode >>= Streams.handleToInputStream >>= Streams.parserToInputStream bmpParser
getBMPStreamInet addr = do
    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    NS.connect sock ( NS.SockAddrInet 5000 (IP.toHostAddress addr))
    handle <- NS.socketToHandle sock System.IO.ReadWriteMode
    Streams.handleToInputStream handle >>= Streams.parserToInputStream bmpParser



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
