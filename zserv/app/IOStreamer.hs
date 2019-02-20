module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString

import WireFormat
import ZMsg
import Debug

main = do
    -- is <- handleToInputStream stdin
    zStream <- parserToInputStream zMessageParser stdin
    loop zStream where
    -- decodeBgp = decode :: L.ByteString -> BGPMessage
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              print zMsg
                              -- putStrLn (identify $ decodeBgp $ L.fromStrict rawMsg)
                              loop stream )
                                             -- )
              msg
