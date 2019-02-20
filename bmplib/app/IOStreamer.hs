{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.ByteString as BS

import BMPlib

-- main = go rawBMPMessageParser action2
main = go bmpParser print

action2 msg = do
    print msg
    let msg' = extract msg
        parse' p bs = feed (parse p bs) BS.empty
    print $ parse' bmpMessageParser' msg'

go parser action = do

    stream <- parserToInputStream parser stdin
    loop stream where
    loop stream = do
        msg <- System.IO.Streams.read stream
        maybe (putStrLn "end of messages")
              ( \bmpMsg -> do action bmpMsg
                              loop stream )
              msg




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
