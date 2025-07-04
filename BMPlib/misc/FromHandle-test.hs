module Main where

import BMPMessage
import Data.Binary
import FromHandle
import System.IO (stdin)
import qualified System.IO.Streams

-- main = generator stdin (get :: Get BMPMessageRaw)

main = do
  stream <- streamBinary stdin -- :: System.IO.Streams.Stream BMPMessageRaw ()
  loop stream
  where
    loop :: System.IO.Streams.InputStream BMPMessageRaw -> IO ()
    loop s = do
      bmpMsg <- System.IO.Streams.read s
      maybe
        (putStrLn "EOF")
        ( \msg -> do
            print msg
            loop s
        )
        bmpMsg
