module Log where

import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)

noOp :: String -> IO ()
noOp _ = return ()
say :: String -> IO ()
say = BS.hPutStrLn stderr . BS.pack
--say = BS.hPutStrLn stderr . encodeUtf8
debug = noOp
trace = noOp
warn = say
info = say
