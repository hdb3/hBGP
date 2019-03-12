module Log where

import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding (encodeUtf8)

say :: String -> IO ()
say = BS.hPutStrLn stderr . BS.pack
--say = BS.hPutStrLn stderr . encodeUtf8
trace = say
warn = say
info = say
