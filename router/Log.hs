module Log where

import System.IO
import qualified Data.ByteString.Char8 as BS

noOp :: String -> IO ()
noOp _ = return ()
say :: String -> IO ()
say = BS.hPutStrLn stderr . BS.pack
debug = noOp
trace = noOp
warn = say
info = say
