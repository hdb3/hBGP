module Log where

import System.IO

trace = hPutStrLn stderr
warn = hPutStrLn stderr
info = hPutStrLn stderr
