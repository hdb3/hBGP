{-# LANGUAGE CPP #-}
module Router.Log (LogMode(),logMode,debug,trace,event,info,warn,ifTrace,say) where

import System.IO
import Prelude hiding (log)
import qualified Data.ByteString.Char8 as BS

data LogMode =  Debug | Trace | Event | Info | Warn | Silent deriving (Eq, Ord, Show)

outputStream = stdout

#ifdef LOGMODE
logMode = LOGMODE
#else
logMode = Info
#endif

log :: LogMode -> String -> IO ()
log mode s = if mode >= logMode then say s else noOp s
noOp :: String -> IO ()
noOp _ = return ()
say :: String -> IO ()
say s = BS.hPutStrLn outputStream ( BS.pack s ) >> hFlush outputStream
debug = log Debug
trace = log Trace
event = log Event
info = log Info
warn = log Warn

ifTrace = Trace >= logMode
