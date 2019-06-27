module BGPlib.BGPHandle where

import BGPlib.GetBGPMsg(sndRawMessage, sndRawMessages, getRawMsg)
import BGPlib.BGPparse(BGPMessage,decodeBGPByteString)
import Network.Socket(Socket,socketToHandle)
import System.IO.Error(catchIOError)
import System.IO(IOMode( ReadWriteMode ),Handle, hClose)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import qualified Control.Concurrent
import Control.Exception(throw,Exception)


newtype BGPIOException = BGPIOException String deriving Show
instance Exception BGPIOException


-- Follows the BGPHandle abstraction, which should allow the debugging of data integrity isses and also pluggable alternative socket access

newtype BGPHandle = BGPHandle Handle

getBGPHandle :: Socket -> IO BGPHandle
getBGPHandle sock = BGPHandle <$> socketToHandle sock ReadWriteMode

bgpClose :: BGPHandle -> IO ()
bgpClose (BGPHandle h) = hClose h

bgpSnd :: BGPHandle -> BGPMessage -> IO()
bgpSnd (BGPHandle h) msg | 4079 > lengthEncodedMsg = catchIOError ( sndRawMessage h encodedMsg )
                                                                  (\e -> throw $ BGPIOException (show (e :: IOError)))
                         | otherwise = error $ "encoded message too long in bgpSnd " ++ show lengthEncodedMsg
                         where encodedMsg = encode msg
                               lengthEncodedMsg = L.length encodedMsg

bgpSndAll :: BGPHandle -> [BGPMessage] -> IO()
bgpSndAll (BGPHandle h) msgs = catchIOError ( sndRawMessages h $ map encode msgs )
                                            (\e -> throw $ BGPIOException (show (e :: IOError)))

bgpRcv :: BGPHandle -> Int -> IO BGPMessage
bgpRcv (BGPHandle h) t | t > 0     = fmap decodeBGPByteString (getRawMsg h t)
                       | otherwise = error "state machine should never set zero timeout for a handle read"
