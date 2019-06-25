module Router.BGPHandle where

import BGPlib.GetBGPMsg(BGPByteString,sndRawMessage, sndRawMessages, getRawMsg)
import BGPlib.BGPparse(BGPMessage,decodeBGPByteString,isUpdate)
import Network.Socket(Socket,close)
import System.IO.Error(catchIOError)
--import System.IO(IOMode( ReadWriteMode ),Handle, hClose)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Control.Concurrent(MVar,newEmptyMVar,putMVar,tryTakeMVar)
import Control.Exception(handle,evaluate,throw,Exception,SomeException)
import Control.DeepSeq(force)
import System.Exit(die)

--import BGPRib.Update(ParsedUpdate,decodeUpdate)


newtype BGPIOException = BGPIOException String deriving Show
instance Exception BGPIOException


-- Follows the BGPHandle abstraction, which should allow the debugging of data integrity isses and also pluggable alternative socket access

--newtype BGPHandle = BGPHandle Handle
--data BGPHandle = BGPHandle Handle (MVar(BGPByteString,Maybe ParsedUpdate))
data BGPHandle = BGPHandle { socket :: Socket , msgQueue :: [BGPMessage] };
 
getBGPHandle :: Socket -> IO BGPHandle
getBGPHandle sock = do -- h <- socketToHandle sock ReadWriteMode
                       --mv <- newEmptyMVar
                       --return $ BGPHandle h mv
                       return $ BGPHandle sock []

bgpClose :: BGPHandle -> IO ()
bgpClose (BGPHandle h _ ) = close h

bgpSnd :: BGPHandle -> BGPMessage -> IO()
bgpSnd (BGPHandle h _ ) msg | 4079 > lengthEncodedMsg = catchIOError ( sndRawMessage h encodedMsg )
                                                                  (\e -> throw $ BGPIOException (show (e :: IOError)))
                         | otherwise = error $ "encoded message too long in bgpSnd " ++ show lengthEncodedMsg
                         where encodedMsg = encode msg
                               lengthEncodedMsg = L.length encodedMsg

bgpSndAll :: BGPHandle -> [BGPMessage] -> IO()
bgpSndAll (BGPHandle h _ ) msgs = catchIOError ( sndRawMessages h $ map encode msgs )
                                            (\e -> throw $ BGPIOException (show (e :: IOError)))

bgpRcv :: BGPHandle -> Int -> IO BGPMessage
bgpRcv (BGPHandle h q ) t | t > 0     = bgpRcv'
                          | otherwise = error "state machine should never set zero timeout for a handle read"
    where
    bgpRcv' = do
        let
            exBGPMessage :: SomeException -> IO BGPMessage
            exBGPMessage e = die $ "exBGPMessage " ++ show e

        rawMsg <- getRawMsg h t
        handle exBGPMessage ( evaluate $ force $ decodeBGPByteString rawMsg )
