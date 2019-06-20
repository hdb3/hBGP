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
data BGPHandle = BGPHandle Socket ()
 
getBGPHandle :: Socket -> IO BGPHandle
getBGPHandle sock = do -- h <- socketToHandle sock ReadWriteMode
                       --mv <- newEmptyMVar
                       --return $ BGPHandle h mv
                       return $ BGPHandle sock ()

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
--bgpRcv (BGPHandle h _ ) t | t > 0     = decodeBGPByteString <$> (getRawMsg h t)
bgpRcv (BGPHandle h mvar ) t | t > 0     = bgpRcv'
                             | otherwise = error "state machine should never set zero timeout for a handle read"
    where
    bgpRcv' = do
    -- for diagnostics, force parse of the message as soon as it is received, and attempt to store it
    -- getRawMsg returns BGPByteString, which wraps errors and timeouts in an Either over a regular ByteString
    -- decodeBGPByteString handles all of these cases: delivers BGPMessage which also wraps exceptions, though not in Either
    -- note: decodeBGPByteString calls (Binary) decode, which can fail....
    --
    -- I want to store the lowest level value....
    -- then force the conversion to BGPMessage (and catch errors immediately)
    -- and then, when the BGPMessage is BGPUpdate, force full parsing of that
    -- (again, catch errors immediately)
    -- in every case we replace the stored value....
    


        let
            exBGPMessage :: SomeException -> IO BGPMessage
            exBGPMessage e = die $ "exBGPMessage " ++ show e
            --exUpdate :: SomeException -> IO (Maybe ParsedUpdate)
            --exUpdate e = die $ "exUpdate " ++ (show e)

        rawMsg <- getRawMsg h t
        handle exBGPMessage ( evaluate $ force $ decodeBGPByteString rawMsg )
        --let bgpMsg = evaluate $ force $ decodeBGPByteString rawMsg
        --bgpMsg <- handle exBGPMessage ( evaluate $ force $ decodeBGPByteString rawMsg )
        --maybeUpdate <- handle exUpdate ( if isUpdate bgpMsg then evaluate $ Just $ force $ decodeUpdate bgpMsg else return Nothing )
            --maybeUpdate = if isUpdate bgpMsg then Just $ evaluate $ force $ decodeUpdate bgpMsg else Nothing
        --tryTakeMVar mvar
        --putMVar mvar (rawMsg,maybeUpdate)
        --return bgpMsg
