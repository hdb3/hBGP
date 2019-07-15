module Router.BGPHandle where

import BGPlib.BGPparse(BGPMessage(..))
import Network.Socket(Socket,close)
import Network.Socket.ByteString(recv)
import System.IO.Error(catchIOError)
import qualified Data.ByteString.Lazy as L
import Data.Binary(encode)
import Control.Exception(handle,evaluate,throw,Exception,SomeException)
import Control.DeepSeq(force)
import BGPlib.AttoBGP
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.IORef

import qualified Network.Socket.ByteString.Lazy as L
import Control.Monad(void)
import Data.ByteString.Builder
import Data.Monoid((<>))

newtype BGPIOException = BGPIOException String deriving Show
instance Exception BGPIOException

data BGPHandle = BGPHandle Socket (IORef B.ByteString)

getBGPHandle :: Socket -> IO BGPHandle
getBGPHandle s = do
        ior <- newIORef B.empty
        return $ BGPHandle s ior

bgpRcv :: BGPHandle -> Int -> IO BGPMessage
bgpRcv (BGPHandle stream ioref) _ = do

    oldBuf <- readIORef ioref
    newBuf <- if B.null oldBuf then getBuf else return oldBuf
    result <- complete ( parse terminatingBGPParser newBuf )
    case result of
        (Done i r) -> if B.null newBuf then do writeIORef ioref undefined
                                               return BGPEndOfStream
                                       else do writeIORef ioref i
                                               evaluate r
        (Partial _) -> error "Partial has been removed already by `g`"
        (Fail _ s sx) -> error $ "parse fail in getNext" ++ show (s,sx)
        -- 'production' version in the event that parse fails actually occur
        -- (Fail _ s sx) -> do hPutStrln stderr $ "parse fail in getNext" ++ show (s,sx)
        --                     return $ BGPError $ show (s,sx)

    where

    --getBuf = B.hGetNonBlocking stream 4096
    getBuf = recv stream 4096

    complete :: Result BGPMessage -> IO (Result BGPMessage)
    complete (Partial cont) = do
        bs <- getBuf
        complete (cont bs)
    
    complete result = return result

bgpClose :: BGPHandle -> IO ()
bgpClose (BGPHandle h _ ) = close h

wireFormat :: L.ByteString -> L.ByteString
wireFormat bs = toLazyByteString $ lazyByteString (L.replicate 16 0xff) <> word16BE (fromIntegral $ 18 + L.length bs) <> lazyByteString bs 

--strictWireFormat :: B.ByteString -> B.ByteString
--strictWireFormat bs = L.toStrict $ toLazyByteString $ byteString (B.replicate 16 0xff) <> word16BE (fromIntegral $ 18 + B.length bs) <> byteString bs 

bgpSnd :: BGPHandle -> BGPMessage -> IO()
bgpSnd (BGPHandle h _ ) msg | 4079 > lengthEncodedMsg = catchIOError ( sndRawMessage encodedMsg )
                                                                     (\e -> throw $ BGPIOException (show (e :: IOError)))
                            | otherwise = error $ "encoded message too long in bgpSnd " ++ show lengthEncodedMsg
                         where encodedMsg = encode msg
                               lengthEncodedMsg = L.length encodedMsg
                               sndRawMessage bgpMsg = void $ L.send h $ wireFormat bgpMsg

bgpSndAll :: BGPHandle -> [BGPMessage] -> IO()
bgpSndAll (BGPHandle h _ ) msgs = catchIOError ( sndRawMessages $ map encode msgs )
                                            (\e -> throw $ BGPIOException (show (e :: IOError)))
    where
    sndRawMessages bgpMsgs = void $ L.send h $ L.concat $ map wireFormat bgpMsgs

{-
-- keeping this as reference for an exception handler
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
-}
