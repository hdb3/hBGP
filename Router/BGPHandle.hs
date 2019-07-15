module Router.BGPHandle(BGPIOException(..),BGPHandle,getBGPHandle,bgpSnd,bgpRcv,bgpSndAll,bgpClose) where

import BGPlib.BGPparse(BGPMessage(..),encodeBGPMessage)
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

bgpSnd :: BGPHandle -> BGPMessage -> IO()
bgpSnd h m = bgpSndAll h [m]

bgpSndAll :: BGPHandle -> [BGPMessage] -> IO()
bgpSndAll (BGPHandle h _ ) msgs = catchIOError ( sndRawMessages $ map encode msgs )
                                            (\e -> throw $ BGPIOException (show (e :: IOError)))
    where
    sndRawMessages bgpMsgs = do let msg = L.concat $ map wireFormat bgpMsgs
                                -- diagostic for sent message
                                -- print (length bgpMsgs , L.length msg)
                                void $ L.sendAll h msg
