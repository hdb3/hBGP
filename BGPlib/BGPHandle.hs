module BGPlib.BGPHandle where

import BGPlib.AttoBGP
import BGPlib.BGPMessage (BGPMessage (..))
import Control.Exception (Exception, evaluate, throw)
import Control.Logger.Simple
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Socket (Socket, socketToHandle)
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import qualified System.Timeout

newtype BGPIOException = BGPIOException String deriving (Show)

instance Exception BGPIOException

data BGPHandle = BGPHandle Handle (IORef B.ByteString)

byteStreamToBGPMessages :: B.ByteString -> [BGPMessage]
byteStreamToBGPMessages bs = let Right msgs = parseOnly bgpParser bs in msgs

getBGPHandle :: Socket -> IO BGPHandle
getBGPHandle sock = do
  ior <- newIORef B.empty
  handle <- socketToHandle sock System.IO.ReadWriteMode
  return $ BGPHandle handle ior

bgpRcv :: BGPHandle -> Int -> IO BGPMessage
bgpRcv bgpHandle t = fromMaybe BGPTimeout <$> System.Timeout.timeout (1000000 * t) (bgpRcv' bgpHandle)

bgpRcv' :: BGPHandle -> IO BGPMessage
bgpRcv' (BGPHandle stream ioref) = do
  oldBuf <- readIORef ioref
  newBuf <- if B.null oldBuf then getBuf else return oldBuf
  result <- complete (parse terminatingBGPParser newBuf)
  case result of
    (Done i r) ->
      if B.null newBuf
        then do
          writeIORef ioref undefined
          return BGPEndOfStream
        else do
          writeIORef ioref i
          evaluate r
    (Partial _) -> error "Partial has been removed already by `g`"
    -- agressive debug version:
    -- (Fail _ s sx) -> error $ "parse fail in getNext" ++ show (s, sx)

    -- 'production' version in the event that parse fails actually occur
    (Fail _ s sx) -> do
      hPutStrLn stderr $ "parse fail in getNext" ++ show (s, sx)
      return $ BGPError $ show (s, sx)
  where
    getBuf = B.hGetSome stream 4096
    complete :: Result BGPMessage -> IO (Result BGPMessage)
    complete (Partial cont) = do
      bs <- getBuf
      complete (cont bs)
    complete result = return result

bgpClose :: BGPHandle -> IO ()
bgpClose (BGPHandle h _) = hClose h

bgpSendHandle :: BGPHandle -> B.ByteString -> IO ()
bgpSendHandle (BGPHandle h _) bs =
  catchIOError
    (B.hPut h bs)
    -- (\e -> throw $ BGPIOException (show (e :: IOError)))
    -- (\e -> logDebug $ T.pack $ (show (e :: IOError)))
    ( \e -> do
        logDebug $ T.pack $ (show (e :: IOError))
        throw $ BGPIOException (show (e :: IOError))
    )
