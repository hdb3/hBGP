{-# LANGUAGE FlexibleInstances #-}

module BGPlib.GetBGPMsg where

import BGPlib.BGPMessage
import BGPlib.LibCommon
import Control.Monad (unless, void, when)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified System.IO as SI
import System.IO.Error (catchIOError)
import System.Timeout (timeout)

hGetRawMsg :: SI.Handle -> Int -> IO BGPByteString
hGetRawMsg = getRawMsg

lBGPMarker = L.replicate 16 0xff

_BGPMarker = B.replicate 16 0xff

getRawMsg :: SI.Handle -> Int -> IO BGPByteString
getRawMsg h t = getNextTimeout t h

getNextTimeout :: Int -> SI.Handle -> IO BGPByteString
getNextTimeout t bsock =
  let t' = t * 1000000
    in do
        resMaybe <- timeout t' (getNext bsock)
        maybe
          (return (BGPByteString $ Left Timeout))
          return
          resMaybe

getNext :: SI.Handle -> IO BGPByteString
getNext h =
  catchIOError
    (getNext' h)
    (\e -> return (BGPByteString $ Left (Error (show e))))

getNext' :: SI.Handle -> IO BGPByteString
getNext' h = do
  header <- L.hGet h 18
  if L.length header < 18
    then return $ BGPByteString $ Left EndOfStream
    else do
      let (m, l) = L.splitAt 16 header
          l' = fromIntegral $ getWord16 l
      if m /= lBGPMarker
        then return $ BGPByteString $ Left $ Error "Bad marker in GetBGPByteString"
        else
          if l' < 19 || l' > 4096
            then return $ BGPByteString $ Left $ Error "Bad length in GetBGPByteString"
            else do
              let bl = l' - 18
              body <- L.hGet h bl
              if L.length body /= fromIntegral bl
                then return $ BGPByteString $ Left EndOfStream
                else return $ BGPByteString $ Right body
    where
      getWord16 :: L.ByteString -> Word16
      getWord16 lbs = getWord16' $ map fromIntegral (L.unpack lbs)
      getWord16' :: [Word16] -> Word16
      getWord16' (l0 : l1 : _) = l1 .|. unsafeShiftL l0 8

instance Binary BGPByteString where
  put (BGPByteString (Right bs))
    | msgLength > 4096 = error $ "trying to put an overlong BGPByteString, " ++ show msgLength ++ " bytes"
    | otherwise = do
      putLazyByteString lBGPMarker
      putWord16be msgLength
      putLazyByteString bs
    where
      msgLength = fromIntegral $ L.length bs + 18
  put (BGPByteString (Left _)) = error "trying to but an invalid BGPByteString"

  get = label "BGPByteString" $ do
    empty <- isEmpty
    when empty (error "BGP end of stream")
    marker <- getLazyByteString 16
    unless (marker == lBGPMarker) (error "BGP marker synchronisation error")
    len <- getWord16be
    unless (len < 4097) (error "BGP message length invalid")
    bs <- getLazyByteString (fromIntegral (len -18))
    return (BGPByteString $ Right bs)

getBGPByteString :: Get BGPByteString
getBGPByteString = get

getBGPByteStrings :: Get [BGPByteString]
getBGPByteStrings = get

instance {-# OVERLAPPING #-} Binary [BGPByteString] where
  put = putn
  get = getn

-- simple replacement for Binary instance of BGPByteString
wireFormat :: L.ByteString -> L.ByteString
wireFormat bs = toLazyByteString $ lazyByteString lBGPMarker <> word16BE (fromIntegral $ 18 + L.length bs) <> lazyByteString bs

hSndRawMessage :: SI.Handle -> L.ByteString -> IO ()
hSndRawMessage h bgpMsg = void $ L.hPut h $ wireFormat bgpMsg

hSndRawMessages :: SI.Handle -> [L.ByteString] -> IO ()
hSndRawMessages h bgpMsgs = void $ L.hPut h $ L.concat $ map wireFormat bgpMsgs
