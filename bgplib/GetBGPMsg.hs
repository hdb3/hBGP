{-# LANGUAGE FlexibleInstances,BangPatterns #-}
module GetBGPMsg where

import System.Timeout(timeout)
import System.IO.Error(catchIOError)
import System.IO(Handle)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad(when,unless,fail)
import Data.ByteString.Builder
import Data.Monoid((<>))

import LibCommon

lBGPMarker = L.replicate 16 0xff
_BGPMarker = B.replicate 16 0xff
data RcvStatus =   Timeout | EndOfStream | Error String deriving (Eq,Show)

newtype BGPByteString = BGPByteString (Either RcvStatus L.ByteString) deriving Eq

getRawMsg :: Handle -> Int -> IO BGPByteString
getRawMsg h t = getNextTimeout t h

getNextTimeout :: Int -> Handle -> IO BGPByteString
getNextTimeout t bsock = let t' = t * 1000000 in
             do resMaybe <- timeout t' (getNext bsock)
                maybe
                    (return (BGPByteString $ Left Timeout ))
                    return
                    resMaybe

getNext:: Handle -> IO BGPByteString
getNext h = catchIOError (getNext' h)
                         (\e -> return (BGPByteString $ Left (Error (show e)) ))
             
getNext':: Handle -> IO BGPByteString
getNext' h = do
    header <- L.hGet h 18
    if  L.length header < 18 then 
        return $ BGPByteString $ Left EndOfStream
    else do
        let (m,l) = L.splitAt 16 header
            l' = fromIntegral $ getWord16 l
        if m /= lBGPMarker then return $ BGPByteString $ Left $ Error "Bad marker in GetBGPByteString"
        else if l' < 19 || l' > 4096 then return $ BGPByteString $ Left $ Error "Bad length in GetBGPByteString"
        else do
            let bl = l' - 18
            body <- L.hGet h bl
            if  L.length body /= fromIntegral bl then return $ BGPByteString $ Left EndOfStream
            else return $ BGPByteString $ Right body
    where
    getWord16 :: L.ByteString -> Word16
    getWord16 lbs = getWord16' $ map fromIntegral (L.unpack lbs)
    getWord16' :: [Word16] -> Word16
    getWord16' (l0:l1:_) = l1 .|. unsafeShiftL l0 8

instance Binary BGPByteString where 

    put (BGPByteString (Right bs)) | msgLength > 4096 = fail $ "trying to put an overlong BGPByteString, " ++ show msgLength ++ " bytes"
                                   | otherwise = do
        putLazyByteString lBGPMarker
        putWord16be msgLength
        putLazyByteString bs where
            msgLength = fromIntegral $ L.length bs + 18

    put (BGPByteString (Left _)) = fail "trying to but an invalid BGPByteString"

    get = label "BGPByteString" $ do
        empty <- isEmpty
        when empty (fail "BGP end of stream")
        marker <- getLazyByteString 16
        unless ( marker == lBGPMarker ) (fail "BGP marker synchronisation error")
        len <- getWord16be
        unless ( len < 4097 ) (fail "BGP message length invalid")
        bs <- getLazyByteString (fromIntegral (len-18))
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

sndRawMessage :: Handle -> L.ByteString -> IO ()
sndRawMessage h bgpMsg = L.hPut h $ wireFormat bgpMsg

sndRawMessages :: Handle -> [L.ByteString] -> IO ()
sndRawMessages h bgpMsgs = L.hPut h $ L.concat $ map wireFormat bgpMsgs
