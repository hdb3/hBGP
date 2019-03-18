{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Module      : $Header$
Description : ioctl wrapping
Copyright   : (c) Maciej Piechotka
License     : BSD3

Stability   : none
Portability : portable

The module wrapps the ioctl system call for sockets.
-}
module Network.Socket.IOCtl
  (
    IOControl(..),
    ioctlsocket,
    ioctlsocket_,
    ioctlsocket',
  )
where

import Foreign
import Foreign.C
import Network.Socket
import System.IOControl

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

c_ioctl' :: IOControl req d => Socket -> req -> Ptr d -> IO ()
c_ioctl' f req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fdSocket f) (ioctlReq req) (castPtr p)

-- | Calls a ioctl reading the structure after the call
ioctlsocket :: IOControl req d
            => Socket -- ^ The socket
            -> req -- ^ The request
            -> d -- ^ The data
            -> IO d -- ^ The data after the call
ioctlsocket f req d = with d $ \p -> c_ioctl' f req p >> peek p

-- | Call a ioctl ignoring the result
ioctlsocket_ :: IOControl req d
             => Socket -- ^ The socket
             -> req -- ^ The request
             -> d -- ^ The data
             -> IO ()
ioctlsocket_ f req d = with d $ \p -> c_ioctl' f req p

-- | Call a ioctl with uninitialized data
ioctlsocket' :: IOControl req d
             => Socket -- ^ The socket
             -> req -- ^ The request
             -> IO d -- ^ The data
ioctlsocket' f req = alloca $ \p -> c_ioctl' f req p >> peek p
