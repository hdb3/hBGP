{-# LANGUAGE MultiParamTypeClasses #-}
module Poll where

import Data.Word
import Data.Time.Clock
import Control.Concurrent
import qualified Network.Socket.IOCtl as NSI
import qualified Network.Socket as NS
import qualified System.Posix.IOCtl as SPI
import qualified System.Posix as SP

_1mS = 10^3
_10mS = 10^4
tPollDelay = _10mS

poll :: ( Show a, Eq a) => IO a -> a -> IO ()
poll action terminationValue = poll' ( terminationValue == ) tPollDelay action

poll' :: ( Show a, Eq a)=> (a-> Bool) -> Int -> IO a -> IO ()
poll' cmp tDelay action = do
    ts0 <- getCurrentTime
    iv <- action
    go ts0 iv where
    go t0 v = do
        --t <- getCurrentTime
        --print (t,v)
        if cmp v then
            --putStrLn $ "Poll: elapsed time = " ++ show ( diffUTCTime t t0 )
            return ()
        else do
            v' <- retry v
            go t0 v'

    retry x = do
        x' <- action
        if x' == x
        then do threadDelay tDelay
                retry x
        else return x'

data SIOCOUTQ = SIOCOUTQ
instance NSI.IOControl SIOCOUTQ Word32 where
    ioctlReq _ = 0x5411

-- NSI.IOControl and SPI.IOControl are equivalent
--instance SPI.IOControl SIOCOUTQ Word32 where
--    ioctlReq _ = 0x5411

fdUnsent :: SP.Fd -> IO Word32
fdUnsent fd = SPI.ioctl' fd SIOCOUTQ

unsent :: NS.Socket -> IO Word32
unsent sock = NSI.ioctlsocket' sock SIOCOUTQ

waitOnQEmpty :: NS.Socket -> IO ()
waitOnQEmpty sock = poll ( unsent sock ) 0

fdWaitOnQEmpty :: SP.Fd -> IO ()
fdWaitOnQEmpty fd = poll ( fdUnsent fd ) 0
