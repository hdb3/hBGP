module Router.BGPConnect
  ( Socket,
    close,
    gracefulClose,
    socketToHandle,
    clientConnect,
    getServerSession,
    openServerSocket,
    getSockAddresses,
  )
where

import Control.Concurrent
import Data.IP
import Data.Word
import Foreign.C.Error
import Foreign.C.Types (CInt)
import GHC.IO.Exception (ioe_description)
import Network.Socket (Socket, close, gracefulClose, socketToHandle)
import qualified Network.Socket as NS
import System.Exit (die)
import System.IO
import System.IO.Error

retryOnBusy = False

clientConnect :: Word16 -> IPv4 -> IPv4 -> IO Socket
clientConnect port peer local = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.NoDelay 1
  bind sock local
  connect' sock (fromIntegral port) peer
  return sock
  where
    connect' :: Socket -> NS.PortNumber -> IPv4 -> IO ()
    connect' sock port peer =
      catchIOError
        (NS.connect sock (NS.SockAddrInet port (toHostAddress peer)))
        ( \e -> do
            Errno errno <- getErrno
            if
                | elem errno [2, 103, 115] -> do
                    putStrLn $ ioe_description e ++ "(" ++ show errno ++ ") retrying in 10 seconds"
                    threadDelay 10000000 -- 10 seconds
                    connect' sock port peer
                | otherwise -> unknownSocketErrorHandler e errno
        )
    bind :: Socket -> IPv4 -> IO ()
    bind sock addr =
      catchIOError
        (NS.bind sock (NS.SockAddrInet 0 $ toHostAddress addr))
        ( \e -> do
            Errno errno <- getErrno
            if
                | errno == 99 -> die "address error binding port - host configuration mismatch?"
                | otherwise -> unknownSocketErrorHandler e errno
        )

getServerSession :: Socket -> IO Socket
getServerSession listeningSocket = fst <$> NS.accept listeningSocket

openServerSocket :: Word16 -> IPv4 -> IO Socket
openServerSocket port address = do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.setSocketOption sock NS.NoDelay 1
  serverBind sock (fromIntegral port) address
  NS.listen sock 100
  return sock
  where
    serverBind :: Socket -> NS.PortNumber -> IPv4 -> IO ()
    serverBind sock port ip =
      catchIOError
        (NS.bind sock (NS.SockAddrInet port (toHostAddress ip)))
        ( \e -> do
            Errno errno <- getErrno
            if
                | errno == 13 ->
                    die
                      "permission error binding port (are you su?) (or try: sysctl net.ipv4.ip_unprivileged_port_start=179?)"
                | errno == 99 ->
                    die "address error binding port - host configuration mismatch?"
                | errno == 98 ->
                    if retryOnBusy
                      then do
                        hPutStrLn stderr "waiting to bind port"
                        threadDelay 10000000 -- 10 seconds
                        serverBind sock port address
                      else die "port already in use"
                | otherwise -> unknownSocketErrorHandler e errno
        )

unknownSocketErrorHandler :: IOError -> CInt -> IO ()
unknownSocketErrorHandler e errno =
  die $
    unlines
      [ "*** UNKNOWN exception, please record this",
        "errno " ++ show errno,
        "description " ++ ioe_description e
      ]

getSockAddresses :: Socket -> IO ((Word16, IPv4), (Word16, IPv4))
getSockAddresses sock = do
  let render (NS.SockAddrInet port address) = (fromIntegral port, fromHostAddress address)
  peer <- NS.getPeerName sock
  local <- NS.getSocketName sock
  return (render local, render peer)
