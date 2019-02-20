{-#LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import qualified Data.IP
import Session
import Data.Char(toUpper)

-- echo :: App
echo name (sock,peerAddress) = do
    putStrLn $ "echo starting with name " ++ name
    -- peerAddress  <- getPeerName sock
    localAddress <- getSocketName sock
    putStrLn $ "echo - local address: " ++ show localAddress ++ " peer address: " ++ show peerAddress
    send sock "hello friend\n"
    reply <- recv sock 4096
    putStrLn $ "my friend said: \"" ++ reply ++ "\""
    send sock $ "you said " ++ (map toUpper reply)
    send sock "Goodbye!"
    return ()


main = do 
    let peers = 
            [ "192.168.122.236"
            , "192.168.122.60"
            , "192.168.122.178"
            ]
    session 5000 (echo "default app") peers
