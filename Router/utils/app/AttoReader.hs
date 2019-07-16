{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Hashable
import Data.Binary
import qualified Data.ByteString.Lazy
import BGPlib.BGPlib(Prefix,BGPMessage(..))
import BGPRib.BGPRib(myHash)
import BGPRib.BGPReader(readMsgs)
--import ASPathUtils
--import BGPutils

import BGPlib.AttoBGP
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
main = do
    bs <- B.getContents
    either (\s -> putStrLn $ "parse failed: " ++ s)
           (\msgs -> do putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes"
                        analyseMessageTypes msgs
                        --analysePrefixes msgs
           )
           ( parseOnly bgpParser bs )


analyseMessageTypes msgs = do
    let groups = foldl f ([],[],[],[]) msgs
        f (o,u,k,n) open@BGPOpen{..}           = (open:o,u,k,n)
        f (o,u,k,n) update@BGPUpdate{..}       = (o,update:u,k,n)
        f (o,u,k,n) keepalive@BGPKeepalive     = (o,u,keepalive:k,n)
        f (o,u,k,n) notification@BGPNotify{..} = (o,u,k,notification:n)
        count (o,u,k,n) = (length o,length u,length k,length n)
    putStrLn $ "count (o,u,k,n) = " ++ show (count groups)

analysePrefixes msgs = do
    let prefixes = foldl f (0,0) msgs
        f (u,w) BGPUpdate{..} = ( u + length nlri , w + length withdrawn )
        f (u,w) _ = (u,w)
    putStrLn $ "count (update,withdrawn) = " ++ show prefixes
