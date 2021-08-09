module Main where

import BGPRib.BGPReader (readMsgs)
import BGPRib.BGPRib (myHash)
import BGPlib.AttoBGP
import BGPlib.BGPlib (BGPMessage (..), Prefix, wireFormat)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Stopwatch
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  bs <- if null args then B.getContents else B.readFile (head args)
  if 2 > length args
    then simple bs
    else complex (tail args) bs

simple :: B.ByteString -> IO ()
simple bs = do
  timeIO "bgpParse" (bgpParse bs)

complex args bs = do
  rawMsgs <- wireParse bs
  let Right rawMsgs = parseOnly wireParser bs
      rawMsgCount = length rawMsgs
      n = if not (null args) then read (head args) :: Int else rawMsgCount
      rebuiltBS = L.toStrict $ L.concat $ map (wireFormat . L.fromStrict) $ take n rawMsgs

  B.writeFile "tmp.raw" rebuiltBS
  bgpParse rebuiltBS

wireParse :: B.ByteString -> IO [B.ByteString]
wireParse bs = do
  either
    (\s -> die $ "BGP wire format parse failed: " ++ s)
    ( \msgs -> do
        putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes"
        return msgs
    )
    (parseOnly wireParser bs)

bgpParse :: B.ByteString -> IO ()
bgpParse bs = do
  either
    (\s -> die $ "BGP message parse failed: " ++ s)
    ( \msgs -> do
        putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes"
        analyseMessageTypes msgs
        analysePrefixes msgs
    )
    (parseOnly bgpParser bs)
  where
    analyseMessageTypes msgs = do
      let groups = foldl f ([], [], [], []) msgs
          f (o, u, k, n) open@BGPOpen {..} = (open : o, u, k, n)
          f (o, u, k, n) update@BGPUpdate {..} = (o, update : u, k, n)
          f (o, u, k, n) keepalive@BGPKeepalive = (o, u, keepalive : k, n)
          f (o, u, k, n) notification@BGPNotify {..} = (o, u, k, notification : n)
          count (o, u, k, n) = (length o, length u, length k, length n)
      putStrLn $ "BGP message type counts (o,u,k,n) = " ++ show (count groups)

    analysePrefixes msgs = do
      let prefixes = foldl f (0, 0) msgs
          f (u, w) BGPUpdate {..} = (u + length nlri, w + length withdrawn)
          f (u, w) _ = (u, w)
      putStrLn $ "prefix count (update,withdrawn) = " ++ show prefixes
