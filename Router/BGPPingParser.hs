{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Router.BGPPingParser where

import BGPlib.AttoBGP (bgpParser, bgpParser1)
import BGPlib.BGPlib (toHex, BGPMessage(..))
-- import Control.Concurrent
-- import Control.Monad (forever)
-- import Control.Monad.Extra (ifM)
import Data.Attoparsec.ByteString (IResult (..), Parser, feed, maybeResult, parse, parseWith)
import qualified Data.ByteString as ByteString
-- import Data.ByteString.Builder
-- import Data.ByteString.Builder.Extra (byteStringCopy)
-- import Data.IORef
-- import Data.IP
-- import Data.List (partition)
-- import Data.Maybe (fromJust)
-- import Foreign.C.Error
-- import GHC.IO.Exception (ioe_description)
-- import qualified Network.Socket as NS
-- import System.Environment (getArgs)
-- import System.Exit (die, exitSuccess)
import System.IO

-- import System.IO.Error
-- import Text.Read (readMaybe)

main = example

myGetNext = getNext (ByteString.hGetSome stdin 65536) parser

type BS = ByteString.ByteString

-- type Val = BGPMessage
-- action = display
-- parser = bgpParser1

type Val = [BGPMessage]
action = mapM_ display
parser = bgpParser

display m = putChar $ case m of (BGPUpdate {..}) -> if | ByteString.null attributes ->  'E'  
                                                       | null nlri -> 'A' 
                                                       | null withdrawn -> 'u'                                           
                                                       | otherwise -> 'U'
                                (BGPNotify {}) -> 'N'
                                BGPKeepalive -> 'K'
                                BGPTimeout -> 'T'
                                BGPEndOfStream -> 'X'
                                (BGPError _ ) -> '?'

getNext :: IO BS -> Parser Val -> BS -> IO (Maybe (BS, Val))
getNext getter myParser bs = go (parse myParser bs)
  where
    go result = case result of
      (Done i r) -> return $ Just (i, r)
      f@(Fail _ _ _) -> failHandler f
      f@(Partial _) -> do
        bs <- getter
        if ByteString.null bs then return Nothing else go (feed f bs)

-- An example of this strategy:

getterNoisy = do
  putStrLn "enter getter for 64K"
  bs <- ByteString.hGetSome stdin _64K
  putStrLn $ "exit getter with " ++ show (ByteString.length bs) ++ " bytes"
  return bs

_64K = 65536 :: Int
_1M = 1024 * 1024 :: Int
_16M = 16 * _1M
getterQuiet = ByteString.hGetSome stdin _16M

getter = getterQuiet

example =
  let go bs = do
        next <- getNext getter parser bs
        maybe
          (return ())
          (\(bs', val) -> action val >> go bs')
          next
   in go ByteString.empty

{-

-- This completes a circle of reasoning - the next step here is to recognize the immutable pattern erected around the ‘action val’ expression and build a control flow abstraction, which returns to the first proposed approach!  Nonetheless, a complete instance providing getNext as a simple IO action has value, and requires an IORef bound to an IO function to produce a handle:

getNextIORef getter parser ioref = do
  bs <- readIORef ioref
  let (bs’,val) = getNext getter parser bs
  writeIORef ioref bs’
  return val

-- this can be initialised into a simple genrator as follows:

getGenerator getter parser = getNextIORef getter parser <$> (newIORef empty)

-}
-- fail handler generic:

failHandler (Fail _ context description) = do
  putStrLn $ "parse failed: " ++ description
  mapM_ putStrLn context
  error "error value from parse fail"
