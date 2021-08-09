module Router.Console where

import BGPRib.BGPRib
import BGPlib.BGPlib
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Trans.Class
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Router.Global
import System.Console.Haskeline
import System.IO (hFlush, stdout)
import Text.Read hiding (lift)

startConsole :: Global -> IO ()
startConsole global = do
  let push v = lift $ ribPush (rib global) (localPeer $ gd global) v
      exit = lift $ putMVar (exitFlag global) ()
      consoleThread = runInputT defaultSettings $ console $ CState push exit global [] [] "0.0.0.0" 100 (routerName global ++ " # ")
  void $ forkIO consoleThread

data CState = CState
  { push :: ParsedUpdate -> InputT IO (),
    exit :: InputT IO (),
    csGlobal :: Global,
    csPath :: [Word32],
    csNlri :: [AddrRange IPv4],
    csNextHop :: IPv4,
    csLocalPref :: Word32,
    csPrompt :: String
  }

query :: CState -> [String] -> InputT IO ()
query cs s = do
  prefixTable <- lift $ getLocRib (rib $ csGlobal cs)
  if null $ head s
    then lift $ print prefixTable
    else
      maybe
        (outputStrLn "couldn't parse prefix")
        (\prefix -> outputStrLn $ "[" ++ show prefix ++ "] " ++ showRibAt prefixTable (fromAddrRange prefix))
        (parsePrefix $ head s)
  console cs

updateFrom CState {..} = push $ iBGPUpdate csPath csNlri csNextHop csLocalPref

withdrawFrom CState {..} = push (bgpWithdraw csNlri)

console :: CState -> InputT IO ()
console cstate@CState {..} = do
  inputM <- getInputLine csPrompt
  let input = fromMaybe "" inputM
      (command : px) = words input ++ repeat ""
  case map toLower command of
    "" -> console cstate
    "q" -> query cstate px
    "s" -> do
      outputStrLn $ "Route: " ++ show csPath ++ " : " ++ show csNlri ++ " nh=" ++ show csNextHop ++ " lp=" ++ show csLocalPref
      console cstate
    "u" -> do
      outputStrLn $ "Sending update: " ++ show csPath ++ " : " ++ show csNlri ++ " : " ++ show csNextHop
      updateFrom cstate
      console cstate
    "w" -> do
      outputStrLn $ "Sending withdraw: " ++ show csNlri
      withdrawFrom cstate
      console cstate
    "h" ->
      maybe
        (outputStrLn "couldn't parse nexthop")
        ( \p -> do
            outputStrLn $ "Nexthop: " ++ show p
            console cstate {csNextHop = p}
        )
        (parseAddress $ px !! 0)
    "p" ->
      maybe
        (outputStrLn "couldn't parse a path")
        ( \p -> do
            outputStrLn $ "Path: " ++ show p
            console cstate {csPath = p}
        )
        (parsePath $ px !! 0)
    "n" ->
      maybe
        (outputStrLn "couldn't parse prefixes")
        ( \p -> do
            outputStrLn $ "nlri: " ++ show p
            console cstate {csNlri = p}
        )
        (parsePrefixes $ px !! 0)
    "l" ->
      maybe
        (outputStrLn "couldn't parse local preference")
        ( \p -> do
            outputStrLn $ "LocPref: " ++ show p
            console cstate {csLocalPref = p}
        )
        (parseWord32 $ px !! 0)
    "x" -> outputStrLn "goodbye" >> exit
    _ -> outputStrLn "couldn't parse a command - try Show / Path / nH / Nlri / Local preference / Update / Withdraw / Quit"

  console cstate

prompt = putStr ">>> " >> hFlush stdout

parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)

parsePrefixes s = readMaybe s :: Maybe [AddrRange IPv4]

parseAddress s = readMaybe s :: Maybe IPv4

parsePath s = readMaybe s :: Maybe [Word32]

parseWord32 s = readMaybe s :: Maybe Word32
