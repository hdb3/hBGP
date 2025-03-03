module Main where

import BGPReader (readMsgs)
import BGPlib
import qualified Data.List

main = do
  msgs <- readMsgs
  putStrLn $ "got " ++ show (length msgs) ++ " messages"
  let (u, nonU) = Data.List.partition isUpdate msgs
  putStrLn $ "got " ++ show (length u) ++ " updates"
  putStrLn $ "got " ++ show (length nonU) ++ " non-updates"
