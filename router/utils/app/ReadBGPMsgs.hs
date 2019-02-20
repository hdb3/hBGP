module Main where

import qualified Data.List
import BGPlib
import BGPReader(readMsgs)

main = do
    msgs <-readMsgs
    putStrLn $ "got " ++ show (length msgs) ++ " messages"
    let (u,nonU) = Data.List.partition isUpdate msgs
    putStrLn $ "got " ++ show (length u) ++ " updates"
    putStrLn $ "got " ++ show (length nonU) ++ " non-updates"
