module Main where

import BGPlib
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as L

main = do
  c <- L.getContents
  let m = decode c :: BGPMessage
  print (display m, L.length (withdrawn m), L.length (attributes m), L.length (nlri m))
  putStrLn $ "attributes = " ++ show (decode (attributes m) :: [PathAttribute])
  putStrLn $ "withdrawn = " ++ show (decode (withdrawn m) :: [Prefix])
  putStrLn $ "nlri = " ++ show (decode (nlri m) :: [Prefix])
