module Main where

-- Lazy version import
import Control.Monad (mapM_)
import qualified Data.ByteString.Lazy as BS
import MRTformat

main :: IO ()
main = do
  putStrLn "MRTlib-test"
  f <- BS.getContents
  let mrtMsgs = mrtParse f
  mapM_ print mrtMsgs
  putStrLn "done"
