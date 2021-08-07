module Router.ArgConfig where

import Data.Maybe (fromMaybe)
import System.Environment
import Text.Read (readMaybe)

-- read test configuration parameters from the command line or a file
-- syntax is equivalent - 'key=value'
-- in file format fields are delineated by newline
-- in command line format fields are delineated by non-quoted spaces (so that args finds them separately)
-- if the command line argument 'file=' is found then....

-- first build just the command line version

-- design

-- build a dictionary of key/value pairs, where the delimiter is '='
-- for each parameter the type is used to determine the parameret getter, via the Read class

type Dictionary = [(String, String)]

buildDictionary :: IO Dictionary
buildDictionary = do
  args <- getArgs
  return $ map f args
  where
    f s = let (s1, s2) = break ('=' ==) s in (s1, dropWhile ('=' ==) s2)

getVal :: Read p => Dictionary -> p -> String -> p
getVal d z k = case lookup k d of
  Nothing -> z
  Just p -> fromMaybe z (readMaybe p)

getArgVal :: Read p => p -> String -> IO p
getArgVal z k = do
  d <- buildDictionary
  return $ getVal d z k
