module ArgConfig where

import System.Environment
import Text.Read(readMaybe)

-- read test configuration parameters from the command line or a file
-- syntax is equivalent - 'key=value'
-- in file format fields are delineated by newline
-- in command line format fields are delineated by non-quoted spaces (so that args finds them separately)
-- if the command line argument 'file=' is found then....

-- first build just the command line version

-- design

-- build a dictionary of key/value pairs, where the delimiter is '='
-- for each parameter the type is used to determine the parameret getter, via the Read class
type Dictionary = [(String,String)]
buildDictionary :: IO Dictionary
buildDictionary = do
    args <- getArgs
    return $ map f args
    where
        f s = let (s1,s2) = break ('=' ==) s in (s1,dropWhile ('=' ==) s2)

getInt :: Dictionary -> String -> Maybe Int
--getInt d k = maybe (Just Nothing) ( Just . readMaybe) ( lookup k d )
getInt d k = case ( lookup k d ) of
                  Nothing -> Nothing
                  Just s -> readMaybe s

