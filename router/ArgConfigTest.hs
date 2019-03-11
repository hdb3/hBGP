
module Main where

import ArgConfig

main = do
    d <-  buildDictionary
    print d
    let
        p1 = getInt d "p1"
        p2 = getInt d "p2"
    print (p1,p2)
