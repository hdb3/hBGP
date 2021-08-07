module Router.Main where

import ArgConfig

data AADT = Hot | Cold | Warmish Int deriving (Show, Read)

main = do
  d <- buildDictionary
  let p1 = getVal d 100 "p1"
      p2 = getVal d 200 "p2"
  print (p1, p2)
  a <- getArgVal 99 "a"
  b <- getArgVal 999 "b"
  print (a, b)
  aadt <- getArgVal Hot "temp"
  print aadt
