{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prefixes
import Overlap

fl :: [Prefix] -> PrefixTree
fl = fromList

main=do
    main0
    main1
    main2

main0 = let p px = putStrLn $ show px ++ " " ++ show (insertPrefix px Empty) in do
    -- p "0.0.0.0/1"
    -- p "128.0.0.0/1"
    -- p "192.0.0.0/2"
    p "0.0.0.0/8"
    p "1.0.0.0/8"
    p "2.0.0.0/8"
    p "3.0.0.0/8"
    p "4.0.0.0/8"
    p "5.0.0.0/8"

main1 = do
    print $ map fl n
    print $ map size $ map fl n
    print $ map height $ map fl n

main2 = do
    let f x = (id x,"  -  ",longest x)
    -- let f x = (id x,size x,height x, reduce x, size $ reduce x, height $ reduce x)
        trees = map ( f . fl) z
    mapM print trees

z = [ ["128.0.0.0/1","128.0.0.0/2"] , ["128.0.0.0/1","128.0.0.0/5"] , ["128.0.0.0/1","128.0.0.0/2","128.0.0.0/5"] ]
z' = [ ["128.0.0.0/1","128.0.0.0/2"] , ["128.0.0.0/1","128.0.0.0/3"] , ["128.0.0.0/1","128.0.0.0/4"] , ["128.0.0.0/1","128.0.0.0/5"] ]
n = [
      ["0.0.0.0/0","0.0.0.0/1","0.0.0.0/2","0.0.0.0/3","0.0.0.0/4","0.0.0.0/5"]
   ,  ["0.0.0.0/8","1.0.0.0/8","2.0.0.0/8","3.0.0.0/8","4.0.0.0/8","5.0.0.0/8"]
   ,  reverse ["0.0.0.0/8","1.0.0.0/8","2.0.0.0/8","3.0.0.0/8","4.0.0.0/8","5.0.0.0/8"]
--     ["0.0.0.0/0"]
--     , ["0.0.0.0/1"]
--     , ["0.0.0.0/2"]
--     , ["128.0.0.0/1"]
--     , ["128.0.0.0/2"]
--     , ["128.0.0.0/1","0.0.0.0/1"]
--     , ["0.0.0.0/1","128.0.0.0/1"]
    ]

l = [
       ["0.0.0.0/0"]
     , ["192.168.0.0/24"]
     , ["192.168.0.0/24","192.168.0.0/24"]
     , ["192.168.1.0/24","192.168.0.0/24"]
     , ["192.168.1.0/24","192.168.1.128/25"]
    ]

m = [ ["192.168.0.0/24"]
    , ["192.168.0.0/24","192.168.1.0/24","192.168.2.0/24"]
    , ["192.168.1.0/24","192.168.1.0/25","192.168.1.0/26"]
    , ["192.168.0.0/24","192.168.1.0/24","192.168.2.0/24","192.168.2.0/25","192.168.2.0/26"]
    ]