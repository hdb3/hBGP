module Main where

import GroupRIBReport

main :: IO ()
main = do
   putStrLn "Operations-test"
   putStrLn $ groupRIBReport [[1,2] , [1,2,3] , [1,2,3,4] , [5] ]
   putStrLn "done"

