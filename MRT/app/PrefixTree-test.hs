{-# LANGUAGE OverloadedStrings #-}

module Main where
import PrefixTree
import qualified Overlap


main' = do
    putStrLn "PrefixTree-test"
    let td = [("192.168.0.0/24",())
             ,("192.168.128.0/24",())
             ,("192.168.127.0/24",())
             ,("192.168.64.0/18",())
             ,("192.168.0.0/18",())
             ]
    print $ PrefixTree.fromList td
    print $ PrefixTree.fromListLS td

main = do
    putStrLn "Tree-test"
    let td = [("192.168.0.0/24",())
             ,("192.168.128.0/24",())
             ,("192.168.127.0/24",())
             ,("192.168.64.0/18",())
             ,("192.168.0.0/18",())
             ]
    putStrLn $ "toList . fromList   " ++ show (Overlap.toList $ Overlap.fromList td)
    putStrLn $ "toListLS . fromList " ++ show (Overlap.toListLS $ Overlap.fromList td)
    putStrLn $ "toList . fromListLS " ++ show (Overlap.toList $ Overlap.fromListLS td)
