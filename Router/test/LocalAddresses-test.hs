{-# LANGUAGE OverloadedStrings #-}


module Main where

-- package network-info

import Control.Monad (liftM)
import Data.IP
import LocalAddresses
import Network.Info

main = do
  validAddresses <- getValidAddresses
  putStrLn $ "valid local addresses: " ++ show validAddresses

  validAddress <- getValidAddress
  putStrLn $ "valid local address: " ++ show validAddress

  validPublicAddress <- getPublicAddress
  putStrLn $ "valid public address: " ++ show validPublicAddress

  bestAddress <- getBestAddress
  putStrLn $ "best address: " ++ show bestAddress

  allInterfaces <- getAllInterfaces
  putStrLn $ "\n*************************\nallInterfaces\n" ++ allInterfaces
