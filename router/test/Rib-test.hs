{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary
import Data.ByteString.Lazy(toStrict)

import Prefixes
import PathAttributes
import Rib
import BGPData
import BGPDataTestData
import Update


prefix1 = fromAddrRange "172.16.0.77/12"
prefix2 = fromAddrRange "192.168.122.1/24"
prefix3 = fromAddrRange "10.0.0.0/8"
prefix4 = fromAddrRange "10.0.4.0/24"
prefix5 = fromAddrRange "10.0.5.0/24"
prefix6 = fromAddrRange "10.0.6.0/24"
prefix7 = fromAddrRange "10.0.7.0/24"
prefix8 = fromAddrRange "10.0.8.0/24"
prefix9 = fromAddrRange "10.0.9.0/24"
prefix0 = fromAddrRange "10.0.11.0/24"

prefixes1 = [ "10.0.3.0/24", "10.0.4.0/24" ]
prefixes2 = [ "10.0.5.0/24" , "10.0.6.0/24" ]
path01 = ASPath4 [ ASSequence [6,5,4] ]
path02 = ASPath4 [ ASSequence [6,5,4,3,2,1] ]
attrs1 = [PathAttributeOrigin 2, PathAttributeASPath path01, PathAttributeNextHop "192.168.0.1"]
attrs2 = [PathAttributeOrigin 1, PathAttributeASPath path02, PathAttributeNextHop "192.168.0.2"]
pathA = (attrs1, toStrict $ encode attrs1)
pathB = (attrs2, toStrict $ encode attrs2)

{-
pr rib = do locRib <- getLocRib rib
            adjRib <- getAdjRib rib
            putStrLn "locRib"
            print locRib
            putStrLn "adjRib"
            print adjRib
            putStrLn ""
-}
pr = printRib

diff rib1 rib2 = do
    r1 <- show rib1
    r2 <- show rib2
    return $ r1 == r2

main = do
    putStrLn "*****************\nRIB delPeer test\n*****************"
    rib <- newRib local
    pr rib
    putStrLn "\nadd internalPeer + externalPeer"
    addPeer rib internalPeer
    addPeer rib externalPeer
    pr rib

    putStrLn "\nupdate internalPeer prefixes1"
    --print("XXZZ",( makeUpdateSimple attrs1 prefixes1 [] ))
    ribUpdater rib internalPeer ( makeUpdateSimple attrs1 prefixes1 [] )
    -- ribUpdater rib internalPeer ( makeUpdateSimple attrs2 prefixes2 [] )
    pr rib
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    -- pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )

    putStrLn "\ndel internalPeer"
    delPeer rib internalPeer
    pr rib
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    pr rib

main' = do
    putStrLn "*****************\nRIB withdraw test\n*****************"
    rib <- newRib local
    pr rib
    addPeer rib internalPeer
    addPeer rib externalPeer
    pr rib

    putStrLn "\nupdate internalPeer prefixes1"
    ribUpdater rib internalPeer ( makeUpdateSimple attrs1 prefixes1 [] )
    -- ribUpdater rib internalPeer ( makeUpdateSimple attrs2 prefixes2 [] )
    pr rib
    pullAllUpdates internalPeer rib >>= ( \v -> putStrLn $ "internalPeer AdjRibOut" ++ show v )
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )

    putStrLn "\nwithdraw internalPeer prefixes1"
    ribUpdater rib internalPeer ( makeUpdateSimple [] [] prefixes1 )
    pr rib
    pullAllUpdates internalPeer rib >>= ( \v -> putStrLn $ "internalPeer AdjRibOut" ++ show v )
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    pullAllUpdates externalPeer rib >>= ( \v -> putStrLn $ "externalPeer AdjRibOut" ++ show v )
    delPeer rib internalPeer
    delPeer rib externalPeer
    pr rib
