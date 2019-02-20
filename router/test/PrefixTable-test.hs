{-#LANGUAGE OverloadedStrings #-}
module Main where

{- A single prefix table holds everything about a prefix we could care about
 - but, this is merely the prefix itself, and the associated path
 -
 - for IPv4 the prefix including length fits in a 64 bit word, so can be the actual key
 - though it might be that a simple scarmable operation would make a better key for a tree...
 - Note also that the pathtable key is also a 64 bit word, so a map of Ints is all that is required....
 -
 - However, the LocRIB needs to access every prefix table when performing selection
 -
 - Note: the route selection algorithm is at the heart of this system, and is performed for every prefix inserted
 - hence a fast implementation is essential
-}

import Data.IntMap.Strict
import qualified Data.SortedList as SL -- package sorted-list
import qualified Data.List
import Control.Exception(assert)

import BGPData(RouteData)
import Prefixes (IPrefix(..))
import PrefixTable
import BGPDataTestData

prefixList1 = 
        ["1.2.3.4/32"
        ,"1.2.3.4/24"
        ,"1.2.3.4/16"
        ,"1.2.3.4/8"
        , "0.0.0.0/0"
        , "192.168.1.99/24"
        , "129.129.0.0/16"
        , "172.16.0.77/12"
        , "169.254.108.17/32"
        , "10.1.2.3/8"
        ] :: [IPrefix]

l0 = ["1.2.3.4/32"] :: [IPrefix]
l1 = ["1.2.3.4/32" ,"1.2.3.4/24" ,"1.2.3.4/16" ,"1.2.3.4/8"] :: [IPrefix]
l2 = ["2.2.3.4/32" ,"2.2.3.4/24" ,"2.2.3.4/16" ,"2.2.3.4/8"] :: [IPrefix]
l1_1 = ["1.2.3.4/32" ] :: [IPrefix]
l1_2_4 = ["1.2.3.4/24" ,"1.2.3.4/16" ,"1.2.3.4/8"] :: [IPrefix]

update_ pfxs rte t = fst $ PrefixTable.update t pfxs rte
update__ pfx rte t = fst $ PrefixTable.updatePrefixTable t pfx rte
withdraw_ pfxs peer t = fst $ PrefixTable.withdraw t pfxs peer

l = l0
np   = newPrefixTable
up11 = update_ l route11
up12 = update_ l route12
up13 = update_ l route13
up21 = update_ l route21
up22 = update_ l route22
wd1 = withdraw_ l peer1
wd2 = withdraw_ l peer2
ap [] pt = pt
ap (up:ups) pt = ap ups (up pt) 
ap' ups = ap ups np


main' = do
    -- selectTest1
    -- selectTest2
    -- selectTestM
    -- updateTestK
    withdrawSelectTestJ

main'' = do
-- update:: PrefixTable -> [IPrefix] -> RouteData -> (PrefixTable,[IPrefix])
-- withdraw :: PrefixTable -> [IPrefix] -> PeerData -> (PrefixTable,[IPrefix])
-- newPrefixTable :: PrefixTable
-- withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[IPrefix])
    let tell s v = putStrLn $ s ++ " : " ++ show v
    let pt0 = newPrefixTable
    tell "pt0" pt0
    let (pt1,r1) = PrefixTable.update pt0 ["10.0.1.0/24"] route2
    tell "pt1" pt1
    tell "r1" r1
    let (pt2,r2) = PrefixTable.update pt1 ["10.0.1.0/24"] route1
    tell "pt2" pt2
    tell "r2" r2
    let (pt3,r3) = PrefixTable.withdraw pt2 ["10.0.1.0/24"] peer2
    tell "pt3" pt3
    tell "r3" r3
    let (pt4,r4) = PrefixTable.withdraw pt3 ["10.0.1.0/24"] peer1
    tell "pt4" pt4
    tell "r4" r4

main = do
-- update:: PrefixTable -> [IPrefix] -> RouteData -> (PrefixTable,[IPrefix])
-- withdraw :: PrefixTable -> [IPrefix] -> PeerData -> (PrefixTable,[IPrefix])
-- newPrefixTable :: PrefixTable
-- withdrawPeer :: PrefixTable -> PeerData -> (PrefixTable,[IPrefix])
    let tell s v = putStrLn $ s ++ " : " ++ show v
    let pt0 = newPrefixTable
    tell "pt0" pt0
    let (pt1,r1) = PrefixTable.update pt0 ["10.0.1.0/24"] route11
    tell "pt1" pt1
    tell "r1" r1
    let (pt2,r2) = PrefixTable.update pt1 ["10.0.2.0/24"] route12
    tell "pt2" pt2
    tell "r2" r2
    let (pt3,r3) = PrefixTable.update pt2 ["10.0.3.0/24"] route13
    tell "pt3" pt3
    tell "r3" r3
    let (pt4,r4) = PrefixTable.withdrawPeer pt3 peer1
    tell "pt4" pt4
    tell "r4" r4

showPrefixTable = show
showPrefixTableByRoute = show
showRib = showPrefixTable
-- showRib = showPrefixTableByRoute

updateTest = do
   putStrLn "updateTest"
   let pt = newPrefixTable
       rib =   ( update_ ["192.168.1.0/24"] route11 )
             $ ( update_ ["192.168.2.0/24"] route11 )
             $ ( update_ ["192.168.3.0/24"] route11 )
             $ ( update_ ["192.168.11.0/24"] route12 )
             $ ( update_ ["192.168.12.0/24"] route12 )
             $ ( update_ ["192.168.13.0/24"] route12 )
             $ newPrefixTable
   putStrLn $ showPrefixTable rib
   putStrLn $ showRib rib
   let (resPT,resPFX) = PrefixTable.update newPrefixTable prefixList1 route11
   print resPFX
   putStrLn $ showRib resPT

updateTestK = do
   putStrLn "updateTestK"
   let pt = newPrefixTable
       pt1 = update__ "192.168.1.0/24" route11 pt
       pt2 = update__ "192.168.1.0/24" route11 pt1
       pt3 = update__ "192.168.1.0/24" route11 pt2
       rib =   ( update__ "192.168.1.0/24" route11 )
             $ ( update__ "192.168.1.0/24" route11 )
             $ ( update__ "192.168.1.0/24" route11 )
             $ ( update__ "192.168.1.0/24" route12 )
             $ ( update__ "192.168.1.0/24" route13 )
             $ newPrefixTable
   putStrLn $ showPrefixTableByRoute rib
   putStrLn $ showPrefixTable rib
   putStrLn $ show rib

withdrawTest = do
   putStrLn "\nWithdraw test\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l1 route11
       (pt1,_) = PrefixTable.update pt0 l2 route12
   tell' "pt1" pt1

   let pt2 = fst $ withdraw pt1 l1_1 peer1
   tell' "pt2" pt2

   let pt3 = fst $ withdraw pt2 l1_2_4 peer1
   tell' "pt3" pt3

   let pt4 = fst $ withdraw pt3 l2 peer1
   tell' "pt4" pt4

selectTest1 = do
   putStrLn "\nselectTest1\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l0 route11
       (pt1,_) = PrefixTable.update pt0 l0 route12
   tell' "pt0" pt0
   assert (pt0 == pt1) $ tell' "pt1" pt1

selectTest2 = do
   putStrLn "\nselectTest2\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l0 route12
       (pt1,_) = PrefixTable.update newPrefixTable l0 route11
       (pt2,_) = PrefixTable.update newPrefixTable l0 route11
   tell' "pt0" pt0
   tell' "pt1" pt1
   tell' "pt2" pt2
   assert (pt1 == pt2) $ putStrLn "OK"

withdrawSelectTestJ = do
   putStrLn "\nwithdrawSelectTestJ\n"

   tell' "[up11]" $ ap' [up11]
   tell' "[up11,wd1]" $ ap' [up11,wd1]
   tell' "[up21,wd2]" $ ap' [up21,wd2]
   tell' "[up11,wd1,up21,wd2]" $ ap' [up11,wd1,up21,wd2]
   tell' "[up12,wd1,up22]" $ ap' [up12,wd1,up22]
   -- tell' "[up11,up12,up13,up21,up22,up11,up21,wd1,wd2]" $ ap' [up11,up12,up13,up21,up22,up11,up21,wd1,wd2]
   -- tell' "[up11,up12,up13,up21,up22,up11,up21,wd1,wd3]" $ ap' [up11,up12,up13,up21,up22,up11,up21,wd1,wd3]
   -- tell' "[up11,up12,up13,up21,up22,up11,up21,wd1,wd2,wd3]" $ ap' [up11,up12,up13,up21,up22,up11,up21,wd1,wd2,wd3]

selectTestM = do
   putStrLn "\nselectTestM\n"

   tell' "[up11,up11,up11,up11]" $ ap' [up11,up11,up11,up11]
   tell' "[up22,up12]" $ ap' [up22,up12]
   tell' "[up12,up22]" $ ap' [up12,up22]
   tell' "[up22,up12,up12]" $ ap' [up22,up12,up12]
   tell' "[up22,up12,up11]" $ ap' [up22,up12,up11]
   tell' "[up12,up11]" $ ap' [up12,up11]
   tell' "[up11,up12]" $ ap' [up11,up12]
   tell' "[up11,up12,up13,up21,up22,up11,up21]" $ ap' [up11,up12,up13,up21,up22,up11,up21]

selectTestN = do
   putStrLn "\nselectTestN\n"
   let
       (pt0,_) = PrefixTable.update newPrefixTable l1     route12
       (pt1,_) = PrefixTable.update pt0            l1     route22
       (pt2,_) = PrefixTable.update pt1            l1_2_4 route11
       (pt3,_) = PrefixTable.update pt2            l1_2_4 route21
       (pt4,_) = PrefixTable.withdraw pt3          l1_1   peer1
       (pt5,_) = PrefixTable.withdraw pt4          l1     peer2
       (pt6,_) = PrefixTable.withdraw pt5          l1     peer1
   tell' "pt0" pt0
   tell' "pt1" pt1
   tell' "pt2" pt2
   tell' "pt3" pt3
   tell' "pt4" pt4
   tell' "pt5" pt5
   tell' "pt6" pt6

tell' s pt = do
    putStrLn $ s ++ ": "
    putStrLn $ showRib pt
    putStrLn "==========================\n"

tell (pt,pfxs) = do
    putStr $ showRib pt
    putStr " -- "
    print pfxs
