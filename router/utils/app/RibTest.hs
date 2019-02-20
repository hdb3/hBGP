{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.IP
import RibDef
import RIBData
import MapRib
import IP4Prefix

emptyRib = mkRib compare :: MapRib
emptyRib' = ([], emptyRib)

print' (a,b) = putStrLn $ showRibChanges a ++ "\n" ++ show b

main = c5 >> c6

c6 = print' $ withdrawM prefix1 peer2 $
              updateM prefix1 peer1 route1 $
              updateM prefix1 peer2 route2 emptyRib'

c5 = print' $ withdrawM prefix1 peer2 $
              updateM prefix1 peer2 route2 $
              updateM prefix1 peer1 route1 emptyRib'

c4 = print' $ withdrawM prefix1 peer1 $
              updateM prefix1 peer2 route2 $
              updateM prefix1 peer1 route1 emptyRib'

c3 = print' $ withdrawM prefix1 peer1 $ updateM prefix1 peer1 route1 emptyRib'
c2 = print' $ updateM prefix1 peer1 route1 emptyRib'
c1 = print $ emptyRib

main' = do
    putStrLn "RibTest"

    let riby = buildUpdateSequence peer1 routes emptyRib
        ribz = buildUpdateSequence peer2 routes riby

    let riby' = buildUpdateSequence' peer1 routes emptyRib'
        ribz' = buildUpdateSequence' peer2 routes ([],riby)
        ribz'' = buildUpdateSequence' peer2 routes $ buildUpdateSequence' peer1 routes emptyRib'
        ribz''' = buildUpdateSequence' peer1 routes $ buildUpdateSequence' peer2 routes emptyRib'
        ribz'''' = removePeerM peer1 ribz'''
        ribz''''' = removePeerM peer2 ribz'''

    putStrLn "\n---------------\n"
    putStrLn "riby\n"
    print riby
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst riby'

    putStrLn "\n---------------\n"
    putStrLn "ribz\n"
    print ribz
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst ribz'
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst ribz''
    putStrLn "\n---------------\n"

    putStrLn "\n---------------\n"
    putStrLn "ribz'''\n"
    print $ snd ribz'''
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst ribz'''
    putStrLn "\n---------------\n"

    putStrLn "\n---------------\n"
    putStrLn "ribz''''\n"
    print $ snd ribz''''
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst ribz''''
    putStrLn "\n---------------\n"

    putStrLn "\n---------------\n"
    putStrLn "ribz'''''\n"
    print $ snd ribz'''''
    putStrLn "\n. . . . . . . .\n"
    putStrLn $ showRibChanges $ fst ribz'''''
    putStrLn "\n---------------\n"

routes = [(prefix1,route1),(prefix2,route2),(prefix3,route3)]
peer1 = Peer "peer1" True 64500 "10.0.0.1" "10.0.0.1" "10.0.0.99"
peer2 = Peer "peer2" False 64501 "10.0.0.2" "10.0.0.2" "10.0.0.99"
route1 = Route "route1" 100 [] 5 0 0 True
route2 = Route "route2" 200 [] 4 1 0 False
route3 = Route "route3" 300 [] 3 1 0 False
prefix1 = "192.168.1.0/24" :: IP4Prefix
prefix2 = "192.168.2.0/24" :: IP4Prefix
prefix3 = "192.168.3.0/24" :: IP4Prefix
