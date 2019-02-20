module Main where

import Core
import BasicOperations

main :: IO ()
main = do
    putStrLn "1,2 : 2"
    display $ updateBasicGroups [mkBasicGroup [1,2]] [2]
    putStrLn ""

    where
    
    display :: ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)]) -> IO()
    display (newGroups , cg , editList) = do
        putStrLn $ "new groups:      " ++ show newGroups
        putStrLn $ "composite group: " ++ show cg
        putStrLn $ "edit list:       " ++ show editList

    testUpdateBasicGroups = do
        putStrLn "BasicOperations-test"
        --updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)])
        
        -- note - the only valid inputs are those in which the prefix list and basic groups are both not null
        --        and in which the prefix list and basic groups are all disjoint
        --        and in which all members of prefix list are members of the basic groups
        -- display $ updateBasicGroups [] [] -- INVALID REQUEST!!!!
        -- putStrLn ""
        -- display $ updateBasicGroups [(mkBasicGroup [1])] [] -- INVALID REQUEST!!!!
        putStrLn "1,2 : 2"
        display $ updateBasicGroups [mkBasicGroup [1,2]] [2]
        putStrLn ""
    
        putStrLn "1,2 3,4 : 2"
        display $ updateBasicGroups (map mkBasicGroup [[1,2],[3,4]]) [2]
        putStrLn ""
    
        putStrLn "1,2 3,4 : 2,3"
        display $ updateBasicGroups (map mkBasicGroup [[1,2],[3,4]]) [2,3]
        putStrLn ""
    
        putStrLn "1,2,3,4  5 : 2,3,5"
        display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5]]) [2,3,5]
        putStrLn ""
    
        putStrLn "1,2,3,4  5,6 7 : 2,3,5"
        display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5,6],[7]]) [2,3,5]
        putStrLn ""
    
        putStrLn "1,2,3,4  5,6 7 : 7,2,3,5"
        display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5,6],[7]]) [7,2,3,5]
        putStrLn ""
    
    testUpdateCompositeGroup = do
        putStrLn "BasicOperations-test - updateCompositeGroup"
        -- updateCompositeGroup :: [(BasicGroup,BasicGroup,BasicGroup)] -> CompositeGroup -> CompositeGroup
        print $ updateCompositeGroup [] ( mkCompositeGroup $ map mkBasicGroup [[1,2],[3,4]] )
        print $ updateCompositeGroup [(mkBasicGroup [1,2], mkBasicGroup [1], mkBasicGroup [2])] ( mkCompositeGroup $ map mkBasicGroup [[1,2],[3,4]] )
        print $ updateCompositeGroup [(mkBasicGroup [2,1], mkBasicGroup [1], mkBasicGroup [2])] ( mkCompositeGroup $ map mkBasicGroup [[1,2],[3,4]] )
        print $ updateCompositeGroup [(mkBasicGroup [2,1], mkBasicGroup [1], mkBasicGroup [2])] ( mkCompositeGroup $ map mkBasicGroup [[4,3],[2,1]] )
        print $ updateCompositeGroup [(mkBasicGroup [2,1], mkBasicGroup [1], mkBasicGroup [2])] ( mkCompositeGroup $ map mkBasicGroup [[4,2],[3,1]] )
    
        print $ updateCompositeGroup [(mkBasicGroup [1,2], mkBasicGroup [1], mkBasicGroup [2])
                                     ,(mkBasicGroup [3,4,5], mkBasicGroup [3], mkBasicGroup [4,5])]
                                     ( mkCompositeGroup $ map mkBasicGroup [[1,2],[3,4,5]] )
