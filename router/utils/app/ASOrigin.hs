module Main where

import BGPReader(readRib)
import BGPlib(getASPathOrigin)
import RIBData

main = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    let getRoute ((_,attributes),prefix) = (makeRoute True attributes,prefix)
    let getOrigin ((_,attributes),prefix) = (getASPathOrigin attributes,prefix)
    -- print attributes
    let route = getRoute (last rib)
        routes = map getRoute rib 
        origins = map getOrigin rib 
    putStrLn $ unlines $ map show routes
    putStrLn $ unlines $ map show origins
    --print $ getASPathOrigin attributes
