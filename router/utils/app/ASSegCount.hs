module Main where

import BGPReader(readRib)
import BGPlib(getASPathContent,getASPathSegmentCount,isSingletonASSet)

main = do
    rib <- readRib
    putStrLn $ "got " ++ show (length rib) ++ " routes"
    let getAttributes ((_,attributes),prefix) = (attributes,prefix)
        relavantAttributes (attributes,prefix) = (prefix, getASPathContent attributes)
        routes = map getAttributes rib
        getASPath (attributes,_) = getASPathContent attributes

        nSegmentCount = getASPathSegmentCount . fst
        pSingletonSetRoute = isSingletonASSet . last . getASPath
        pInterestingPathRoute r = pComplexPathRoute r && ((2 < nSegmentCount r) || ( not ( pSingletonSetRoute r )))
        pComplexPathRoute = (1 <) . getASPathSegmentCount . fst

        complexPathRoutes = filter pComplexPathRoute routes
        interestingPathRoutes = filter pInterestingPathRoute complexPathRoutes
    putStrLn $ "got " ++ show (length complexPathRoutes) ++ " complex routes"
    putStrLn $ "got " ++ show (length interestingPathRoutes) ++ " interesting routes"
    putStrLn $ unlines $ map ( show . relavantAttributes ) interestingPathRoutes
