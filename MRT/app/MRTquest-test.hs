module Main where
import MRTquest

main :: IO ()
main = mainAllRecords

mainGroupedRecords = do
    putStrLn "get grouped records"
    groups <- getGroupedMRT
    print $ fmap (\(a,b) -> (a, length b)) groups

mainAllRecords = do
    putStrLn "get all records"
    mrt <- getMRT
    putStrLn $ show (length mrt) ++ " records read"
    putStr $ unlines $ map show $ mrtTypeCount mrt

mainRibRecords = do
    putStrLn "get RIB records"
    (peerTable,rib) <- getMRTTableDumpV2
    putStrLn $ "peer table:" ++ show peerTable
    putStrLn $ show (length rib) ++ " records read"

mainUpdateRecords = do
    putStrLn "get Update records"
    updates <- getMRTUpdates
    putStrLn $ show (length updates) ++ " records read"
