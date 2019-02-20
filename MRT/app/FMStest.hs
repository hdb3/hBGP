module Main where
import qualified Data.ByteString.Lazy as BS -- Lazy version import
import MRTformat
import FilterMoreSpecifics

main :: IO ()
main = do
    putStrLn "FMS-test"
    (header:mrts) <- fmap mrtParse BS.getContents
    putStrLn $ show (length mrts) ++ " MRT messages read"
    let filteredMRTMessages = filterLS mrts
    putStrLn $ show (length filteredMRTMessages) ++ " MRT messages after filter"
    --let tree = mrtToTree (take 10 mrts)
    --putStrLn $ "count = " ++ show (count tree) ++ "  size = " ++ show (size tree)
    --print tree
    --print (toList tree)
    let gauge p = length (filter p mrts)
    putStrLn $ show (gauge filterSlash32) ++ " /32s"
    putStrLn $ show (gauge filterSlash25to31) ++ " /25-31s"
    putStrLn "done"
