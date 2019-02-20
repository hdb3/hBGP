module Main where
import qualified Data.ByteString.Lazy as BS -- Lazy version import
import Control.Monad(mapM_)
import MRTformat

main :: IO ()
main = do
    putStrLn "MRTlib-test"
    f <- BS.getContents
    let mrtMsgs = mrtParse f
    mapM_ print mrtMsgs 
    putStrLn "done"
