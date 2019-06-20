module Main where
import BGPlib.AttoBGP
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

main = do
    bs <- B.getContents
    either (\s -> putStrLn $ "parse failed: " ++ s)
           (\msgs -> putStrLn $ "read " ++ show (length msgs) ++ " messages from " ++ show (B.length bs) ++ " bytes" )
           ( parseOnly wireParser bs )
