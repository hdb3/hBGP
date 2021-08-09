module Main where

import BGPRib.BGPRib
import BGPlib.AttoBGP
import BGPlib.BGPlib
import Data.Attoparsec.ByteString
import Data.Binary
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import System.Environment (getArgs)

main' = do
  bs <- B.getContents
  parseCheck wireParser bs

parseCheck p bs = do
  either
    (\s -> putStrLn $ "parse failed: " ++ s)
    print
    (parseOnly p bs)

main = do
  test (["0.0.0.0/0"] :: [Prefix])
  test (["10.0.0.0/8"] :: [Prefix])
  test (["172.16.0.0/12"] :: [Prefix])
  test (["172.16.1.0/16"] :: [Prefix])
  test (["192.168.1.0/24"] :: [Prefix])
  test (["169.254.169.254/32"] :: [Prefix])

  test
    ( [ "0.0.0.0/0",
        "10.0.0.0/8",
        "172.16.0.0/12",
        "172.16.1.0/16",
        "192.168.1.0/24",
        "169.254.169.254/32"
      ] ::
        [Prefix]
    )
  where
    test pfxs = do
      print pfxs
      let bs = toStrict $ encode pfxs
      putStrLn $ toHex bs
      parseCheck (parsePrefixes (B.length bs)) bs
      putStrLn ""
