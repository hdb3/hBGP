module Main where

import BGPReader (readGroupedRib)
import BogonFilter
import Comm (distribution)
import qualified Data.List
import qualified Overlap
import Prefixes

main = do
  rib <- readGroupedRib
  putStrLn $ "got " ++ show (length rib) ++ " routes"
  let allPrefixes = concatMap snd rib
  report allPrefixes

report :: [Prefix] -> IO ()
report prefixes = do
  let len (Prefix (a, b)) = a
      -- drop default if present and also every thing longer than /24
      bogonFree = filter bogonFilter prefixes
      real = filter ((25 >) . len) $ filter ((0 /=) . len) bogonFree
      bogonDiscards = (length prefixes) - (length bogonFree)
      realDiscards = (length bogonFree) - (length real)

  putStrLn $
    "got "
      ++ show (length prefixes)
      ++ " raw prefixes"
      ++ if bogonDiscards > 0
        then ", discarded " ++ show bogonDiscards ++ " bogons"
        else
          ""
            ++ if realDiscards > 0 then ", discarded " ++ show realDiscards ++ " /25+ or 0.0.0.0" else ""

  let sortPrefixes = Data.List.sortOn tuple
        where
          tuple (Prefix (a, b)) = (a, b)
      coverage (Prefix (a, _)) = 2 ^ (32 - a)
      fullCoverage =
        2 ^ 32
          - 2 * (2 ^ (32 - 4))
          - 3 * (2 ^ (32 - 8))
          - 1 * (2 ^ (32 - 10))
          - 1 * (2 ^ (32 - 12))
          - 1 * (2 ^ (32 - 15))
          - 2 * (2 ^ (32 - 16))
          - 8 * (2 ^ (32 - 24))
          - 6 * (2 ^ (32 - 32))
      -- list compiled from RFC https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry.xhtml
      --                        https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry-1.csv
      -- and adding a /4 for multicast (224.0.0.0/4)

      t = Overlap.fromList real
      r = Overlap.reduce t
      p = Overlap.partition r
      t' = sortPrefixes $ Overlap.toList r
      p' = sortPrefixes $ concatMap Overlap.toList p
      (single, multiple) = Data.List.partition ((2 >) . Overlap.size) p
      monsters = filter ((7 <) . Overlap.height) p
      slobs = filter ((999 <) . Overlap.size) p

  if real /= t' then putStrLn "consistency check on Overlap.fromList / Overlap.toList FAILS!!!!" else return ()
  if real /= p' then putStrLn "consistency check on Overlap.partition FAILS!!!!" else return ()

  putStrLn $ "tree contains " ++ show (Overlap.size t)
  putStrLn $ "tree height   " ++ show (Overlap.height t)

  -- naive longest - would be much faster to use partitions first
  -- putStrLn $ "longest   " ++ show (Overlap.longest t)
  putStrLn $ "tree contains " ++ show (length p) ++ " partitions"
  putStrLn $ show (length single) ++ " non-overlapping and " ++ show (length multiple) ++ " overlapping partitions"
  putStrLn $ "full distribution by size: " ++ show (distribution $ map Overlap.size p)
  putStrLn $ "full distribution by height: " ++ show (distribution $ map Overlap.height p)
  putStrLn $ "the monsters are: " ++ show (map Overlap.head monsters) ++ "(" ++ show (map Overlap.size monsters) ++ ")"
  putStrLn $ "the slobs are: " ++ show (map Overlap.head slobs) ++ "(" ++ show (map Overlap.size slobs) ++ ")"

  putStrLn "\nCoverage analysis\n"
  let heads = map Overlap.head p
      actualCoverage = sum $ map coverage heads
      overlapCoverage = sum $ map coverage real
  putStrLn $ "actual coverage is " ++ show actualCoverage ++ " (" ++ show (100 * actualCoverage `div` fullCoverage) ++ "%)"
  putStrLn $ "overlap coverage is " ++ show overlapCoverage ++ " (" ++ show (100 * overlapCoverage `div` fullCoverage) ++ "%)"
