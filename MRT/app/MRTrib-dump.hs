module Main where

import Control.Monad (when)
import Data.List (isPrefixOf)
import MRTBuilder
import MRTrib
import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Read (readMaybe)

verbose = any (isPrefixOf "--v") <$> getArgs

info s = do
  v <- verbose
  when v (hPutStrLn stderr s)

main :: IO ()
main = do
  info "MRTrib-dump"
  routes <- getRoutes
  sample <- selectRoutes routes
  handle <- getOutputHandle
  dumpRoutes handle sample

restrict :: Int -> [(BGPAttributes, IP4PrefixList)] -> [(BGPAttributes, IP4PrefixList)]
-- restrict limit = filter ((limit > ) . length . snd)
restrict limit = map (\(attrs, prefixes) -> (attrs, take limit prefixes))

selectRoutes allRoutes = do
  -- numeric positional parameters
  -- 1 # routes to dump - default = all
  -- 2 maximum prefix population to permit - default = no limit
  --
  let getOptArgN n = do
        args <- getArgs
        return $ if length args < n + 1 then Nothing else readMaybe (args !! n)
  --
  maxCount <- getOptArgN 1
  packCap <- getOptArgN 2
  --
  let countedRoutes = maybe allRoutes (`take` allRoutes) maxCount
      cappedRoutes = maybe countedRoutes (`restrict` countedRoutes) packCap
  --
  let prefixTotal = sum $ map (length . snd) cappedRoutes
      updateTotal = length cappedRoutes
  --
  info $ show updateTotal ++ " updates " ++ show prefixTotal ++ " prefixes"
  --
  return cappedRoutes

getOutputHandle = do
  istty <- queryTerminal stdOutput
  if istty then openFile "updates.raw" WriteMode else return stdout

getRoutes = do
  mrts <- getMRTTableDump
  if null mrts
    then die "no RIB records found in file"
    else info $ show (length mrts) ++ " records read "
  let ipv4PeerTable = getMRTRibV4 mrts
  info $ showMRTRibV4 ipv4PeerTable
  let (index, peer, routeMap) = last (sortRibsOnPrefixCount ipv4PeerTable)
      routes = fromRouteMapv4 routeMap
  info $ "selected rib for peer(" ++ show index ++ ") " ++ show peer
  return routes

dumpRoutes :: Handle -> [(BGPAttributes, IP4PrefixList)] -> IO ()
dumpRoutes h routes = do
  hPutUpdates h routes
  hClose h
