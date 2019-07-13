{-# LANGUAGE RecordWildCards #-}
module BGPRib.BGPReader( updateRib, readMsgs, readRib, bgpMsgReader, bgpReader, readGroupedRibF, readGroupedRib, pathReadRib ) where
import System.Exit(die)
import System.Environment(getArgs)
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get(runGet)

import BGPlib.BGPlib
import BGPRib.BGPRib
import qualified BGPRib.BGPRib as BGPRib
import BGPRib.BogonFilter
import BGPRib.PathFilter

bgpMsgReader :: FilePath -> IO [BGPMessage]
bgpMsgReader path = do
    stream <- L.readFile path
    let bgpByteStrings = runGet getBGPByteStrings stream
        bgpMessages = map decodeBGPByteString bgpByteStrings
    return bgpMessages

bgpUpdateMsgReader :: FilePath -> IO [ParsedUpdate]
bgpUpdateMsgReader = fmap ( map getUpdate . filter isUpdate ) . bgpMsgReader

bgpReader :: FilePath -> IO [(BGPRib.RouteData, IPrefix)]
bgpReader path = do
    updates <- bgpUpdateMsgReader path
    rib <- BGPRib.newRib BGPRib.dummyPeerData
    mapM_ (updateRib rib) updates
    rib' <- BGPRib.getLocRib rib
    return (getRIB rib')
updateRib :: Rib -> ParsedUpdate -> IO ()
updateRib rib parsedUpdate@ParsedUpdate{..} = BGPRib.ribPush rib BGPRib.dummyPeerData parsedUpdate

-- readRib: a convenience function for simple applications
-- the returned structure masks only derived or artificial data
-- - it contains the full parsed list of path attributes, associated prefixes (unpacked), and a route identifer which is unique in all cases
--   of path attribute sets, it is a hash of the original path attribute binary structure
--  However, it only contains the last version of the table, so earlier updates in the stream which were superceded are not returned


readRib :: IO [((Int, [PathAttribute]), IPrefix)]
readRib = readUngroupedRib

readUngroupedRib :: IO [((Int, [PathAttribute]), IPrefix)]
readUngroupedRib = fmap ( map normalise ) readRib' 

readGroupedRib :: IO [((Int, [PathAttribute]), [IPrefix])]
readGroupedRib = fmap (map normalise . applyBogonFilter . groupBy_) readRib'

readGroupedRibF :: IO [((Int, [PathAttribute]), [IPrefix])]
readGroupedRibF = fmap applyBogonFilter readGroupedRib

pathReadRib :: FilePath -> IO [((Int, [PathAttribute]), [IPrefix])]
pathReadRib path = fmap ( applyPathFilter . map normalise . applyBogonFilter . groupBy_ ) ( bgpReader path)

readMsgs :: IO [BGPMessage]
readMsgs = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then
        die "no filename specified"
    else do
        msgs <- bgpMsgReader (args !! 0)
        if n == 0 then
            return msgs
        else
            return (take n msgs)

-- TODO convert the readrib chain to use readMsgs.....
readRib' :: IO [(RouteData, IPrefix)]
readRib' = do
    args <- getArgs
    let n = if 1 < length args then read (args !! 1) :: Int else 0
    if null args then
        die "no filename specified"
    else do
        rib <- bgpReader (args !! 0)
        if n == 0 then
            return rib
        else
            return (take n rib)

normalise :: (RouteData, t) -> ((Int, [PathAttribute]), t)
normalise (routeData,a) = ((BGPRib.routeId routeData , BGPRib.pathAttributes routeData) , a)
