{-# LANGUAGE FlexibleContexts #-}
module Report where
import Data.Word
import System.IO

import BGPlib
import BGPRib
--import Common
--import BGPparse
--import GetBGPMsg
--import Update
--import PathAttributes
--import PathAttributeUtils
--import Prefixes
--import Rib
--import BGPData
--import PrefixTable
--import PrefixTableUtils
--import AdjRIBOut

report :: (PrefixTable, [AdjRIBEntry]) -> IO ()
report (rib,adjrib) = do
        let fib = getFIB rib
            ribOut = getAdjRIBOut rib
            locRib = getRIB rib
            groomedAdjRib = groomAdjRIBList adjrib
        hPutStrLn stderr $ "got " ++ show (length fib) ++ " prefixes"
        hPutStrLn stderr $ "got " ++ show (length adjrib) ++ " updates for peer"
        hPutStrLn stderr $ "got " ++ show (length groomedAdjRib) ++ " groomed updates for peer"
        hPutStrLn stderr $ "got " ++ show (length ribOut) ++ " routes"
        hPutStrLn stderr $ "locRib size = " ++ show (length locRib)
        if True then do
            -- putStrLn $ showPrefixTable rib
            -- putStrLn "\n #############################\n"
            -- putStrLn $ showPrefixTableByRoute rib
            putStrLn $ showPrefixTableByRoute' customShowRoute rib
            putStrLn ""
        else
            return ()
    where

    customShowRoute :: RouteData -> String
    customShowRoute = showASPath . getASPath . pathAttributes
    -- customShowRoute route = show (pathAttributes route)

    showASPath :: ASPath -> String
    showASPath = showPath . stripASPath

    showPath :: ASNumber asn => [ASSegment asn] -> String
    showPath [ASSequence seq1 , ASSet set, ASSequence seq2] = "SEQ+SET+SEQ " ++ show seq1 ++ " / " ++ show set ++ " / " ++ show seq2
    showPath [ASSequence seq , ASSet set] = "SEQ+SET     " ++ show seq ++ " / " ++ show set
    showPath [ASSequence seq] = "SEQ-       " ++ show seq
    showPath [] = "EMPTY       "
    showPath x = "UNKNOWN     " ++ show x

    stripASPath :: ASPath -> [ASSegment Word32]
    stripASPath ((ASPath4 path)) = path
    stripASPath ((ASPath2 path)) = gmap fromIntegral path
