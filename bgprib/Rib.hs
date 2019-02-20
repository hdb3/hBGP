{-# LANGUAGE RecordWildCards #-}
module Rib where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Control.Monad(unless,when,void)
import Data.List(intercalate)

import BGPlib
import BGPData
import PrefixTable
import qualified PrefixTableUtils
import Update
import AdjRIBOut

type Rib = MVar Rib'
-- TODO rename AdjRIB -> AdjRIBMap
-- and create a type 'AdjRIBMapEntry = (PeerData,AdjRIBTable)'

type AdjRIB = Data.Map.Map PeerData AdjRIBTable
data Rib' = Rib' { 
                   prefixTable :: PrefixTable
                 , adjRib :: AdjRIB
                 }

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
    (Rib' _ adjRib ) <- readMVar rib
    return $ Data.Map.keys adjRib


showAdjRIBMapEntry :: (PeerData,AdjRIBTable) -> IO String
showAdjRIBMapEntry (peerData, adjRIBTable) = do
    s <- showAdjRIBTable adjRIBTable
    let p = show peerData
    return $ "AdjRIB (" ++ p ++ " : " ++ s ++ ")"

showAdjRIB :: AdjRIB -> IO String
showAdjRIB adjRIBMap = do
    let adjRIBs = Data.Map.toList adjRIBMap
    s <- mapM showAdjRIBMapEntry adjRIBs
    return $ intercalate "\n                 " s

showRib :: Rib -> IO String
showRib r = do
    rib <- readMVar r
    let s1 = show (prefixTable rib)
    s2 <- showAdjRIB (adjRib rib)
    return $ "Rib { prefixTable = " ++ s1 ++ " ,\n      adjRib = [ " ++ s2 ++ "]"

printRib r = showRib r >>= putStrLn

newRib :: PeerData -> IO Rib
newRib localPeer = do
    adjRib <- newAdjRIBTable
    newMVar $ Rib' newPrefixTable ( Data.Map.singleton localPeer adjRib )

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyMVar_ rib ( delPeer' peer )

delPeer' :: PeerData -> Rib' -> IO Rib'
delPeer' peer Rib' {..} = do
    -- drain the prefix table and save the resulting withdraws
    let (prefixTable',iprefixes) = withdrawPeer prefixTable peer
    -- schedule the withdraw dissemination
    -- NOTE - this does not change the AdjRIBMap
    unless (null iprefixes)
         ( updateRibOutWithPeerData peer nullRoute iprefixes adjRib)
    -- now remove this peer completely from the AdjRIBMap
    -- it is liekly that this could be done before the previous action.....
    -- but the semantics should be identical as long as we didn't try to send withdraw messages to the peer which has gone away...
    let adjRib' = (Data.Map.delete peer adjRib)
    return $ Rib' prefixTable' adjRib'

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyMVar_ rib ( addPeer' peer )

addPeer' ::  PeerData -> Rib' -> IO Rib'
addPeer' peer Rib' {..} = do
        -- get a complete RIB dump for the new peer...
    let ribDump = map f (PrefixTableUtils.getAdjRIBOut prefixTable) where
        f (rd,ipfxs) = (ipfxs , routeId rd)
        -- make the RIB dump into a Fifo
    aro <- fifo ribDump
        -- TODO - this would be the place to insert an end-of-rib marker
    let adjRib' = Data.Map.insert peer aro adjRib
    return $ Rib' prefixTable adjRib'

queryRib :: Rib -> IPrefix -> IO (Maybe RouteData)
queryRib rib prefix = do
    rib' <- readMVar rib
    return $ queryPrefixTable (prefixTable rib') prefix 

-- overloaded in a very non-Haskell way - requesting zero updates actually returns everything!
pullAllUpdates :: Int -> PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates t peer rib = do
    (Rib' _ arot) <- readMVar rib
    dequeueTimeout t (arot Data.Map.! peer)
-- TODO write and use the function 'getAdjRibForPeer'



pullUpdates :: Int -> PeerData -> Rib -> IO [AdjRIBEntry]
pullUpdates n peer rib = do
    (Rib' _ arot) <- readMVar rib
    if n == 0
    then dequeueAll (arot Data.Map.! peer) 
    else  dequeueN n (arot Data.Map.! peer) 

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
    rib' <- readMVar rib
    return (prefixTable rib')

getAdjRib :: Rib -> IO AdjRIB
getAdjRib rib = do
    rib' <- readMVar rib
    return (adjRib rib')

updateRibOutWithPeerData :: PeerData -> RouteData -> [IPrefix] -> AdjRIB -> IO ()
-- NOTE!!!! - we can be called with a null route in which case only the routeId is defined, and is equal 0!!!
-- this is OK since we only get the routeId in this function

updateRibOutWithPeerData originPeer routeData updates adjRib = do
    let updateWithKey destinationPeer table = if (destinationPeer /= originPeer) && ( isExternal destinationPeer || isExternal originPeer )
                               then insertAdjRIBTable (updates, routeId routeData ) table
                               else ( return ())
    when (null updates)
         (putStrLn $ "null updates in updateRibOutWithPeerData: " ++ show originPeer ++ " / " ++ if (0==routeId routeData) then "nullRoute" else show routeData)
    void $ sequence $ Data.Map.mapWithKey updateWithKey adjRib

makeRouteData :: PeerData -> ParsedUpdate -> RouteData
makeRouteData peerData parsedUpdate = makeRouteData' peerData ( puPathAttributes parsedUpdate) ( hash parsedUpdate)


makeRouteData' :: PeerData -> [PathAttribute] -> Int -> RouteData
makeRouteData' peerData pathAttributes routeId = RouteData peerData pathAttributes routeId pathLength nextHop origin med fromEBGP
    where
    pathLength = getASPathLength pathAttributes
    fromEBGP = isExternal peerData
    med = if fromEBGP then 0 else getMED pathAttributes
    nextHop = getNextHop pathAttributes
    origin = getOrigin pathAttributes

ribUpdater :: Rib -> PeerData -> ParsedUpdate -> IO()
ribUpdater rib routeData update = modifyMVar_ rib (ribUpdater' routeData update)

ribUpdater' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
-- TODO write the monadic style in a way that works????!!!
-- ribUpdater' peerData ParsedUpdate{..} = ribUpdateMany' peerData puPathAttributes hash nlri >>= ribWithdrawMany' peerData withdrawn
ribUpdater' peerData ParsedUpdate{..} rib0 = do
    rib1 <- ribUpdateMany' peerData puPathAttributes hash nlri rib0
    rib2 <- ribWithdrawMany' peerData withdrawn rib1 
    return rib2

-- TODO - convert ribUpdateMany/ribWithdrawMany to IPrefix based, for consistency...
ribUpdateMany :: Rib -> PeerData -> [PathAttribute] -> Int -> [Prefix] -> IO()
ribUpdateMany rib peerData attrs hash pfxs = modifyMVar_ rib (ribUpdateMany' peerData attrs hash pfxs)

ribUpdateMany' :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
ribUpdateMany' peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
    | null pfxs = return (Rib' prefixTable adjRibOutTables ) 
    | otherwise = do
          let routeData = makeRouteData' peerData pathAttributes routeId
              ( prefixTable' , updates ) = PrefixTable.update prefixTable (fromPrefixes pfxs) routeData
          updateRibOutWithPeerData peerData routeData updates adjRibOutTables
          return $ Rib' prefixTable' adjRibOutTables

ribWithdrawMany rib peer p = modifyMVar_ rib (ribWithdrawMany' peer p)

ribWithdrawMany' :: PeerData -> [Prefix] -> Rib' -> IO Rib'
ribWithdrawMany' peerData pfxs (Rib' prefixTable adjRibOutTables)
    | null pfxs = return (Rib' prefixTable adjRibOutTables )
    | otherwise = do
        let ( prefixTable' , withdraws ) = PrefixTable.withdraw prefixTable (fromPrefixes pfxs) peerData
        -- adjRibOutTables' = updateAdjRibOutTables (updates,0) adjRibOutTables
        -- ** NOTE **
        -- The withdraw entry in the adj-rib-out has a special route value
        -- in future this could be better done as just the withdrawn route with an indicator to distinguish it from a normal one
        -- probably just using Either monad?
        updateRibOutWithPeerData peerData nullRoute withdraws adjRibOutTables
        return $ Rib' prefixTable' adjRibOutTables

