{-# LANGUAGE RecordWildCards, TupleSections #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates) where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Control.Monad(unless,when,void)
import Data.List(intercalate)
import Data.Word(Word32)

import BGPlib.BGPlib

import BGPRib.BGPData
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPRib.Update
import BGPRib.AdjRIBOut
import BGPRib.Common(group_)

type Rib = MVar Rib'
-- TODO rename AdjRIB -> AdjRIBMap
-- and create a type 'AdjRIBMapEntry = (PeerData,AdjRIBTable)'

type AdjRIB = Data.Map.Map PeerData AdjRIBTable
data Rib' = Rib' { prefixTable :: PrefixTable
                 , adjRib :: AdjRIB }

newRib :: PeerData -> IO Rib
newRib localPeer = do
    adjRib <- newAdjRIBTable
    newMVar $ Rib' newPrefixTable ( Data.Map.singleton localPeer adjRib )

getPeersInRib :: Rib -> IO [PeerData]
getPeersInRib rib = do
    (Rib' _ adjRib ) <- readMVar rib
    return $ Data.Map.keys adjRib

delPeer :: Rib -> PeerData -> IO ()
delPeer rib peer = modifyMVar_ rib ( delPeer' peer )

    where

    delPeer' :: PeerData -> Rib' -> IO Rib'
    delPeer' peer Rib' {..} = do
        -- drain the prefix table and save the resulting withdraws
        let (prefixTable',iprefixes) = withdrawPeer prefixTable peer
        -- schedule the withdraw dissemination
        -- NOTE - this does not change the AdjRIBMap
        unless (null iprefixes)
             -- ( updateRibOutWithPeerData peer nullRoute iprefixes adjRib)
             ( putStrLn "WARNING - delPeer' incomplete due to change in signature of updateRibOutWithPeerData" )
        -- now remove this peer completely from the AdjRIBMap
        -- it is liekly that this could be done before the previous action.....
        -- but the semantics should be identical as long as we didn't try to send withdraw messages to the peer which has gone away...
        return $ Rib' prefixTable' ( Data.Map.delete peer adjRib )

addPeer :: Rib -> PeerData -> IO ()
addPeer rib peer = modifyMVar_ rib ( addPeer' peer )

    where

    addPeer' :: PeerData -> Rib' -> IO Rib'
    addPeer' peer Rib' {..} = do
            -- get a complete RIB dump for the new peer...
        let ribDump = map f (PrefixTableUtils.getAdjRIBOut prefixTable)
            f (rd,pfxs) = (pfxs , routeId rd)
            -- make the RIB dump into a Fifo
        aro <- fifo ribDump
            -- TODO - this would be the place to insert an end-of-rib marker
        let adjRib' = Data.Map.insert peer aro adjRib
        return $ Rib' prefixTable adjRib'

lookupRoutes :: Rib -> AdjRIBEntry -> IO [(RouteData,[Prefix])]
lookupRoutes rib (iprefixes,routeHash) = group_ <$> mapM (\pfx -> (,pfx) <$> adjRibQueryRib rib pfx routeHash) iprefixes

    where
    -- adjRibQueryRib extends queryRib by checking that the current route hash matches the one saved when the AdjRIbOut entry was created
    --    this is useful because if it has changed then probably the correct action is to discard the result
    --    the special case of routeId == 0 is not hadnled differently - this would correspond to a withdraw - it should never occur in a RouteData record
    --    but it could on lookup, in which case the correct behaviour would be to discard the withdraw if the prefix is found
    --    however the caller should not use this function since there is no valid Just value which can represent the withdraw
    --    instead the caller should merely use queryRib and discard the withdraw if the return value is not Nothing
    adjRibQueryRib :: Rib -> Prefix -> Int -> IO (Maybe RouteData)
    adjRibQueryRib rib iprefix routeHash =
        maybe Nothing (\route -> if routeHash == routeId route then Just route else Nothing) <$> queryRib rib iprefix

    queryRib :: Rib -> Prefix -> IO (Maybe RouteData)
    queryRib rib prefix = do
        rib' <- readMVar rib
        return $ queryPrefixTable (prefixTable rib') prefix

pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
    (Rib' _ arot) <- readMVar rib
    --dequeueAll (arot Data.Map.! peer)
    rval <- dequeueAll (arot Data.Map.! peer)
    -- diagnostic for sent updates - TODO convert to trace
    --print $ length rval
    return rval
-- TODO write and use the function 'getAdjRibForPeer'

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
    rib' <- readMVar rib
    return (prefixTable rib')

checkPoison :: PeerData -> [PathAttribute] -> [Prefix] -> IO Bool
checkPoison peerData pathAttributes pfxs = return $ elem 666 $ flattenPath $ getASPathContent pathAttributes

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)

    where

    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate{..} rib0 = do
        rib1 <- ribUpdateMany peerData puPathAttributes hash nlri rib0
        -- diagnostic for received updates - TODO convert to trace
        --print (PrefixTableUtils.lengthRIB $ prefixTable rib0 , PrefixTableUtils.lengthRIB $ prefixTable rib1)
        ribWithdrawMany peerData withdrawn rib1

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              poisoned <- checkPoison peerData pathAttributes pfxs
              when poisoned ( putStrLn ( "poisoned route detected " ++ show peerData ++ " " ++ show pfxs))
              let routeData = makeRouteData peerData pathAttributes routeId poisoned
                  ( prefixTable' , updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData
              putStrLn $ "ribUpdateMany: " ++ show updates
              mapM_ (updateRibOutWithPeerData peerData adjRibOutTables) updates
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.withdraw prefixTable pfxs peerData
            -- ** NOTE **
            -- The withdraw entry in the adj-rib-out has a special route value
            -- in future this could be better done as just the withdrawn route with an indicator to distinguish it from a normal one
            -- probably just using Either monad?
            -- updateRibOutWithPeerData peerData nullRoute withdraws adjRibOutTables
            putStrLn "WARNING - ribWithdrawMany incomplete due to change in signature of updateRibOutWithPeerData"
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Bool -> RouteData
    makeRouteData peerData pathAttributes routeId poisoned = RouteData {..}
        where
        pathLength = getASPathLength pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        localPref = if fromEBGP then peerLocalPref peerData else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: PeerData -> AdjRIB -> (PeerData, Int, [Prefix]) -> IO ()
updateRibOutWithPeerData originPeer adjRib (targetPeer, route, prefixes) = do
    putStrLn $ "updateRibOutWithPeerData - target peer: " ++ show targetPeer ++ " routeId:" ++ show route ++ " prefixes:" ++ show prefixes 

{-
    let updateWithKey destinationPeer table = when ( destinationPeer /= originPeer )
                                                   ( insertAdjRIBTable (updates, routeId routeData ) table )
    when ( null updates )
         ( putStrLn $ "null updates in updateRibOutWithPeerData: " ++ show originPeer ++ " / " ++ if 0 == routeId routeData then "nullRoute" else show routeData)
    void $ sequence $ Data.Map.mapWithKey updateWithKey adjRib
-}    