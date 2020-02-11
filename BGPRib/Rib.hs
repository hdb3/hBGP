{-# LANGUAGE RecordWildCards, TupleSections #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates,getNextHops) where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Control.Monad(unless,when,void)
import Data.List(intercalate)
import Data.Maybe(fromJust)
import Data.Word(Word32)
import Data.IP

import BGPlib.BGPlib

import BGPRib.BGPData
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPRib.Update
import BGPRib.AdjRIBOut
import BGPRib.Common(group)

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
        -- # let (prefixTable',prefixes) = withdrawPeer prefixTable peer
        -- schedule the withdraw dissemination
        -- NOTE - this does not change the AdjRIBMap
        -- # unless (null prefixes)
        -- #     ( updateRibOutWithPeerData peer nullRoute prefixes adjRib)
        let prefixes = getPeerPrefixes prefixTable peer
            (prefixTable',withdraws) = BGPRib.PrefixTable.update prefixTable prefixes peer Nothing
        putStrLn "*** delPeer UNIT TEST remove when confirmed! ***"
        print (peer,prefixes)
        putStrLn "*** delPeer UNIT TEST remove when confirmed! ***"

        mapM_ (updatePeer adjRib) withdraws
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
{-
    delayed route look-up logic
    After an update/withdraw is scheduled the best route may change, or be removed, leaving no route at all.
    This raises two issues: firstly, the original route may no longer be available, so the immediate update cannot be sent.
    Secondly, even if the route is still valid, or a withdraw was requested, the optimal behaviour may not be to send the originally selected route.
    The simple course of action is to discard tbe immediate update, safe in the knowledge that another update for the specific peer is in the pipeline.
    However, the logic fails when the request is withdraw, since it is possible that the replacement route would be filtered for the specific target peer.
    Therefore scheduled withdraw should not be discarded.  A similar problem arises when the current route is null, in which case a withdraw is in the pipeline,
    but if we discard the earlier route and it was also the first route selected for this prefix/peer then a withdraw will be generated without a matching update.
    This outcome is likely to be rare, and does not compromise route integrity, although a peer may detect the anomaly.
    A full resoultion is not possible in this design - it motivates a change from use of route hashes to actual route referneces in ARO entries.

    Summary: withdrawals are never discarded, changed non-null routes are...

    New design

    lookupRoutes should return [(Maybe RouteData,[Prefix])] rather than [(RouteData,[Prefix])],
    where Maybe RouteData allows a withdraw to be represented.  The prescribed filter logic is implemented after initial lookup and before function return.
    The initial lookup function returns an ARE record (tuple) augmented with the lookup outcome.
    Before grouping on the lookup outcome the list is filtered according to the description above. 
-}

lookupRoutes :: Rib -> AdjRIBEntry -> IO [(Maybe RouteData,[Prefix])]
lookupRoutes rib (prefixes,routeHash) = if 0 == routeHash
    then return [(Nothing,prefixes)] 
    else do
        rib' <- readMVar rib
        let pxrs = map (\prefix -> (queryPrefixTable (prefixTable rib') prefix, prefix)) prefixes
            discardChanged (mrd,_) = maybe True (\rd -> routeHash == routeId rd) mrd
        return $ group $ filter discardChanged pxrs

getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    let getNextHop prefix = (prefix,) $ nextHop <$> queryPrefixTable (prefixTable rib') prefix
    return $ map getNextHop prefixes

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
    ribPush' peerData ParsedUpdate{..} rib0 =
        -- # rib1 <- ribUpdateMany peerData puPathAttributes hash nlri rib0
        -- diagnostic for received updates - TODO convert to trace
        --print (PrefixTableUtils.lengthRIB $ prefixTable rib0 , PrefixTableUtils.lengthRIB $ prefixTable rib1)
        -- # ribWithdrawMany peerData withdrawn rib1
        ribUpdateMany peerData puPathAttributes hash nlri rib0 >>= ribWithdrawMany peerData withdrawn

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              poisoned <- checkPoison peerData pathAttributes pfxs
              when poisoned ( putStrLn ( "poisoned route detected " ++ show peerData ++ " " ++ show pfxs))
              let routeData = makeRouteData peerData pathAttributes routeId 100 poisoned
                  ( prefixTable' , updates ) = BGPRib.PrefixTable.update prefixTable pfxs peerData (Just routeData)
              putStrLn $ "ribUpdateMany: " ++ show updates
              mapM_ (updatePeer adjRibOutTables) updates
              putStrLn "*** ribUpdateMany UNIT TEST remove when confirmed! ***"
              print (peerData, updates)
              putStrLn "*** ribUpdateMany UNIT TEST remove when confirmed! ***"
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.update prefixTable pfxs peerData Nothing
            mapM_ (updatePeer adjRibOutTables) withdraws
            putStrLn "*** ribWithdrawMany UNIT TEST remove when confirmed! ***"
            print (peerData, withdraws)
            putStrLn "*** ribWithdrawMany UNIT TEST remove when confirmed! ***"
            -- ** NOTE **
            -- The withdraw entry in the adj-rib-out has a special route value
            -- in future this could be better done as just the withdrawn route with an indicator to distinguish it from a normal one
            -- probably just using Either monad?
            -- updateRibOutWithPeerData peerData nullRoute withdraws adjRibOutTables
            -- putStrLn "WARNING - ribWithdrawMany incomplete due to change in signature of updateRibOutWithPeerData"
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> Bool -> RouteData
    makeRouteData peerData pathAttributes routeId overrideLocalPref poisoned = RouteData {..}
        where
        pathLength = getASPathLength pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updatePeer :: AdjRIB -> (PeerData, Int, [Prefix]) -> IO ()
updatePeer adjRib (peer,routeHash,prefixes) = insertAdjRIBTable (prefixes, routeHash ) (fromJust $ Data.Map.lookup peer adjRib)


-- NOTE!!!! - we can be called with a null route in which case only the routeId is defined, and is equal 0!!!
-- this is OK since we only get the routeId in this function
updateRibOutWithPeerData :: PeerData -> RouteData -> [Prefix] -> AdjRIB -> IO ()
updateRibOutWithPeerData originPeer routeData updates adjRib = do
    let updateWithKey destinationPeer table = when ( destinationPeer /= originPeer )
                                                   ( insertAdjRIBTable (updates, routeId routeData ) table )
    when ( null updates )
         ( putStrLn $ "null updates in updateRibOutWithPeerData: " ++ show originPeer ++ " / " ++ if 0 == routeId routeData then "nullRoute" else show routeData)
    sequence_ $ Data.Map.mapWithKey updateWithKey adjRib


{-
updateRibOutWithPeerData :: PeerData -> AdjRIB -> (PeerData, Int, [Prefix]) -> IO ()
updateRibOutWithPeerData originPeer adjRib (targetPeer, route, prefixes) = do
    putStrLn $ "updateRibOutWithPeerData - target peer: " ++ show targetPeer ++ " routeId:" ++ show route ++ " prefixes:" ++ show prefixes 


    let updateWithKey destinationPeer table = when ( destinationPeer /= originPeer )
                                                   ( insertAdjRIBTable (updates, routeId routeData ) table )
    when ( null updates )
         ( putStrLn $ "null updates in updateRibOutWithPeerData: " ++ show originPeer ++ " / " ++ if 0 == routeId routeData then "nullRoute" else show routeData)
    sequence_ $ Data.Map.mapWithKey updateWithKey adjRib
-}
