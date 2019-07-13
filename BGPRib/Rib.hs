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
             ( updateRibOutWithPeerData peer nullRoute iprefixes adjRib)
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
            f (rd,ipfxs) = (ipfxs , routeId rd)
            -- make the RIB dump into a Fifo
        aro <- fifo ribDump
            -- TODO - this would be the place to insert an end-of-rib marker
        let adjRib' = Data.Map.insert peer aro adjRib
        return $ Rib' prefixTable adjRib'

lookupRoutes :: Rib -> AdjRIBEntry -> IO [(RouteData,[IPrefix])]
lookupRoutes rib (iprefixes,routeHash) = group_ <$> mapM (\pfx -> (,pfx) <$> adjRibQueryRib rib pfx routeHash) iprefixes

    where
    -- adjRibQueryRib extends queryRib by checking that the current route hash matches the one saved when the AdjRIbOut entry was created
    --    this is useful because if it has changed then probably the correct action is to discard the result
    --    the special case of routeId == 0 is not hadnled differently - this would correspond to a withdraw - it should never occur in a RouteData record
    --    but it could on lookup, in which case the correct behaviour would be to discard the withdraw if the prefix is found
    --    however the caller should not use this function since there is no valid Just value which can represent the withdraw
    --    instead the caller should merely use queryRib and discard the withdraw if the return value is not Nothing
    adjRibQueryRib :: Rib -> IPrefix -> Int -> IO (Maybe RouteData)
    adjRibQueryRib rib iprefix routeHash =
        maybe Nothing (\route -> if routeHash == routeId route then Just route else Nothing) <$> queryRib rib iprefix

    queryRib :: Rib -> IPrefix -> IO (Maybe RouteData)
    queryRib rib prefix = do
        rib' <- readMVar rib
        return $ queryPrefixTable (prefixTable rib') prefix

pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
    (Rib' _ arot) <- readMVar rib
    dequeueAll (arot Data.Map.! peer)
-- TODO write and use the function 'getAdjRibForPeer'


getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
    rib' <- readMVar rib
    return (prefixTable rib')

evalLocalPref :: PeerData -> [PathAttribute] -> [IPrefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)

    where

    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate{..} rib0 = do
        rib1 <- ribUpdateMany peerData puPathAttributes hash nlri rib0
        ribWithdrawMany peerData withdrawn rib1

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [IPrefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              localPref <- evalLocalPref peerData pathAttributes pfxs
              let routeData = makeRouteData peerData pathAttributes routeId localPref
                  ( prefixTable' , updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData
              updateRibOutWithPeerData peerData routeData updates adjRibOutTables
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [IPrefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.withdraw prefixTable pfxs peerData
            -- adjRibOutTables' = updateAdjRibOutTables (updates,0) adjRibOutTables
            -- ** NOTE **
            -- The withdraw entry in the adj-rib-out has a special route value
            -- in future this could be better done as just the withdrawn route with an indicator to distinguish it from a normal one
            -- probably just using Either monad?
            updateRibOutWithPeerData peerData nullRoute withdraws adjRibOutTables
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeId localPref = RouteData {..}
        where
        pathLength = getASPathLength pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

-- NOTE!!!! - we can be called with a null route in which case only the routeId is defined, and is equal 0!!!
-- this is OK since we only get the routeId in this function
updateRibOutWithPeerData :: PeerData -> RouteData -> [IPrefix] -> AdjRIB -> IO ()
updateRibOutWithPeerData originPeer routeData updates adjRib = do
    let updateWithKey destinationPeer table = when ( destinationPeer /= originPeer )
                                                   ( insertAdjRIBTable (updates, routeId routeData ) table )
    when ( null updates )
         ( putStrLn $ "null updates in updateRibOutWithPeerData: " ++ show originPeer ++ " / " ++ if 0 == routeId routeData then "nullRoute" else show routeData)
    void $ sequence $ Data.Map.mapWithKey updateWithKey adjRib
