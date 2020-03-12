{-# LANGUAGE RecordWildCards, TupleSections, BangPatterns #-}
module BGPRib.Rib(Rib,ribPush,newRib,getLocRib,addPeer,delPeer,getPeersInRib,lookupRoutes,pullAllUpdates,getNextHops) where
import Control.Concurrent
import qualified Data.Map.Strict as Data.Map
import Control.Monad(unless,when,void)
-- TODO replace Data.List with Data.List.EXtra and use nubOrd not nub
import Data.List(intercalate,foldl')
import Data.Word(Word32)
import Data.IP
import Control.Arrow(second)

import BGPlib.BGPlib

import BGPRib.BGPData
import BGPRib.PrefixTable
import qualified BGPRib.PrefixTableUtils as PrefixTableUtils
import BGPRib.Update
import BGPRib.AdjRIBOut
import BGPRib.Common(groupBySecond)

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
        let (prefixTable',prefixes) = withdrawPeer prefixTable peer
        -- schedule the withdraw dissemination
        -- NOTE - this does not change the AdjRIBMap
        updateRibOutWithPeerData peer prefixes adjRib
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
{-
    delayed route look-up logic

    This function performs three functions:
       takes routeIDs and returns full RouteData objects
       checks that the route did not change since the update was originally scheduled
       drops the update for the case where it has changed.


    redux - 
    
    the older versions reapplied 'grouping' to accoodate incosistent treatment of prefixes in a block - whether
    this was ever an issue is not clear - but this insatnce cannot suffer from this becuase updates are either consistent or discarded.
    So, no 'group' function is required.....

    analysis in case of no filter - implication that a changed result is always an indicator for a valid follow on update.
    therefore the simple rule that changed route -> discard is universally applicable

    regrading filters - optimal correct behaviour requires either preserved state from last sent route or recalculated equivalent.
    unsolicited withdraw would allow safe behaviour - but this is only needed in the context of export filter capability, and can easily be implmented
    when that capability is built.
    
    an alternate implementation might simply return the latest route, and mark that prefix/route combination as sent using a sequence number
    then later queued updates can be ignored.  Whether that is better is unclear, but it is more complex, and so not taken up for now.
    A superficial analysis argues tgat defrred transmission is sensible, because updates for churning prefixes would thereby be deferred in favour of stable ones.
-}

lookupRoutes :: Rib -> AdjRIBEntry -> IO (Maybe (RouteData,[Prefix]))
-- TODO make return value no longer a list
{- logic - lookup Route for each prefix
         - discard prefixes whose Routes do not match the given routeHash
         - return a singleton list item with a copy of the matched Route and list of retained prefixes

         - for simplicity generate (Route,prefix) tuples then filter them
         - and use the copy of Route from the first one left, if any. 
 -}
lookupRoutes rib (prefixes,routeHash) = do
        rib' <- readMVar rib
        let -- myLookup (r,pfxs) = (lookUp r, pfxs)
            myLookup = map (\pfx -> ( queryPrefixTable (prefixTable rib') pfx, pfx))
            myFilter = filter ((routeHash ==) . routeId .fst )
            unchangedPrefixes = myFilter $ myLookup prefixes
            route = fst $ head unchangedPrefixes  -- safe becuase only called after test for null
        -- pxrs = map (\prefix -> (queryPrefixTable (prefixTable rib') prefix, prefix)) prefixes
            -- discardChanged (mrd,_) = (\rd -> routeHash == routeId rd) mrd
        -- return $ filter discardChanged pxrs
        return $ if null unchangedPrefixes then Nothing else Just (route,map snd unchangedPrefixes) 

getNextHops :: Rib -> [Prefix] -> IO [(Prefix,Maybe IPv4)]
getNextHops rib prefixes = do
    rib' <- readMVar rib
    return $ map
             (\prefix -> (prefix,) $ getRouteNextHop $ queryPrefixTable (prefixTable rib') prefix)
             prefixes


pullAllUpdates :: PeerData -> Rib -> IO [AdjRIBEntry]
pullAllUpdates peer rib = do
    (Rib' _ arot) <- readMVar rib
    dequeueAll (arot Data.Map.! peer)

-- TODO write and use the function 'getAdjRibForPeer'

getLocRib :: Rib -> IO PrefixTable
getLocRib rib = do
    rib' <- readMVar rib
    return (prefixTable rib')

evalLocalPref :: PeerData -> [PathAttribute] -> [Prefix] -> IO Word32
evalLocalPref peerData pathAttributes pfxs = return (peerLocalPref peerData)

ribPush :: Rib -> PeerData -> ParsedUpdate -> IO()
ribPush rib routeData update = modifyMVar_ rib (ribPush' routeData update)

    where

    ribPush' :: PeerData -> ParsedUpdate -> Rib' -> IO Rib'
    ribPush' peerData ParsedUpdate{..} rib = 
        ribUpdateMany peerData puPathAttributes hash nlri rib >>= ribWithdrawMany peerData withdrawn

    ribUpdateMany :: PeerData -> [PathAttribute] -> Int -> [Prefix] -> Rib' -> IO Rib'
    ribUpdateMany peerData pathAttributes routeId pfxs (Rib' prefixTable adjRibOutTables )
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
              localPref <- evalLocalPref peerData pathAttributes pfxs
              let routeData = makeRouteData peerData pathAttributes routeId localPref
                  ( !prefixTable' , !updates ) = BGPRib.PrefixTable.update prefixTable pfxs routeData
              updateRibOutWithPeerData peerData updates adjRibOutTables
              return $ Rib' prefixTable' adjRibOutTables

    ribWithdrawMany :: PeerData -> [Prefix] -> Rib' -> IO Rib'
    ribWithdrawMany peerData pfxs (Rib' prefixTable adjRibOutTables)
        | null pfxs = return (Rib' prefixTable adjRibOutTables )
        | otherwise = do
            let ( prefixTable' , withdraws ) = BGPRib.PrefixTable.withdraw prefixTable pfxs peerData
            updateRibOutWithPeerData peerData withdraws adjRibOutTables
            return $ Rib' prefixTable' adjRibOutTables

    makeRouteData :: PeerData -> [PathAttribute] -> Int -> Word32 -> RouteData
    makeRouteData peerData pathAttributes routeHash overrideLocalPref = RouteData {..}
        where
        (pathLength, originAS, lastAS) = getASPathDetail pathAttributes
        fromEBGP = isExternal peerData
        med = if fromEBGP then 0 else getMED pathAttributes
        localPref = if fromEBGP then overrideLocalPref else getLocalPref pathAttributes
        nextHop = getNextHop pathAttributes
        origin = getOrigin pathAttributes

updateRibOutWithPeerData :: PeerData -> [(Prefix,RouteData)] -> AdjRIB -> IO ()
updateRibOutWithPeerData _ updates adjRIB = sequence_ $ Data.Map.map action adjRIB

-- reminder: type AdjRIB = Data.Map.Map PeerData AdjRIBTable
--         : insertNAdjRIBTable :: [([Prefix],Int)] -> AdjRIBTable -> IO ()
--
--         - applying an action to every AdjRIBTable in an 'AdjRIB' is achived by
--         -    sequence_ $ Data.Map.map action
--         - where 'action' is  AdjRIBTable -> IO ()
--         - If the requirement is to execute an action which is sependent upon the peer context then
--         -    sequence_ $ Data.Map.mapWithKey action'
--         - is required, where 'action'' is PeerData -> AdjRIBTable -> IO ()

-- this is a simple wrapper / rearrangement function
-- insertNAdjRIBTable consumes type of form [( [Prefix], Int )]
-- whilst the input type is [(Prefix,RouteData)]

-- returning routes to the originator may seem unprofitable
-- however real routers do exactly this (e.g. Cisco IOS) and it may simplify matters to follow suit...
-- Especially considering that the alternative is to issue withdrawals with no material reduction in complexity on either side

-- there would an ordering issue if applying a filter against returned routes to the origin:
-- But we don't care and so the operation is very simple....

    where 
        action :: AdjRIBTable -> IO ()
        action = insertNAdjRIBTable (f updates)
        f0 :: [(Prefix,RouteData)] -> [(Prefix,Int)]
        f0 = map (second routeId)
        f1 :: [(Prefix,Int)] -> [([Prefix],Int)]
        f1 = groupBySecond
        f :: [(Prefix,RouteData)] -> [([Prefix],Int)]
        f = f1 . f0
