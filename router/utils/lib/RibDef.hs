module RibDef where

import IP4Prefix
import RIBData

type Prefix = IP4Prefix
type Action = (Prefix, Maybe (Peer,Route), Maybe (Peer,Route))
type RibM rib = ([Action],rib)
type RibOp rib = ([Action],rib) -> ([Action],rib)
class Rib rib where
    adjust      :: Prefix -> Peer -> Maybe Route -> rib -> (Action,rib)

    update      :: Prefix -> Peer -> Route -> rib -> (Action,rib)
    update prefix peer route = adjust prefix peer (Just route)

    withdraw    :: Prefix -> Peer -> rib -> (Action,rib)
    withdraw prefix peer = adjust prefix peer Nothing

    removePeer  :: Peer -> rib -> RibM rib
    removePeer_  :: Peer -> rib -> rib
    removePeer_ p r = snd ( removePeer p r)
    lookup      :: Prefix -> rib -> Maybe (Peer,Route)
    mkRib       :: ((Peer,Route) -> (Peer,Route) -> Ordering) -> rib
    getLocRib   :: rib -> ([(Prefix, (Peer,Route))])
    getAdjRibIn :: rib -> [(Prefix, [(Peer,Route)])]

    updateM     :: Prefix -> Peer -> Route -> RibM rib -> RibM rib
    updateM p x y (ax,r) = (a':ax,r') where (a',r') = update p x y r

    withdrawM   :: Prefix -> Peer -> RibM rib ->  RibM rib
    withdrawM p x (ax,r) = (a':ax,r') where (a',r') = withdraw p x r

    removePeerM :: Peer -> RibM rib -> RibM rib
    removePeerM p (ax,r) = (a' ++ ax,r') where (a',r') = removePeer p r

-- below the line:
-- class based fundctions for composing and testing Rib actions

showRibChanges :: [(Prefix, Maybe (Peer, b1), Maybe (Peer, b))] -> String
showRibChanges = unlines . map showRibChange . reverse
showRibChange (prefix, Nothing, Nothing) = "nul " ++ show prefix
showRibChange (prefix, Nothing, Just a) = "Add " ++ show prefix ++ " via " ++ peerName (fst a)
showRibChange (prefix, Just a, Just b) = "Chg " ++ show prefix ++ " via " ++ peerName (fst a) ++ " -> " ++ peerName (fst b)
showRibChange (prefix, Just a, Nothing) = "Del " ++ show prefix ++ " via " ++ peerName (fst a)

-- wanted  - generic definintion for emptyRib
--emptyRib = mkRib compare :: MapRib
--emptyRib' = ([], emptyRib)
buildUpdateSequence peer routes = RibDef.sequence $ map (makeUpdateAction peer) routes where
    makeUpdateAction peer (route,prefix) = update prefix peer route
buildUpdateSequence' peer routes = RibDef.sequence' $ map (makeUpdateAction peer) routes where
    makeUpdateAction peer (route,prefix) = updateM prefix peer route

-- sequence - combine a sequence of rib actions into a single action - 
--            - base version discards the adjRibOut updates, producing and consuming purely a current Rib
--            - prime version also aggregates the adjRibOut updates
--
--            sequence takes the 'M' monadic actions because they lift update and withdraw to equivalnece with peerRemove
--            the base version is useful for benchmarking...

sequence :: Foldable t => t (t1 -> (a, t1)) -> t1 -> t1
sequence fx r = foldl (\b f -> snd $ f b) r fx

sequence' :: [(a, t) -> (a, t)] -> (a, t) -> (a, t)
sequence' fx s = foldl (\ s f -> f s) s fx
