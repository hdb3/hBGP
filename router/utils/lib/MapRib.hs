{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module MapRib where
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe(fromMaybe)

import IP4Prefix
import RIBData
import RibDef

data MapRib = MapRib { fSel :: (Peer,Route) -> (Peer,Route) -> Ordering
                                               , locRib :: Map.Map Int (Peer,Route)
                                               , adjRibIn :: Map.Map Int (Map.Map Peer Route) }

instance Show MapRib where
    show mr = "locRib:   { " ++ show ( getLocRib mr ) ++ " } \n" ++
              "adjRibIn: { " ++ show ( getAdjRibIn mr ) ++ " }" 

instance Rib MapRib where

    mkRib cmp = MapRib { fSel = cmp , locRib = Map.empty , adjRibIn = Map.empty }

    lookup prefix rib = Map.lookup (toInt prefix) (locRib rib)

    getLocRib rib = map (\(k,v) -> (fromInt k,v)) $ Map.toAscList (locRib rib)
    getAdjRibIn rib = map (\(k,v) -> (fromInt k,f v)) $ Map.toAscList (adjRibIn rib) where f = Map.toAscList

    removePeer peer rib = (actions,newRib) where
        -- get the populated prefixes for this peer:
        prefixesForPeer = Map.foldrWithKey' f [] (adjRibIn rib) where f k v l = if Map.member peer v then k:l else l

        -- fold over the prefixes with withdraw, accumulating the results and updating the map
        (actions,newRib) = Data.List.foldl' f ([],rib) prefixesForPeer where
            f :: RibM MapRib -> Int -> RibM MapRib
            -- f :: ([(Prefix, (Peer,Route))],MapRib) -> Int -> ([(Prefix, (Peer,Route))],MapRib)
            f (ax,r) i = let pfx = fromInt i -- note this single definition of pfx is needed to allow the following two lines to be unambiguous (and thus to compile)
                                            -- the function signature for 'f', above, though is just for documentation ;-)
                             (a,r') = withdraw pfx peer r
                        in (a:ax,r') 


    adjust prefix' peer route rib = (action,rib') where
        prefix = toInt prefix'
        rib' = MapRib { fSel = fSel rib, locRib = newLocRib, adjRibIn = newAdjRibIn }

        oldLocRib = locRib rib
        oldBestRoute = Map.lookup prefix oldLocRib
        oldAdjRibIn = adjRibIn rib

        -- updating adjRibIn - 

        --    when the prefix is not present then the input target for the new peer value at this prefix is the empty map
        oldPrefixMap = fromMaybe Map.empty ( Map.lookup prefix oldAdjRibIn )

        -- insert function simply overwrites any existing entry - unless it was also the best route we don't care...
        --     because if it is/was best route it will be in locRib too, so no need to worry here
        newPrefixMap = maybe (Map.delete peer oldPrefixMap)
                             (\x -> Map.insert peer x oldPrefixMap)
                             route

        -- similarly, we can simply replace the old one with the new one here....
        -- note (1) even if the operation was delete, this is still insert (replace, really)
        -- note (2) (corrolary of note (1)) if the prefix is now empty of routes, it will still remain in the AdjRibIn
        -- this is fine, as long as no-one uses adjribin for other purposes - it is NOT the same as locRib!!!
        newAdjRibIn = Map.insert prefix newPrefixMap oldAdjRibIn

        -- now check if the new best route has changed for this prefix
        -- first, calculate the new best route:
        adjRibList = Map.toList newPrefixMap
        newBestRoute | null adjRibList = Nothing
                     | otherwise = Just $ Data.List.maximumBy (fSel rib) adjRibList

        -- ** BELOW ALL WRONG!!!!
        -- AFTER WITHDRAW WE CANNOT TELL IF THE ROUTE CHANGED WITHOUT COMPARING OLD AND NEW

        -- there is a subtle point next, regarding comparing the new and old best routes....
        --     if the new best route is to a different peer then it is clearly a change, which must be disseminated
        --     but if the new and old calculated route is from the same peer we would need to know if it is different at the route level
        --     this would require an Eq instance for Route, which we have decalred we don't need...
        --     BUT.... there is an easy way round this - if the new best route is from the peer triggereing this update then 
        --     this is a change, otherwise if the best route is from another peer then it is not - so no need to compare routes, ever....
        -- TLDR summary - check the peer in the calculated new best route - only iff it is the peer given in the update request is it a change of route

        -- in bestRouteChanged if the newBestRoute is empty then the route must have changed due to a withdraw
        -- unless the rib was already empty and this was an erroneous withdraw
        -- however this is still fine since the generated result will simply be (Nothing,Nothing) which is the same as if
        -- the withdraw took out a non selected routed...

        -- further: the route changed when the adjust is an update and the update source is also the source of new best route
        --          - OR - the adjust is a withdraw and the 
        bestRouteChanged = ( newBestRoute /= oldBestRoute )

        newLocRib = if bestRouteChanged then updatedLocRib else oldLocRib where
            updatedLocRib = maybe (Map.delete prefix oldLocRib) ( \newRoute -> Map.insert prefix newRoute oldLocRib) newBestRoute
        --locRib' = if bestRouteChanged then Map.insert prefix newBestRoute (locRib rib) else (locRib rib) 

        -- we are now done building the update Rib values and need to construct the 'result' value which is a '(Maybe (Peer,Route),(Peer,Route))'
        action = if bestRouteChanged then (prefix',oldBestRoute, newBestRoute) else (prefix',Nothing,Nothing)
        -- note, we only need to know now what was the previous best route, so 'locRib' actually is never touched unless it needs to change
        -- also note, as an optimisation (***TODO measure the impact?), the locRib can be updated and queried simultaneously with 'insertLookupWithKey'
        --    (note - 'updateLookupWithKey' does not always return the old value, insertLookupWithKey does).

        -- this is the simple version.....
        -- oldBestRoute = Map.lookup prefix oldLocRib

        -- optimised version: 
        -- require a simpler function than
        -- insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
        -- where the first function parameter 'f' is shown as f key new_value old_value
        --  - so insertLookupWithKey enables a function over a value 'a' to generate the actual new stored value,
        --    whereas we knwo that we just want to specify the new value as an input which will not be modified - so, 
        --    our function is insertLookup :: Ord k => k -> a -> Map k a -> (Maybe a, Map k a), derived from insertLookupWithKey with the function
        --    f key new_value old_value = new_value

        insertLookup :: Ord k => k -> a -> Map.Map k a -> (Maybe a, Map.Map k a)
        insertLookup = Map.insertLookupWithKey f where f _ new_value _ = new_value

        -- and the derived results are just:
        -- *** !!!! (_oldBestRoute,_newLocRib) = insertLookup prefix newBestRoute oldLocRib

        -- but note may need adjustment to allow for the delete(withdraw) action
