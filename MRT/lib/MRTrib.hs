{-# LANGUAGE RecordWildCards #-}
module MRTrib ( module MRTlib
              , prefixCountRouteMap,pathCountRouteMap,statsRouteMap,showStatsRouteMap
              , getMRTRibV4,getMRTRibV6,showMRTRibV4,showMRTRibV6
              , getMRTRibs
              , getMRTRibsV6
              , sortRibsOnPrefixCount,sortRibsOnPathCount,fromRouteMapv4
              , Peer, MRTRib,Rib
              , RouteMapv4,RouteMapv6
              , MRTRibV4,MRTRibV6
              ) where

import qualified Data.IntMap.Strict as Map
import Data.Digest.Murmur64
import Data.Array.IArray
import Data.Maybe(fromMaybe)
import Data.List(foldl',reverse,sortOn)

import MRTlib

-- these are really RIB sets, not RIBs - should redfine to allow 'Rib' to be used in a more meaningful way
--     the two variants are required for reasons i do not understand - in some cases the type signature of Rib is rejected on some functions
type Peer a = (PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,[a]))
type Rib a = [(PeerIndex,MRTPeer,Map.IntMap (BGPAttributes,a))]
type MRTRib a = [(PeerIndex, MRTPeer, Map.IntMap (BGPAttributes, [a]))]

type BGPAttributeHash = Int
type PeerMapInput = (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix)
type RouteMapv4 = Map.IntMap (BGPAttributes,IP4PrefixList)
type RouteMapv6 = Map.IntMap (BGPAttributes,IP6PrefixList)
type RouteMap = (RouteMapv4, RouteMapv6)
emptyRouteMap :: RouteMap
emptyRouteMap = (Map.empty,Map.empty)
type PeerMap = Map.IntMap RouteMap
data PeerTableEntry = PT { ptPeer :: MRTPeer, ptRibV4 :: RouteMapv4, ptRibV6 :: RouteMapv6 }
type PeerTable = Array PeerIndex PeerTableEntry
type IPv4PeerTable = Array PeerIndex (MRTPeer,RouteMapv4)

type MRTRibV4 = [(PeerIndex,MRTPeer,RouteMapv4)]
type MRTRibV6 = [(PeerIndex,MRTPeer,RouteMapv6)]

concatRibs :: [[(PeerIndex, b, c)]] -> [(PeerIndex, b, c)]
concatRibs = reverse . snd . rewritePeerIndices . concat
    where
    rewritePeerIndices = foldl (\ (n,ax) (_,a,b) -> (n+1,(n,a,b):ax)) (0,[]) 

type IPv6PeerTable = Array PeerIndex (MRTPeer,RouteMapv6)

data RIBrecord = RIBrecord { rrPrefix :: IPPrefix, rrPeerIndex :: PeerIndex , rrOriginatedTime :: Timestamp , rrAttributes :: BGPAttributes, rrAttributeHash :: BGPAttributeHash } deriving Show

mrtToPeerMap :: [MRTRecord] -> PeerMap
mrtToPeerMap = buildPeerMap . mrtToPeerMapInput
    where

    mrtToPeerMapInput :: [MRTRecord] -> [PeerMapInput]
    mrtToPeerMapInput = concatMap extractPeerMapInput

    extractPeerMapInput :: MRTRecord -> [PeerMapInput]
    extractPeerMapInput = map ribRecordToPeerMapInput . extractRIBrecords
    extractRIBrecords :: MRTRecord -> [RIBrecord]
    extractRIBrecords RIBIPV4Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = IP4Prefix (re4Address,re4Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re4RIB
    extractRIBrecords RIBIPV6Unicast{..} = map (\RIBEntry{..} -> RIBrecord { rrPrefix = IP6Prefix (re6Address,re6Length), rrPeerIndex = rePeerIndex, rrOriginatedTime = reOriginatedTime, rrAttributes = reAttributes, rrAttributeHash = myHash reAttributes }) re6RIB
    extractRIBrecords _ = []
    myHash (BGPAttributes bs) = fromIntegral $ asWord64 $ hash64 bs

    ribRecordToPeerMapInput :: RIBrecord -> PeerMapInput
    ribRecordToPeerMapInput RIBrecord{..} = (rrPeerIndex,rrAttributeHash,rrAttributes,rrPrefix)


    buildPeerMap :: [PeerMapInput] -> PeerMap
    buildPeerMap = foldl' insertPeerMap Map.empty

    insertPeerMap :: PeerMap -> (PeerIndex, BGPAttributeHash,BGPAttributes,IPPrefix) -> PeerMap
    insertPeerMap m (peer,hash,attrs,prefix) = Map.alter (insertRouteMap (hash,attrs,prefix)) (fromIntegral peer) m

    insertRouteMap :: (BGPAttributeHash,BGPAttributes,IPPrefix) -> Maybe RouteMap -> Maybe RouteMap
    insertRouteMap (hash,attrs,prefix) Nothing = insertRouteMap (hash,attrs,prefix) (Just emptyRouteMap)
    insertRouteMap (hash,attrs,IP4Prefix prefix) (Just (rm4,rm6)) = Just (Map.alter (alterRouteMap (attrs,prefix)) hash rm4,rm6)
    insertRouteMap (hash,attrs,IP6Prefix prefix) (Just (rm4,rm6)) = Just (rm4,Map.alter (alterRouteMap (attrs,prefix)) hash rm6)

    alterRouteMap (attrs,prefix) Nothing = Just (attrs,[prefix])
    alterRouteMap (_,prefix) (Just (attrs, prefixes)) = Just (attrs,prefix:prefixes)

--keys = Map.keys

getPeerTable :: [MRTRecord] -> PeerTable
getPeerTable [] = error "getPeerTable requires at least a MRT Peer Table Record" 
getPeerTable (mrt0:mrtx) = buildPeerTable mrt0 (mrtToPeerMap mrtx)
    where
    buildPeerTable :: MRTRecord -> PeerMap -> PeerTable
    buildPeerTable MRTlib.MRTPeerIndexTable{..} peerMap =
        array (0, fromIntegral al)
              [ (fromIntegral i, PT (peerTable !! i) (fst $ peerLookup i) (snd $ peerLookup i)) | i <- [0..al]]
        where
        al = length peerTable - 1
        peerLookup i = fromMaybe emptyRouteMap (Map.lookup (fromIntegral i) peerMap)
    buildPeerTable _ _ = error "buildPeerTable only valid on MRT Peer Index Table records"

prefixCountRouteMap :: Map.IntMap (a, [b]) -> Int
prefixCountRouteMap = sum . map ( length . snd ) . Map.elems

pathCountRouteMap :: Map.IntMap (a, [b]) -> Int
pathCountRouteMap = length

statsRouteMap :: Map.IntMap (a, [b]) -> (Int,Int)
statsRouteMap m = (pathCountRouteMap m, prefixCountRouteMap m)

showStatsRouteMap :: Map.IntMap (a, [b]) -> String
showStatsRouteMap = show . statsRouteMap

makePeerTable :: [a] -> Array PeerIndex a
makePeerTable l = listArray (0,fromIntegral $ length l - 1) l

getMRTRibV4 :: [MRTRecord] -> MRTRibV4
getMRTRibV4 = map (\(a,(b,c))->(a,b,c)) . assocs . getIPv4PeerTable . getPeerTable

getMRTRibs :: [[MRTRecord]] -> MRTRibV4
getMRTRibs = concatRibs . map getMRTRibV4

getMRTRibsV6 :: [[MRTRecord]] -> MRTRibV6
getMRTRibsV6 = concatRibs . map getMRTRibV6

getMRTRibV6 :: [MRTRecord] -> MRTRibV6
getMRTRibV6 = map (\(a,(b,c))->(a,b,c)) . assocs . getIPv6PeerTable . getPeerTable

getIPv4PeerTable :: PeerTable -> IPv4PeerTable
getIPv4PeerTable pt = makePeerTable l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p r4 _) -> (p,r4)) $ elems pt

showMRTRibV4 :: MRTRibV4 -> String
showMRTRibV4 a = "IPv4 peers ("
                      ++ show ( length a )
                      ++ ")\n"
                      ++ unlines ( map showMRTRibV4Entry a)
    where
    showMRTRibV4Entry (i,p,r) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r
 
getIPv6PeerTable :: PeerTable -> IPv6PeerTable
getIPv6PeerTable pt = makePeerTable l where
    l = filter (\(_,r) -> 0 < Map.size r) $ map (\(PT p _ r6) -> (p,r6)) $ elems pt

showMRTRibV6 :: MRTRibV6 -> String
showMRTRibV6 a = "IPv6 peers ("
                      ++ show ( length a )
                      ++ ")\n"
                      ++ unlines ( map showMRTRibV6Entry a)
    where
    showMRTRibV6Entry (i,p,r) = show i ++ " " ++ show p ++ " " ++ showStatsRouteMap r

sortRibsOnPathCount :: MRTRibV4 -> MRTRibV4
sortRibsOnPathCount = sortOn ( (\(_,_,a) -> pathCountRouteMap a))

sortRibsOnPrefixCount :: MRTRibV4 -> MRTRibV4
sortRibsOnPrefixCount = sortOn ( (\(_,_,a) -> prefixCountRouteMap a))

fromRouteMapv4 :: Map.IntMap (BGPAttributes,IP4PrefixList) -> [(BGPAttributes,IP4PrefixList)]
fromRouteMapv4 = Map.elems