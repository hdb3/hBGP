{-#LANGUAGE OverloadedStrings, RecordWildCards #-}
module BGPRib.BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import Data.Word
import Data.Hashable

import BGPlib.BGPlib

data GlobalData = GlobalData { myAS :: Word32
                             , myBGPid :: IPv4
                             -- TODO add a default local address - usually is going to be myBGPid but this might not be routable in some cases
                             }
                            deriving (Show,Eq)

data PeerData = PeerData { globalData :: GlobalData
                         ,  isExternal :: Bool
                         ,  peerAS :: Word32
                         ,  peerBGPid :: IPv4
                         ,  peerIPv4 :: IPv4
                         ,  peerPort :: Word16
                         ,  localIPv4 :: IPv4
                         ,  localPort :: Word16
                         ,  peerLocalPref :: Word32
                         }

data RouteData =  RouteData { peerData :: PeerData
                            , pathAttributes :: [PathAttribute]
                            , routeHash :: Int
                            , pathLength :: Int
                            , nextHop :: IPv4
                            , origin :: Word8
                            , med :: Maybe Word32
                            , fromEBGP :: Bool
                            , localPref :: Word32
                            , originAS :: Word32
                            , lastAS :: Word32
                            , poisoned :: Bool
                            }
                            | Withdraw { peerData :: PeerData }
                            | NullRoute
getRouteNextHop :: RouteData -> Maybe IPv4
getRouteNextHop rd@RouteData{} = Just $ nextHop rd
getRouteNextHop _ = Nothing

routeId :: RouteData -> Int
routeId NullRoute = 0
routeId Withdraw{} = -1
routeId rd@RouteData{} = routeHash rd

nullRoute :: RouteData
nullRoute = RouteData undefined undefined 0 undefined undefined undefined undefined undefined undefined undefined undefined undefined
instance Hashable RouteData where
    hashWithSalt _ = routeHash

localPeer gd = PeerData { globalData = gd
                    , isExternal = False
                    , peerAS  = myAS gd
                    , peerBGPid = myBGPid gd
                    , peerIPv4 = "127.0.0.1"
                    , peerPort  = 0
                    , localIPv4 = "127.0.0.1"
                    , localPort = 0
                    , peerLocalPref = 0
                    }

-- ## TODO - consider an adjunct 'DdummyPeerData' type for safety (where/how is this used?)
dummyPeerData :: PeerData
dummyPeerData = PeerData dummyGlobalData True 64513 "127.0.0.2" "127.0.0.2" 0 "127.0.0.1" 0 0

dummyGlobalData :: GlobalData
dummyGlobalData = GlobalData 64512 "127.0.0.1"

instance Show PeerData where
    show pd = " peer AS: " ++ show (peerAS pd) ++ ",  peer addr: " ++ show (peerIPv4 pd)

instance Show RouteData where
    -- show rd =( if poisoned rd then "P " else "u" ) ++ " nexthop: " ++ show (nextHop rd) ++ ",  peer ID: " ++ show (peerBGPid $ peerData rd) ++ ",  pref " ++ show (localPref rd)
  -- show rd =( if poisoned rd then "P " else "u" ) ++ " nexthop: " ++ show (nextHop rd) ++ ",  peer ID: " ++ show (peerBGPid $ peerData rd) ++ ",  pref " ++ show (localPref rd)
    show rd@RouteData{..} = ( if poisoned then "P " else "u" ) ++ " path: " ++ getASPathList pathAttributes ++ " nexthop: " ++ show nextHop ++ ",  peer ID: " ++ show (peerBGPid peerData) ++ ",  pref " ++ show localPref    -- show rd@RouteData{..} = getASPathList pathAttributes ++ " nexthop: " ++ show nextHop ++ ",  peer ID: " ++ show (peerBGPid peerData) ++ ",  pref " ++ show localPref
    show NullRoute = "NullRoute"
    show Withdraw{} = "Withdraw"

instance Eq RouteData where
    (==) NullRoute NullRoute = True
    (==) (Withdraw a) (Withdraw b) = a == b
    (==) a@RouteData{} b@RouteData{} = routeHash a == routeHash b
    (==) _ _ = False

instance Eq PeerData where
    p1 == p2 = peerBGPid p1 == peerBGPid p2

instance Ord PeerData where
    compare p1 p2 = compare (peerBGPid p1, peerIPv4 p1, peerPort p1) (peerBGPid p2, peerIPv4 p2, peerPort p2)

-- note only defined for case where neither parameter is Withdraw (that constructor should never be found in the wild)
instance Ord RouteData where
  compare rd1@RouteData{} rd2@RouteData{} = compare (localPref rd1, pathLength rd2, origin rd2, fromEBGP rd1, peerBGPid (peerData rd2), peerIPv4 (peerData rd2))
                                                    (localPref rd2, pathLength rd1, origin rd1, fromEBGP rd2, peerBGPid (peerData rd1), peerIPv4 (peerData rd1))

-- rank as higher some parameters when lower - these are Origin, Path Length, peer BGPID, peer address
-- ## TODO ## MED comparison is slightly tricky - only applies when adjacent AS is equal, and needs to accomodate missing Meds in either or both routes
