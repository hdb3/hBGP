{-# LANGUAGE DuplicateRecordFields #-}

module BGPRib.BGPData where

{- peer data holds persistent/static data about a BGP session peer
 - this can be useful for BGP operatins like path selection
-}

import BGPlib.BGPlib
import Data.Hashable
import Data.Word

data GlobalData = GlobalData
  { myAS :: Word32,
    myBGPid :: IPv4
    -- TODO add a default local address - usually is going to be myBGPid but this might not be routable in some cases
  }
  deriving (Show, Eq, Generic)

instance Hashable GlobalData

data PeerData = PeerData
  { globalData :: GlobalData,
    isExternal :: Bool,
    peerAS :: Word32,
    peerBGPid :: IPv4,
    peerIPv4 :: IPv4,
    peerPort :: Word16,
    localIPv4 :: IPv4,
    localPort :: Word16,
    peerLocalPref :: Word32
  }
  deriving (Generic)

instance Hashable PeerData

exportRoute :: RouteData -> RouteExport
exportRoute (Withdraw _) = RouteExportWithdraw --  error "the rib cannot contain an explicit withdraw"
exportRoute NullRoute = RouteExportWithdraw
exportRoute Update {..} = RouteExportUpdate {exportSourcePeer = sourcePeer, exportPath = path}

exportPathID :: RouteExport -> Int
exportPathID RouteExportUpdate {..} = routeHash exportPath
exportPathID RouteExportWithdraw = -1

data Route = Route
  { routePeer :: PeerData,
    routePath :: PathData
  }

data RouteData
  = Update
      { sourcePeer :: PeerData,
        path :: PathData
      }
  | Withdraw PeerData
  | NullRoute

data RouteExport
  = RouteExportUpdate
      { exportSourcePeer :: PeerData,
        exportPath :: PathData
      }
  | RouteExportWithdraw
  deriving (Eq)

data PathData = PathData
  { pathAttributes :: [PathAttribute],
    routeHash :: Int,
    pathLength :: Int,
    nextHop :: IPv4,
    origin :: Word8,
    med :: Maybe Word32,
    fromEBGP :: Bool,
    localPref :: Word32,
    originAS :: Word32,
    lastAS :: Word32
  }

instance Eq PathData where (==) a@PathData {} b@PathData {} = routeHash a == routeHash b

getRouteNextHop :: RouteData -> Maybe IPv4
getRouteNextHop rd@Update {} = Just $ nextHop $ path rd
getRouteNextHop _ = Nothing

routeId :: RouteData -> Int
routeId NullRoute = 0
routeId Withdraw {} = -1
routeId rd@Update {} = routeHash (path rd)

instance Hashable RouteData where
  hashWithSalt _ = routeHash . path

localPeer gd =
  PeerData
    { globalData = gd,
      isExternal = False,
      peerAS = myAS gd,
      peerBGPid = myBGPid gd,
      peerIPv4 = "127.0.0.1",
      peerPort = 0,
      localIPv4 = "127.0.0.1",
      localPort = 0,
      peerLocalPref = 0
    }

-- ## TODO - consider an adjunct 'DdummyPeerData' type for safety (where/how is this used?)
-- (august 2021) - probably the usage is export withdraw 'routes' where there is no peer context.
--               - the prefrred solution in progress is a disticnt data type for this case

dummyPeerData :: PeerData
dummyPeerData = PeerData dummyGlobalData True 64513 "127.0.0.2" "127.0.0.2" 0 "127.0.0.1" 0 0

dummyGlobalData :: GlobalData
dummyGlobalData = GlobalData 64512 "127.0.0.1"

instance Show PeerData where
  show pd = " peer AS: " ++ show (peerAS pd) ++ ",  peer addr: " ++ show (peerIPv4 pd)

-- TDOD make first class instances for Path and derive the Route instances from them
instance Show RouteData where
  show rd@Update {..} = getASPathList (pathAttributes path) ++ " nexthop: " ++ show (nextHop path) ++ ",  peer ID: " ++ show (peerBGPid sourcePeer) ++ ",  pref " ++ show (localPref path)
  show NullRoute = "NullRoute"
  show (Withdraw peer) = "Withdraw " ++ show peer

instance Eq RouteData where
  (==) NullRoute NullRoute = True
  (==) (Withdraw a) (Withdraw b) = a == b
  -- TDOD this looks plainly wrong - the comparison of an Update should compare peers and paths
  -- if program logic requires a path only comparison it should doit explicitly!
  (==) a@Update {} b@Update {} = routeHash (path a) == routeHash (path b)
  (==) _ _ = False

instance Eq PeerData where
  p1 == p2 = peerBGPid p1 == peerBGPid p2

instance Ord PeerData where
  compare p1 p2 = compare (peerBGPid p1) (peerBGPid p2)

-- note only defined for case where neither parameter is Withdraw (that constructor should never be found in the wild)
instance Ord RouteData where
  compare up1@Update {} up2@Update {} =
    let rd1 = path up1; rd2 = path up2; peer1 = sourcePeer up1; peer2 = sourcePeer up2
     in compare
          (localPref rd1, pathLength rd2, origin rd2, fromEBGP rd1, peerBGPid peer2, peerIPv4 peer2)
          (localPref rd2, pathLength rd1, origin rd1, fromEBGP rd2, peerBGPid peer1, peerIPv4 peer1)
  compare _ _ = error "should never order unmatched route sorts"

-- rank as higher some parameters when lower - these are Origin, Path Length, peer BGPID, peer address
-- ## TODO ## MED comparison is slightly tricky - only applies when adjacent AS is equal, and needs to accomodate missing Meds in either or both routes
