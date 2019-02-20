{-#LANGUAGE OverloadedStrings #-}
module BGPDataTestData where

import Data.Word
import Data.IP(IPv4)

import RFC4271
import BGPData
import Capabilities

gd = GlobalData 65501 "172.16.0.254"
local = localPeer gd
internalPeer = local {
                       localIPv4 = "192.168.0.1"
                    ,  peerIPv4  = "192.168.0.2"
                    ,  peerBGPid = "192.168.0.2"
                    ,  localPref = 100
                    }

externalPeer = local {
                       peerAS = 65502
                    ,  isExternal = True
                    ,  localIPv4 = "192.168.0.1"
                    ,  peerIPv4  = "192.168.0.3"
                    ,  peerBGPid = "192.168.0.3"
                    ,  localPref = 100
                    }


route1 = RouteData { peerData = internalPeer
                   , pathAttributes = []
                   , routeId = 99
                   , nextHop = "1.2.3.4"
                   , pathLength = 10
                   , origin = _BGP_ORIGIN_IGP
                   , med = 0
                   , fromEBGP = True
                   }
route2 = route1 { peerData = externalPeer
                   , routeId = 100
                   , nextHop = "1.2.3.5"
                   , pathLength = 11
                  } 
-- gd1Peer2Route1 = gd1Peer1Route1 { peerData = gd2Peer2 }
-- gd1Peer1Route2 = gd1Peer1Route1 { pathLength = 11 }
-- gd1Peer1Route3 = gd1Peer1Route1 { pathLength = 12 }
-- gd1Peer2Route2 = gd1Peer1Route2 { peerData = gd2Peer2 }

peer1 = internalPeer
peer2 = externalPeer
route11 = route1
route12 = route1 { pathLength = 11 }
route13 = route1 { pathLength = 12 }
route21 = route11 { peerData = peer2 }
route22 = route12 { peerData = peer2 }
route23 = route13 { peerData = peer2 }

route peer pl = route11 { peerData = peer, pathLength = pl }
