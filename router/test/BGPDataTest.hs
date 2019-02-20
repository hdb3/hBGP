{-#LANGUAGE OverloadedStrings #-}
module Main where

import Data.Word
import Data.IP(IPv4)

import RFC4271
import BGPData
import BGPDataTestData

{-
gd1 = GlobalData 65501 "172.16.0.1"
gd2 = GlobalData 65502 "172.16.0.2"

gd1Peer1 = PeerData { globalData = gd1
                    , isExternal = True
                    , peerAS  = 65502
                    , peerBGPid = "172.16.0.2"
                    ,  peerIPv4 = "192.168.0.2"
                    ,  localIPv4 = "192.168.0.1"
                    ,  localPref = 100
                    }

gd2Peer1 = PeerData { globalData = gd2
                    , isExternal = True
                    , peerAS  = 65501
                    , peerBGPid = "172.16.0.1"
                    ,  peerIPv4 = "192.168.0.1"
                    ,  localIPv4 = "192.168.0.2"
                    ,  localPref = 100
                    }

route1 = RouteData { peerData = gd1Peer1
                   , pathLength = 10
                   , origin = _BGP_ORIGIN_IGP
                   , med = 0
                   , fromEBGP = True
                   }

route2 = route1 { peerData = gd2Peer1 }
-}

main = do
    print gd1
    print gd1Peer1
    print route1

    print gd2
    print gd2Peer1
    print route2
