module Global where

import Control.Concurrent
import Network.Socket
import qualified Data.Map.Strict as Data.Map

import BGPRib
import Config
import Collision

data Global = Global { rib :: BGPRib.Rib
                     , peerMap :: Data.Map.Map IPv4 PeerConfig
                     ,collisionDetector :: CollisionDetector
                     ,sessions :: MVar ( Data.Map.Map ThreadId PeerData )
                     ,gd :: GlobalData
                     ,listenAddress :: SockAddr
                     , delayOpenTimer :: Int
                     , initialHoldTimer :: Int
                     , config :: Config
                     }



type FSMExit = ( ThreadId, SockAddr, Either String String )
