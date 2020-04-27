module Router.Global where

import Control.Concurrent
import Network.Socket
import qualified Data.Map.Strict as Data.Map

import BGPRib.BGPRib
import Router.Config
import Router.Collision

data Global = Global { rib :: BGPRib.BGPRib.Rib
                     , peerMap :: Data.Map.Map (IPv4,IPv4) PeerConfig
                     , collisionDetector :: CollisionDetector
                     , sessions :: MVar ( Data.Map.Map ThreadId PeerData )
                     , gd :: GlobalData
                     , listenAddress :: SockAddr
                     , delayOpenTimer :: Int
                     , initialHoldTimer :: Int
                     , config :: Config
                     , logger :: String -> IO ()
                     , exitFlag :: MVar ()
                     , monitorChannel :: Chan (Either PeerData PeerData)
                     , routerName :: String
                     }

type FSMExit = ( ThreadId, SockAddr, Either String String )