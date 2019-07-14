{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Router.Config where

-- ## TODO rework the whole Config concept of 'enabled' peers
--         objective - allow concise specification of peers with default attributes
--                     whilst distinguishing active and passive roles
--                   - allow complete flexibility on attributes as required
--                   - reconsider the config defintion (JSON?), and whether
--                     the 'raw' config should be made available 'anonymously' from Global
--                     (possibly yes, so that extensions can use custom configuration
--                      without modifying top level code)

-- define the configuration which is passed to the main program

import Data.List(nub,(\\))
import Data.IP
import Data.Word
import Data.Maybe(fromMaybe)
import BGPlib.BGPlib

data Config = Config { configAS :: Word32
                     , configBGPID :: IPv4
                     , configListenAddress :: IPv4
                     , configEnabledPeers :: [IPv4]
                     , configConfiguredPeers :: [PeerConfig]
                     , configDelayOpenTimer :: Int
                     , configInitialHoldTimer :: Int
                     , configAllowDynamicPeers :: Bool
                     , configEnableDataPlane :: Bool
                     , configEnableRedistribution :: Bool
                     , configTestRoutePath :: String
                     , configTestRouteCount :: Int
                     , configOfferedCapabilities :: [ Capability ]
                     , configRequiredCapabilities :: [ Capability ]
                     , configOfferedHoldTime :: Word16
                     }
                     deriving (Show,Read)

activePeers :: Config -> [(IPv4,IPv4)]
activePeers config = map peerConfigIPv4 $ filter peerConfigEnableOutbound (configConfiguredPeers config)

activeOnly :: Config -> Bool
activeOnly c = null (configEnabledPeers c) && null (filter peerConfigEnableInbound (configConfiguredPeers c)) && not (configAllowDynamicPeers c)

data PeerConfig = PeerConfig { peerConfigIPv4 :: (IPv4,IPv4)
                             , peerConfigAS :: Maybe Word32
                             , peerConfigBGPID :: Maybe IPv4
                             , peerConfigLocalAS :: Maybe Word32
                             , peerConfigLocalBGPID :: Maybe IPv4
                             , peerConfigEnableOutbound :: Bool
                             , peerConfigEnableInbound :: Bool
                             , peerConfigOfferedCapabilities :: [ Capability ]
                             , peerConfigRequiredCapabilities :: [ Capability ]
                             }
                             deriving (Eq,Show,Read)

defaultPeerConfig = PeerConfig { peerConfigIPv4 = undefined
                               , peerConfigAS = Nothing
                               , peerConfigBGPID = Nothing
                               , peerConfigLocalAS = Nothing
                               , peerConfigLocalBGPID = Nothing
                               , peerConfigEnableOutbound = True
                               , peerConfigEnableInbound = True
                               , peerConfigOfferedCapabilities = [CapAS4 0]
                               , peerConfigRequiredCapabilities = [CapAS4 0]
                               }

dummyPeerConfig = defaultPeerConfig {peerConfigIPv4=("0.0.0.0","127.0.0.1") }

-- expand an input configuration to push all peer definitions into the 'configConfiguredPeers' group
-- at the same time fix up the CapAS4 AS number in configOfferedCapabilities
buildPeerConfigs :: Config -> Config

buildPeerConfigs inConfig = inConfig { configOfferedCapabilities = outConfigOfferedCapabilities
                                     , configConfiguredPeers = allPeers } where

   outConfigOfferedCapabilities = setAS (configAS inConfig) (configOfferedCapabilities inConfig)

   allPeers = map (fixAS4 inConfig) ( configConfiguredPeers inConfig ++ constructedPeers )

   constructedPeers = map ( fillConfig inConfig) (enabledIPv4s \\ configuredIPv4s)
       where
       enabledIPv4s = nub $ configEnabledPeers inConfig -- eliminate any duplicates
       configuredIPv4s = nub $ map ( snd . peerConfigIPv4) (configConfiguredPeers inConfig) -- extract the destination IPs from full configuration

   -- this is to allow an AS4 requirement to be specified without explicitly writing the local AS into the configuration file again
   -- so - the capability can be written [ CapAS4 0 ] which parses, knowing it will be set right before use...
   -- it is done for both fully and partially configured peers
   -- though a case could be made for leaving the fully configured ones intact...


   fixAS4 :: Config -> PeerConfig -> PeerConfig
   fixAS4 config pc = pc { peerConfigOfferedCapabilities = setAS localAS $ peerConfigOfferedCapabilities pc }
       where
       localAS = fromMaybe (configAS config) (peerConfigLocalAS pc)

   setAS :: Word32 -> [ Capability ] -> [ Capability ]
   setAS as = map setAS'
       where
       setAS' (CapAS4 _) = CapAS4 as
       setAS' cap = cap


setASfromConfig :: Config -> [ Capability ] -> [ Capability ]
setASfromConfig config = map setAS'
    where
    setAS' (CapAS4 _) = CapAS4 $ configAS config
    setAS' cap = cap

   -- construct complete peer configurations from bare IPs
fillConfig :: Config -> IPv4 -> PeerConfig
fillConfig config ip = defaultPeerConfig { peerConfigIPv4 = ("0.0.0.0",ip)
                                         , peerConfigOfferedCapabilities = setASfromConfig config $ configOfferedCapabilities config
                                         , peerConfigRequiredCapabilities = setASfromConfig config $ configRequiredCapabilities config
                                         }
