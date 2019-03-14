{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Config where

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
import BGPlib

data Config = Config { configAS :: Word32
                     , configBGPID :: IPv4
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

activePeers :: Config -> [IPv4]
activePeers config = map peerConfigIPv4 $ filter peerConfigEnableOutbound (configConfiguredPeers config)

activeOnly :: Config -> Bool
activeOnly c = null (configEnabledPeers c) && null ((filter (peerConfigEnableInbound)) (configConfiguredPeers c)) && not (configAllowDynamicPeers c)

data PeerConfig = PeerConfig { peerConfigIPv4 :: IPv4
                             , peerConfigAS ::  Maybe Word32
                             , peerConfigBGPID ::  Maybe IPv4
                             , peerConfigEnableOutbound :: Bool
                             , peerConfigEnableInbound :: Bool
                             , peerConfigOfferedCapabilities :: [ Capability ]
                             , peerConfigRequiredCapabilities :: [ Capability ]
                             }
                             deriving (Eq,Show,Read)

defaultPeerConfig = PeerConfig { peerConfigIPv4 = undefined
                               , peerConfigAS = Nothing
                               , peerConfigBGPID = Nothing
                               , peerConfigEnableOutbound = True
                               , peerConfigEnableInbound = True
                               , peerConfigOfferedCapabilities = [CapAS4 0]
                               , peerConfigRequiredCapabilities = [CapAS4 0]
                               }

dummyPeerConfig = defaultPeerConfig {peerConfigIPv4="127.0.0.1" }

-- expand an input configuration to push all peer definitions into the 'configConfiguredPeers' group
-- at the same time also populate fully the 'configEnabledPeers'
-- and fix up the CapAS4 AS number in configOfferedCapabilities
buildPeerConfigs :: Config -> Config

buildPeerConfigs inConfig = inConfig { configOfferedCapabilities = outConfigOfferedCapabilities
                                     --, configEnabledPeers = outEnabledIPv4s
                                     , configConfiguredPeers = outConfiguredPeers } where
   myAS = configAS inConfig
   outConfigOfferedCapabilities = setAS myAS (configOfferedCapabilities inConfig)
   enabledIPv4s = nub $ configEnabledPeers inConfig -- eliminate any duplicates
   configuredPeers = nub $ configConfiguredPeers inConfig -- eliminate any duplicates -- should ideally also eliminate non-duplicates with equal IPv4s
   configuredIPv4s = map peerConfigIPv4 configuredPeers -- extract the IPs from full configuration
   nonDupEnabledPeers = enabledIPv4s \\ configuredIPv4s -- remove from the enabled list any which also have full configuration
   --outEnabledIPv4s = nonDupEnabledPeers ++ configuredIPv4s
   outConfiguredPeers = map fixAS4 configuredPeers ++ map ( fillConfig inConfig) nonDupEnabledPeers

   fixAS4 :: PeerConfig -> PeerConfig
   fixAS4 pc@PeerConfig{..} = pc { peerConfigOfferedCapabilities = setAS myAS peerConfigOfferedCapabilities }

   -- this is to allow an AS4 requirement to be specified without explicitly writing the local AS into the configuration file again
   -- so - the capability can be written [ CapAS4 0 ] which parses, knowing it will be set right before use...
   -- it is done for both fully and partially configured peers
   -- though a case could be made for leaving the fully configured ones intact...

setAS :: Word32 -> [ Capability ] -> [ Capability ]
setAS as = map (setAS' as) 
setAS' as (CapAS4 _) = CapAS4 as
setAS' _ cap = cap

fillConfig :: Config -> IPv4 -> PeerConfig
fillConfig config ip = defaultPeerConfig { peerConfigIPv4 = ip
                                         , peerConfigOfferedCapabilities = setAS (configAS config) ( configOfferedCapabilities config )
                                         , peerConfigRequiredCapabilities = configRequiredCapabilities config
                                         }

