{-# LANGUAGE OverloadedStrings #-}
module Main where

import Capabilities
import Config

main = do
    
    let config = Config { configAS = 200
                        , configBGPID = "192.168.122.1"
                        , configEnabledPeers = [ "192.168.122.236" ]
                        , configConfiguredPeers = [ peerConfig ]
                        , configDelayOpenTimer = 10
                        , configInitialHoldTimer = 300
                        , configAllowDynamicPeers = True
                        , configOfferedCapabilities = [ CapAS4 0 ]
                        , configRequiredCapabilities = [ CapAS4 0 ]
                        , configOfferedHoldTime = 120
                        }

        peerConfig = PeerConfig { peerConfigIPv4 = "192.168.122.60"
                                , peerConfigAS = Just 200
                                , peerConfigBGPID = Just "192.168.122.60"
                                , peerConfigEnableOutbound = True
                                , peerConfigEnableInbound = True
                                , peerConfigOfferedCapabilities = [ CapAS4 0 ]
                                , peerConfigRequiredCapabilities = [ CapAS4 0 ]
                                }

    putStrLn "\nConfig:"
    print config

    let fullConfig = buildPeerConfigs config
    putStrLn "\nFull Config:"
    print fullConfig

    let cfg2 = read configTxt :: Config
    putStrLn "\nConfig2:"
    print cfg2

configTxt =
     "Config {configAS = 200\n\
     \       , configBGPID = 192.168.122.1\n\
     \       , configEnabledPeers = [192.168.122.236]\n\
     \       , configConfiguredPeers = []\n\
     \       , configDelayOpenTimer = 10\n\
     \       , configInitialHoldTimer = 300\n\
     \       , configAllowDynamicPeers = True\n\
     \       , configOfferedCapabilities = [CapAS4 0]\n\
     \       , configRequiredCapabilities = [CapAS4 0]\n\
     \       , configOfferedHoldTime = 120}"
