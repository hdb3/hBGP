{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import BGPlib.BGPlib
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as T
import Data.Yaml
import Router.Config
import System.Exit (die)
import Text.Pretty.Simple (pShow)
import Text.RawString.QQ

main = do
  putStrLn "\nConfig:"
  T.putStr $ pShow config

  putStrLn "\nYAML:\n>>>>>>>"
  B.putStr $ encode config
  putStrLn "<<<<<<<\nend YAML:\n"

  putStr "decode yamlCfg: "
  putStrLn $
    either
      (\errMsg -> "failed decode" ++ show errMsg)
      (\decoded -> if decoded == config then "success" else "failed comparison")
      (decodeEither' yamlCfg :: Either ParseException Config)

  putStr "decode jsonCfg: "
  putStrLn $
    either
      (\errMsg -> "failed decode" ++ show errMsg)
      (\decoded -> if decoded == config then "success" else "failed comparison")
      (decodeEither' jsonCfg :: Either ParseException Config)

{-

sample literal and YAML forms which are expected to align

-}

config =
  Config
    { configAS = 200,
      configBGPID = "192.168.122.1",
      configListenAddress = "192.168.122.1",
      configEnabledPeers = ["192.168.122.236"],
      configConfiguredPeers = [peerConfig],
      configDelayOpenTimer = 10,
      configInitialHoldTimer = 300,
      configAllowDynamicPeers = True,
      configEnableDataPlane = False,
      configEnableRedistribution = False,
      configTestRoutePath = "",
      configTestRouteCount = 0,
      configOfferedCapabilities = [CapAS4 0, CapMultiprotocol 1 1],
      configRequiredCapabilities = [CapAS4 0],
      configOfferedHoldTime = 120
    }

peerConfig =
  PeerConfig
    { peerConfigIPv4 = ("192.168.122.60", "192.168.122.1"),
      peerConfigAS = Just 200,
      peerConfigBGPID = Just "192.168.122.60",
      peerConfigLocalAS = Nothing,
      peerConfigLocalBGPID = Nothing,
      peerConfigEnableOutbound = True,
      peerConfigEnableInbound = True,
      peerConfigOfferedCapabilities = [CapAS4 0, CapMultiprotocol 1 1],
      peerConfigRequiredCapabilities = [CapAS4 0],
      peerConfigLocalPref = 100
    }

-- Note: _layout_ of this embedded example is different from the default YAML generated above
yamlCfg =
  [r|configAS: 200
configAllowDynamicPeers: true
configBGPID: 192.168.122.1
configConfiguredPeers:
- peerConfigAS: 200
  peerConfigBGPID: 192.168.122.60
  peerConfigEnableInbound: true
  peerConfigEnableOutbound: true
  peerConfigIPv4:
  - 192.168.122.60
  - 192.168.122.1
  peerConfigLocalAS: null
  peerConfigLocalBGPID: null
  peerConfigLocalPref: 100
  peerConfigOfferedCapabilities:
  - tag: CapAS4
    contents: 0
  - tag: CapMultiprotocol
    contents:
    - 1
    - 1
  peerConfigRequiredCapabilities:
  - tag: CapAS4
    contents: 0
configDelayOpenTimer: 10
configEnableDataPlane: false
configEnableRedistribution: false
configEnabledPeers:
- 192.168.122.236
configInitialHoldTimer: 300
configListenAddress: 192.168.122.1
configOfferedCapabilities:
- tag: CapAS4
  contents: 0
- tag: CapMultiprotocol
  contents:
  - 1
  - 1
configOfferedHoldTime: 120
configRequiredCapabilities:
- tag: CapAS4
  contents: 0
configTestRouteCount: 0
configTestRoutePath: ''
|]

jsonCfg =
  [r|{
  "configAS": 200,
  "configAllowDynamicPeers": true,
  "configBGPID": "192.168.122.1",
  "configConfiguredPeers": [
    {
      "peerConfigAS": 200,
      "peerConfigBGPID": "192.168.122.60",
      "peerConfigEnableInbound": true,
      "peerConfigEnableOutbound": true,
      "peerConfigIPv4": [
        "192.168.122.60",
        "192.168.122.1"
      ],
      "peerConfigLocalAS": null,
      "peerConfigLocalBGPID": null,
      "peerConfigLocalPref": 100,
      "peerConfigOfferedCapabilities": [
        {
          "contents": 0,
          "tag": "CapAS4"
        },
        {
          "contents": [
            1,
            1
          ],
          "tag": "CapMultiprotocol"
        }
      ],
      "peerConfigRequiredCapabilities": [
        {
          "contents": 0,
          "tag": "CapAS4"
        }
      ]
    }
  ],
  "configDelayOpenTimer": 10,
  "configEnableDataPlane": false,
  "configEnableRedistribution": false,
  "configEnabledPeers": [
    "192.168.122.236"
  ],
  "configInitialHoldTimer": 300,
  "configListenAddress": "192.168.122.1",
  "configOfferedCapabilities": [
    {
      "contents": 0,
      "tag": "CapAS4"
    },
    {
      "contents": [
        1,
        1
      ],
      "tag": "CapMultiprotocol"
    }
  ],
  "configOfferedHoldTime": 120,
  "configRequiredCapabilities": [
    {
      "contents": 0,
      "tag": "CapAS4"
    }
  ],
  "configTestRouteCount": 0,
  "configTestRoutePath": ""
}|]
