{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Router.Config where

-- ## TODO rework the whole Config concept of 'enabled' peers
--         objective - allow concise specification of peers with default attributes
--                     whilst distinguishing active and passive roles
--                   - allow complete flexibility on attributes as required
--                   - reconsider the config definition (JSON?), and whether
--                     the 'raw' config should be made available 'anonymously' from Global
--                     (possibly yes, so that extensions can use custom configuration
--                      without modifying top level code)

-- ## TODO refactor offered/required to required/optional -
--    the 'offer' consists of both, the response check consists of merely required

-- define the configuration which is passed to the main program

import BGPlib.BGPlib
import Control.Applicative (Alternative (empty))
import Control.Monad (unless)
import Data.Aeson
import Data.List (foldl', nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Word
import Router.Log
import System.Exit (die)
import Text.Read (readMaybe)

data Config = Config
  { configAS :: Word32,
    configBGPID :: IPv4,
    configListenAddress :: IPv4,
    configEnabledPeers :: [IPv4],
    configConfiguredPeers :: [PeerConfig],
    configDelayOpenTimer :: Int,
    configInitialHoldTimer :: Int,
    configAllowDynamicPeers :: Bool,
    configEnableDataPlane :: Bool,
    configEnableRedistribution :: Bool,
    configTestRoutePath :: String,
    configTestRouteCount :: Int,
    configOfferedCapabilities :: [Capability],
    configRequiredCapabilities :: [Capability],
    configOfferedHoldTime :: Word16
  }
  deriving (Generic, Show, Read, Eq)

instance ToJSON PeerConfig

instance FromJSON PeerConfig

instance ToJSON Config

instance FromJSON Config

instance ToJSON IPv4 where
  toJSON ipv4 = String $ pack $ show ipv4

instance FromJSON IPv4 where
  parseJSON (String s) = do
    let ipv4 = readMaybe (unpack s) :: Maybe IPv4
    maybe
      empty
      return
      ipv4
  parseJSON _ = empty

-- TODO take all of this capability munging out of IO
-- It should either return an Either or just error.....
-- dying in situ is not any better than an error

checkCapabilities :: Config -> IO Config
checkCapabilities c@Config {..} = do
  unless
    (check configOfferedCapabilities configRequiredCapabilities)
    (die "check configOfferedCapabilities configRequiredCapabilities")
  mapM_ checkPeerCapabilities configConfiguredPeers
  return c
  where
    -- offered must include all of required

    check offered required = null (capCodes required \\ capCodes offered)
    checkPeerCapabilities PeerConfig {..} =
      unless
        (check peerConfigOfferedCapabilities peerConfigRequiredCapabilities)
        ( die $
            "peer "
              ++ show peerConfigBGPID
              ++ " check peerConfigOfferedCapabilities peerConfigRequiredCapabilities"
        )

fixCapabilities :: Config -> IO Config
-- warn and fix the following cases:
--   ADDPATH code without addpath capability in offered and required
--   non-ADDPATH code with addpath capability in offered or required

-- fixCapabilities = fixCapabilitiesBase

fixCapabilities = fixCapabilitiesAddPath

fixCapabilitiesBase :: Config -> IO Config
fixCapabilitiesBase = removeCapabilities _CapCodeAddPath

fixCapabilitiesAddPath :: Config -> IO Config
fixCapabilitiesAddPath = addCapabilities (CapAddPath 1 1 3)

addCapabilities :: Capability -> Config -> IO Config
addCapabilities cap c@Config {..} = do
  configOfferedCapabilities' <- addCaps "in configOfferedCapabilities " cap configOfferedCapabilities
  configRequiredCapabilities' <- addCaps "in configRequiredCapabilities " cap configRequiredCapabilities
  configConfiguredPeers' <- mapM (addPeerCapabilities cap) configConfiguredPeers
  return $
    c
      { configOfferedCapabilities = configOfferedCapabilities',
        configRequiredCapabilities = configRequiredCapabilities',
        configConfiguredPeers = configConfiguredPeers'
      }

addPeerCapabilities :: Capability -> PeerConfig -> IO PeerConfig
addPeerCapabilities cap p@PeerConfig {..} = do
  peerConfigOfferedCapabilities' <- addCaps "in peerConfigOfferedCapabilities " cap peerConfigOfferedCapabilities
  peerConfigRequiredCapabilities' <- addCaps "in peerConfigRequiredCapabilities " cap peerConfigRequiredCapabilities
  return $
    p
      { peerConfigOfferedCapabilities = peerConfigOfferedCapabilities',
        peerConfigRequiredCapabilities = peerConfigRequiredCapabilities'
      }

addCaps :: String -> Capability -> [Capability] -> IO [Capability]
addCaps s cap caps =
  let hasCap :: Capability -> [Capability] -> Bool
      hasCap cap = foldl' (\p x -> p || eq_ cap x) False
   in if hasCap cap caps
        then return caps
        else do
          info $ "added capability" ++ show cap
          return (cap : caps)

removeCapabilities :: CapCode -> Config -> IO Config
removeCapabilities cc c@Config {..} = do
  configOfferedCapabilities' <- removeCaps "in configOfferedCapabilities " cc configOfferedCapabilities
  configRequiredCapabilities' <- removeCaps "in configRequiredCapabilities " cc configRequiredCapabilities
  configConfiguredPeers' <- mapM (removePeerCapabilities cc) configConfiguredPeers
  return $
    c
      { configOfferedCapabilities = configOfferedCapabilities',
        configRequiredCapabilities = configRequiredCapabilities',
        configConfiguredPeers = configConfiguredPeers'
      }

removePeerCapabilities :: CapCode -> PeerConfig -> IO PeerConfig
removePeerCapabilities cc p@PeerConfig {..} = do
  peerConfigOfferedCapabilities' <- removeCaps "in peerConfigOfferedCapabilities " cc peerConfigOfferedCapabilities
  peerConfigRequiredCapabilities' <- removeCaps "in peerConfigRequiredCapabilities " cc peerConfigRequiredCapabilities
  return $
    p
      { peerConfigOfferedCapabilities = peerConfigOfferedCapabilities',
        peerConfigRequiredCapabilities = peerConfigRequiredCapabilities'
      }

removeCaps :: String -> CapCode -> [Capability] -> IO [Capability]
removeCaps s cc caps = do
  unless
    (rval == caps)
    (info $ "removed capability" ++ show (caps \\ rval) ++ " - " ++ s)
  return rval
  where
    rval = removeCaps_ cc caps
    removeCaps_ cc = filter ((cc /=) . capCode)

activePeers :: Config -> [(IPv4, IPv4)]
activePeers config = map peerConfigIPv4 $ filter peerConfigEnableOutbound (configConfiguredPeers config)

activeOnly :: Config -> Bool
activeOnly c = null (configEnabledPeers c) && not (any peerConfigEnableInbound (configConfiguredPeers c)) && not (configAllowDynamicPeers c)

data PeerConfig = PeerConfig
  { peerConfigIPv4 :: (IPv4, IPv4),
    peerConfigAS :: Maybe Word32,
    peerConfigBGPID :: Maybe IPv4,
    peerConfigLocalAS :: Maybe Word32,
    peerConfigLocalBGPID :: Maybe IPv4,
    peerConfigEnableOutbound :: Bool,
    peerConfigEnableInbound :: Bool,
    peerConfigOfferedCapabilities :: [Capability],
    peerConfigRequiredCapabilities :: [Capability],
    peerConfigLocalPref :: Word32
  }
  deriving (Generic, Eq, Show, Read)

defaultPeerConfig =
  PeerConfig
    { peerConfigIPv4 = undefined,
      peerConfigAS = Nothing,
      peerConfigBGPID = Nothing,
      peerConfigLocalAS = Nothing,
      peerConfigLocalBGPID = Nothing,
      peerConfigEnableOutbound = True,
      peerConfigEnableInbound = True,
      peerConfigOfferedCapabilities = [CapAS4 0],
      peerConfigRequiredCapabilities = [CapAS4 0],
      peerConfigLocalPref = 100
    }

dummyPeerConfig = defaultPeerConfig {peerConfigIPv4 = ("0.0.0.0", "127.0.0.1")}

-- expand an input configuration to push all peer definitions into the 'configConfiguredPeers' group
-- at the same time fix up the CapAS4 AS number in configOfferedCapabilities
buildPeerConfigs :: Config -> Config
buildPeerConfigs inConfig =
  inConfig
    { configOfferedCapabilities = outConfigOfferedCapabilities,
      configConfiguredPeers = allPeers
    }
  where
    outConfigOfferedCapabilities = setAS (configAS inConfig) (configOfferedCapabilities inConfig)

    allPeers = map (fixAS4 inConfig) (configConfiguredPeers inConfig ++ constructedPeers)

    constructedPeers = map (fillConfig inConfig) (enabledIPv4s \\ configuredIPv4s)
      where
        enabledIPv4s = nub $ configEnabledPeers inConfig -- eliminate any duplicates
        configuredIPv4s = nub $ map (snd . peerConfigIPv4) (configConfiguredPeers inConfig) -- extract the destination IPs from full configuration

    -- this is to allow an AS4 requirement to be specified without explicitly writing the local AS into the configuration file again
    -- so - the capability can be written [ CapAS4 0 ] which parses, knowing it will be set right before use...
    -- it is done for both fully and partially configured peers
    -- though a case could be made for leaving the fully configured ones intact...

    fixAS4 :: Config -> PeerConfig -> PeerConfig
    fixAS4 config pc = pc {peerConfigOfferedCapabilities = setAS localAS $ peerConfigOfferedCapabilities pc}
      where
        localAS = fromMaybe (configAS config) (peerConfigLocalAS pc)

    setAS :: Word32 -> [Capability] -> [Capability]
    setAS as = map setAS'
      where
        setAS' (CapAS4 _) = CapAS4 as
        setAS' cap = cap

setASfromConfig :: Config -> [Capability] -> [Capability]
setASfromConfig config = map setAS'
  where
    setAS' (CapAS4 _) = CapAS4 $ configAS config
    setAS' cap = cap

-- construct complete peer configurations from bare IPs
fillConfig :: Config -> IPv4 -> PeerConfig
fillConfig config ip =
  defaultPeerConfig
    { peerConfigIPv4 = ("0.0.0.0", ip),
      peerConfigOfferedCapabilities = setASfromConfig config $ configOfferedCapabilities config,
      peerConfigRequiredCapabilities = setASfromConfig config $ configRequiredCapabilities config
    }
