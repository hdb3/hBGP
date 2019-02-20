{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LocalAddresses where
import Network.Info -- package network-info
import Data.List(sortOn)
import Data.IP

fromNetworkIPv4 (Network.Info.IPv4 hostAddress) = Data.IP.fromHostAddress hostAddress

acceptAddresses :: Data.IP.IPv4 -> Bool
acceptAddresses addr | addr `isMatchedTo` "127.0.0.0/8" = False
                     | addr == "0.0.0.0" = False
                     | addr == "169.254.169.254" = False
                     | otherwise = True

publicAddresses = not . privateAddresses
privateAddresses :: Data.IP.IPv4 -> Bool
privateAddresses addr | addr `isMatchedTo` "10.0.0.0/8" = True
                      | addr `isMatchedTo` "172.16.0.0/12" = True
                      | addr `isMatchedTo` "192.168.0.0/16" = True
                      | otherwise = False

getValidAddress :: IO Data.IP.IPv4
getValidAddress = fmap head getValidAddresses
getValidAddresses = do
    interfaces <- getNetworkInterfaces
    return $ filter acceptAddresses $ map ( fromNetworkIPv4 . Network.Info.ipv4 ) interfaces

getPrivateAddress :: IO Data.IP.IPv4
getPrivateAddress = fmap head getPrivateAddresses
getPrivateAddresses = fmap (filter privateAddresses) getValidAddresses

getPublicAddress :: IO Data.IP.IPv4
getPublicAddress = fmap head getPublicAddresses
getPublicAddresses = fmap (filter publicAddresses) getValidAddresses

preferredInterfaces = ["virbr0"]
preferred int = elem (Network.Info.name int) preferredInterfaces

-- this is a local optimisation - add interface names to the list in preferredInterfaces to skew preference
-- the function excludes invalid addresses and prefers private addresses
getBestAddress :: IO Data.IP.IPv4
getBestAddress = getNetworkInterfaces >>=
    return
    . fromNetworkIPv4
    . Network.Info.ipv4
    . head
    . sortOn ( not . preferred)
    . sortOn ( not . privateAddresses . fromNetworkIPv4 . Network.Info.ipv4 )
    . filter ( acceptAddresses . fromNetworkIPv4 . Network.Info.ipv4)

getAllInterfaces = getNetworkInterfaces >>= return . unlines . map (\NetworkInterface{..} -> name ++ " " ++ show ipv4)
