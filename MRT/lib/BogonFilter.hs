{-# LANGUAGE OverloadedStrings #-}

module BogonFilter where

import Data.IP
import MRTPrefixes

applyBogonFilter :: [(a, [Prefix])] -> [(a, [Prefix])]
applyBogonFilter = filter p . map f
  where
    f (a, pfxs) = (a, filter bogonFilter pfxs)
    p (_, []) = False
    p _ = True

-- ref https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry-1.csv
bogonFilter :: Prefix -> Bool
bogonFilter pfx
  | "0.0.0.0/8" >:> ip = False
  | "10.0.0.0/8" >:> ip = False
  | "100.64.0.0/10" >:> ip = False
  | "127.0.0.0/8" >:> ip = False
  | "169.254.0.0/16" >:> ip = False
  | "172.16.0.0/12" >:> ip = False
  | "192.0.0.0/24" >:> ip = False
  | "192.0.0.0/29" >:> ip = False
  | "192.0.0.8/32" >:> ip = False
  | "192.0.0.9/32" >:> ip = False
  | "192.0.0.10/32" >:> ip = False
  | "192.0.0.170/32" >:> ip = False
  | "192.0.0.171/32" >:> ip = False
  | "192.0.2.0/24" >:> ip = False
  | "192.31.196.0/24" >:> ip = False
  | "192.52.193.0/24" >:> ip = False
  | "192.88.99.0/24" >:> ip = False
  | "192.168.0.0/16" >:> ip = False
  | "192.175.48.0/24" >:> ip = False
  | "198.18.0.0/15" >:> ip = False
  | "198.51.100.0/24" >:> ip = False
  | "203.0.113.0/24" >:> ip = False
  | "240.0.0.0/4" >:> ip = False
  | "224.0.0.0/4" >:> ip = False
  -- \| 25 < s = False
  | 0 == s = False
  | otherwise = True
  where
    ip = toAddrRange pfx
    s = subnet pfx
