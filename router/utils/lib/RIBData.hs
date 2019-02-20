{-# LANGUAGE FlexibleInstances #-}
module RIBData where

import Data.Word
import Data.Hashable
import Data.IP(IPv4)
--import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as Map -- IntMap is only marginally faster than base Map

import BGPlib

data Peer = Peer {  peerName :: String
                 ,  isExternal :: Bool
                 ,  peerAS :: Word32
                 ,  peerBGPid :: IPv4
                 ,  peerIPv4 :: IPv4
                 ,  localIPv4 :: IPv4
                 }

data Route =  Route { routeName :: String
                    , routeHash :: Int
                    , localPref :: Word32
                    , pathAttributes :: [PathAttribute]
                    , pathLength :: Int
                    , origin :: Word8
                    , med :: Word32
                    , fromEBGP :: Bool
                    }


makeRoute :: Bool -> [PathAttribute] -> Route
makeRoute fromEBGP attributes = let attributesHash = Data.Hashable.hash attributes in Route {
    routeName = "AS" ++ show (getASPathOrigin attributes) ++ "-" ++ (show (attributesHash `mod` 97)) ,
    routeHash = attributesHash ,
    localPref = getLocalPref attributes ,
    pathAttributes = attributes ,
    pathLength = getASPathLength attributes , 
    origin = getOrigin attributes , 
    med = getMED attributes ,
    fromEBGP = fromEBGP
    }
instance Show Peer where
    show p = "Peer \"" ++ peerName p ++ "\""

instance Show Route where
    show r = "Route \"" ++ routeName r ++ "\""

instance Eq Route where
    r1 == r2 = routeHash r1 == routeHash r2

instance Eq Peer where
    p1 == p2 = peerBGPid p1 == peerBGPid p2

instance Ord Peer where
    compare p1 p2 = compare ( peerBGPid p1 ) ( peerBGPid p2 )

instance {-# OVERLAPPING #-} Ord (Peer,Route) where

  compare (pd1,rd1) (pd2,rd2) = compare (localPref rd1, pathLength rd1, origin rd1, med rd1, not $ fromEBGP rd1, peerBGPid pd1, peerIPv4 pd1)
                                        (localPref rd2, pathLength rd2, origin rd2, med rd2, not $ fromEBGP rd2, peerBGPid pd2, peerIPv4 pd2)

group :: [(Route,a)] -> [(Route,[a])]
group = Map.elems . Map.fromListWith combine . map xIn where
            combine :: (Route,[a]) -> (Route,[a]) -> (Route,[a])
            combine (r1,ax1) (r2,ax2) = (r1,ax1++ax2) -- (++ is faster than unpacking the sigleton using head.....
            xIn :: (Route,a) -> (Int,(Route,[a]))
            xIn (r,a) = (routeHash r, (r,[a]))

ungroup :: [(Route,[a])] -> [(Route,a)]
ungroup = concatMap ungroup' where
                ungroup' :: (Route,[a]) -> [(Route,a)]
                ungroup' (r,ax) = map (\a -> (r,a)) ax
