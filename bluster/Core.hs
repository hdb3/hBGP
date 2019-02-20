{-# LANGUAGE OverloadedStrings,FlexibleInstances,DeriveGeneric #-}
module Core where

--import Data.Word
import Data.Hashable 
import Data.List(sort)
import GHC.Generics(Generic)

newtype Prefix = Prefix { fromPrefix :: Int } deriving (Eq,Ord,Generic)
instance Show Prefix where
    show = show . fromPrefix

-- this instance is just to allow creating prefixes from literals
instance Num Prefix where
    fromInteger x = Prefix $ fromIntegral x
    (+) = error "You cannot add prefixes!!"
    (-) = error "You cannot subtract prefixes!!"
    (*) = error "You cannot * prefixes!!"
    abs = error "You cannot abs prefixes!!"
    signum = error "You cannot signum prefixes!!"

newtype Hash = Hash { fromHash :: Int } deriving (Eq,Ord,Show,Generic)
type PrefixList = [Prefix]

data Cluster = Cluster { clHash :: Hash
                       --, clPrefixes :: PrefixList -- redundant since the prefix list is also contained in the Basic groups
                       , clCompositeGroups :: [CompositeGroup]
                       , clBasicGroups :: [ BasicGroup ]
                       }
instance Show Cluster where
    show (Cluster _ cgs bgs) = "Cluster: (composite) " ++ show cgs ++ " // (basic) " ++ show bgs

clusterPrefixes :: Cluster -> [Prefix]
clusterPrefixes = concatMap basicPrefixes . clBasicGroups

data BasicGroup = BasicGroup { bgHash :: Hash , basicPrefixes :: PrefixList } deriving (Ord,Generic)
instance Show BasicGroup where
    show = show . basicPrefixes

instance Eq BasicGroup where
    (==) bg1 bg2 = bgHash bg1 == bgHash bg2

data CompositeGroup = CompositeGroup { cgHash :: Hash , compositeGroups :: [ BasicGroup ] } deriving (Ord,Eq,Generic)
instance Show CompositeGroup where
    show = show . compositeGroups

instance {-# INCOHERENT #-} Hashable [Prefix] where
    hashWithSalt s pl = hashWithSalt s $ sort $ map fromPrefix pl

instance Hashable Prefix
instance Hashable CompositeGroup
instance Hashable BasicGroup
instance Hashable Hash

prefixHash :: Prefix -> Hash
prefixHash = Hash . fromPrefix

prefixListHash :: PrefixList -> Hash
prefixListHash pl = Hash $ Data.Hashable.hash $ sort pl

mkBasicGroup :: PrefixList -> BasicGroup
mkBasicGroup pl = BasicGroup (prefixListHash pl) (sort pl)

-- note that the hash value in the CG is dependent ONLY on the underlying prefixes, not the structure
-- this is important when the hash is used as the index/key for lookup in the GroupRib
mkCompositeGroup :: [BasicGroup] -> CompositeGroup
mkCompositeGroup bgs = CompositeGroup (Hash $ Data.Hashable.hash (sort $ concatMap basicPrefixes bgs)) (sort bgs)

mkCluster :: [CompositeGroup] -> [ BasicGroup ] -> Cluster
--mkCluster :: PrefixList -> [CompositeGroup] -> [ BasicGroup ] -> Cluster
mkCluster a b = Cluster (Hash $ Data.Hashable.hash (a,b)) a b

emptyCluster :: Cluster
emptyCluster = mkCluster [] []

mergeClusters :: [Cluster] -> Cluster
mergeClusters = foldl (\(Cluster _ acca accb ) (Cluster _ xa xb ) -> (mkCluster (acca++xa) (accb++xb) )) emptyCluster

mergeCompositeGroups :: [CompositeGroup] -> CompositeGroup
mergeCompositeGroups = mkCompositeGroup . concatMap compositeGroups

emptyCompositeGroup :: CompositeGroup
emptyCompositeGroup = mkCompositeGroup [] 
