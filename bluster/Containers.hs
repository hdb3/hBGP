module Containers where
import qualified Data.IntMap.Strict as Map
import Data.List(foldl')

import Core

lookup :: Hash -> Map.IntMap a -> Maybe a
lookup = Map.lookup . fromHash

elems = Map.elems
keys = Map.keys
--fromList = Map.fromList

type ClusterList = Map.IntMap Cluster
type PrefixRib = Map.IntMap Hash
type GroupRib = Map.IntMap CompositeGroup

emptyPrefixRib :: PrefixRib
emptyPrefixRib = Map.empty

insertPrefixRib :: (Prefix,Hash) -> PrefixRib -> PrefixRib
insertPrefixRib _ _ = Map.empty

emptyGroupRib :: GroupRib
emptyGroupRib = Map.empty

insertGroupRib :: CompositeGroup -> GroupRib -> GroupRib
insertGroupRib _ _ = Map.empty

emptyClusterList :: ClusterList
emptyClusterList = Map.empty

insertClusterList :: Cluster -> ClusterList -> ClusterList
insertClusterList _ _ = Map.empty

deleteClusterList :: Hash -> ClusterList
deleteClusterList _ = Map.empty

updateClusterList :: Cluster -> [Cluster] -> ClusterList -> ClusterList
updateClusterList newCluster markedClusters oldClusterList = Map.insert (fromHash $ clHash newCluster) newCluster
                                                            $ foldl' (\cl c -> Map.delete (fromHash $ clHash c) cl ) oldClusterList markedClusters

-- IMPORTANT NOTE!!! : - the group rib is indexed by the hash over the underlying prefix list
--                   : as well as being vital for utility, this ensures that updated (split) groups overwrite older ones
--                   | this prpoerty is maintained as long as the function cgHash has that property
updateGroupRib :: [CompositeGroup] -> GroupRib -> GroupRib 
updateGroupRib cgs oldRib = foldl' (\rib cg -> Map.insert (fromHash $ cgHash cg) cg rib ) oldRib cgs

updatePrefixRib :: Cluster -> PrefixRib -> PrefixRib
updatePrefixRib cl oldRib = foldl' (\rib pfx -> Map.insert (fromPrefix pfx) (clHash cl) rib ) oldRib (clusterPrefixes cl)

