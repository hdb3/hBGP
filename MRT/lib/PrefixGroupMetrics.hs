module PrefixGroupMetrics where

import Text.Printf
import qualified Data.IntMap.Strict as Map
import Data.Maybe(isJust,catMaybes)
import Data.List(sort,group)
import MRTrib

newtype PrefixRib = PR (Map.IntMap PrefixListHash)
newtype PrefixCache = PC (Map.IntMap IP4PrefixList)

lookup :: (PrefixRib, PrefixCache) -> (IP4PrefixList,PrefixListHash) -> Maybe [Int]
lookup (PR pr, PC pc) (pl,plh) = if isJust $ Map.lookup plh pc then Nothing else Just $ reduce $ map (flip Map.lookup pr . v4hash) pl where
    reduce :: [Maybe Int] -> [Int]
    -- reduce takes a list of (probably repeated) hashes and generates a list of small integers which count identical elements
    --    the sum of the list is the length of the input list
    reduce = sort . map length . group . sort . catMaybes

buildRIB :: [IP4PrefixList] -> (PrefixRib, PrefixCache)
buildRIB prefixLists = (buildPrefixRib prefixRibItems, buildPrefixCache prefixCacheItems)
    where
    prefixRibItems :: [(IP4PrefixHash,PrefixListHash)]
    prefixRibItems = concatMap (\prefixList -> map (\prefix -> (v4hash prefix, prefixHash prefixList)) prefixList) prefixLists
    buildPrefixRib :: [(IP4PrefixHash,PrefixListHash)] -> PrefixRib
    buildPrefixRib = PR . Map.fromList

    prefixCacheItems :: [(PrefixListHash,IP4PrefixList)]
    prefixCacheItems = map (\prefixList -> (prefixHash prefixList, prefixList)) prefixLists
    buildPrefixCache :: [(PrefixListHash,IP4PrefixList)] -> PrefixCache
    buildPrefixCache = PC . Map.fromList

fromRouteMapv4 :: RouteMapv4 -> [IP4PrefixList]
fromRouteMapv4 = Map.elems . Map.map snd

compareRouteMapv4 :: RouteMapv4 -> RouteMapv4 -> String
compareRouteMapv4 a b = "(" ++ showAs ra ++ " , " ++ showAs rb ++ " )   " ++ show (ra,rb)
    where
    showAs = showRatios
    (ra,rb) = compareLLists (fromRouteMapv4 a) (fromRouteMapv4 b)
    showAsPercentage (a0,a1,a2,a3,a4,a5) = printf "(%5.2f %5.2f %5.2f %5.2f %5.2f %5.2f" (sp a0) (sp a1) (sp a2) (sp a3) (sp a4) (sp a5) :: String
        where
        sp n = 100.0 * ( fromIntegral n / fromIntegral (a0+a1+a2+a3+a4+a5)) :: Float
    showAsSummary (a0,a1,a2,a3,a4,a5) = printf "(%5.2f %5.2f %5.2f" (sp a0) (sp (a3+a4)) (sp (a1+a2+a5)) :: String
        where
        sp n = 100.0 * ( fromIntegral n / fromIntegral (a0+a1+a2+a3+a4+a5)) :: Float
    showRatios (a0,a1,a2,a3,a4,a5) = printf "(%5.2f %5.2f(%5.2f) %5.2f" (sp a0) (sp (a3+a4)) (100* (a3'/a4')) (sp (a1+a2+a5)) :: String
        where
        sp n = 100.0 * ( fromIntegral n / fromIntegral (a0+a1+a2+a3+a4+a5)) :: Float
        a3' = fromIntegral a3  :: Float
        a4' = fromIntegral a4  :: Float

compareLLists :: [IP4PrefixList] -> [IP4PrefixList] -> ((Int,Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int,Int))
compareLLists a b = (compareLList b a , compareLList a b)
    where
    addT (a0,a1,a2,a3,a4,a5) (b0,b1,b2,b3,b4,b5) = (a0+b0,a1+b1,a2+b2,a3+b3,a4+b4,a5+b5)

    compareLList :: [IP4PrefixList] -> [IP4PrefixList] -> (Int,Int,Int,Int,Int,Int)
    compareLList c d = foldl f empty d where
        rib = buildRIB c
        f acc x = acc `addT` comparePrefix x 
        comparePrefix  :: IP4PrefixList -> (Int,Int,Int,Int,Int,Int)
        comparePrefix x = encode (length x) $ PrefixGroupMetrics.lookup rib (x, prefixHash x)

    encode :: Int -> Maybe [Int] -> (Int,Int,Int,Int,Int,Int)
    encode _ Nothing = complete
    encode _ (Just []) = none
    encode l (Just [x]) | l == x = superset
                        | otherwise = subset
    encode l (Just ax) | l == sum ax = multiple
                       | otherwise = multiplePartial
 
    empty      = (0,0,0,0,0,0)
    complete   = (1,0,0,0,0,0)
    none       = (1,1,0,0,0,0)
    subset     = (1,0,1,0,0,0)
    superset   = (1,0,0,1,0,0)
    multiple   = (1,0,0,0,1,0)
    multiplePartial   = (1,0,0,0,0,1)
   
