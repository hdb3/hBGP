{-# LANGUAGE OverloadedStrings #-}
module Stopwatch where
import qualified Data.Time.Clock.System as DT
import Text.Printf

systime = DT.getSystemTime
diffSystemTime :: DT.SystemTime -> DT.SystemTime -> Double
diffSystemTime (DT.MkSystemTime s0 ns0) (DT.MkSystemTime s1 ns1) = 
    f s1 ns1 - f s0 ns0 where
    f s ns = ( 0.0 + fromIntegral (s * 1000000000) + fromIntegral ns ) / 1000000000.0

stopwatch s t0 t = do
    t' <- systime
    -- putStrLn $ s ++ " " ++ show (diffSystemTime t t')
    let showDiffTime tx ty = if 1.0 > dT then printf "%.3f ms" (1000*dT) else printf "%.3f s" dT where dT = diffSystemTime tx ty
    putStrLn $ "elapsed " ++ showDiffTime t0 t' ++ " delta " ++ showDiffTime t t' ++ " " ++ s
    return t'
