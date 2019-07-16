module Stopwatch where
import qualified Data.Time.Clock.System as DT
import Text.Printf

systime :: IO DT.SystemTime
systime = DT.getSystemTime

diffSystemTime :: DT.SystemTime -> DT.SystemTime -> Double
diffSystemTime (DT.MkSystemTime s0 ns0) (DT.MkSystemTime s1 ns1) = 
    f s1 ns1 - f s0 ns0 where
    f s ns = ( 0.0 + fromIntegral (s * 1000000000) + fromIntegral ns ) / 1000000000.0

showDiffTime :: DT.SystemTime -> DT.SystemTime -> String
showDiffTime tx ty = if 1.0 > dT then printf "%.3f ms" (1000*dT) else printf "%.3f s" dT where dT = diffSystemTime tx ty

stopwatch :: String -> DT.SystemTime -> DT.SystemTime -> IO DT.SystemTime
stopwatch s t0 t = do
    t' <- systime
    putStrLn $ "elapsed " ++ showDiffTime t0 t' ++ " delta " ++ showDiffTime t t' ++ " " ++ s
    return t'

timeIO :: String -> IO () -> IO ()
timeIO s f = do
    t0 <- systime
    f
    t1 <- systime
    putStrLn $ s ++ " - elapsed " ++ showDiffTime t0 t1
