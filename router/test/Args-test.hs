module Main where
import Args

main = do
    config <- getConfig
    -- print config
    either (\s -> putStrLn $ "failed: <" ++ s ++ ">")
           (\conf -> putStrLn $ "success " ++ show conf)
           config
