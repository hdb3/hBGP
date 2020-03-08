module Main where
import BGPRib.PTE
import BGPRib.PT

main :: IO ()
main = do
    let v0 = (42,4)
        v1 = (42,6)
        v2 = (42,1)
        v3 = (42,3)
        vw = (42,0)
        t0 = []
        t1 = [v0]
        t2 = [(42,3)]
        t3 = [(42,5)]
        t4 = [(99,3)]
        t5 = [(99,5)]
        t6 = [(99,5),(99,3)]
        t7 = [(99,5),(99,3),(42,2)]
        t8 = [(99,5),(42,3),(99,2)]
        t9 = [(42,7),(99,5),(99,2)]
        test :: RD -> [RD] -> IO()
        test a b = putStrLn $ show (pteUpdate a b) ++ " (" ++ show b ++ ")"

    putStrLn "delete cases"
    test vw t0
    test vw t1
    test vw t2
    test vw t3
    test vw t4
    test vw t5
    test vw t6
    test vw t7
    test vw t8
    test vw t9

    putStrLn "insert cases - mid preference"
    
    test v0 t0
    test v0 t1
    test v0 t2
    test v0 t3
    test v0 t4
    test v0 t5
    test v0 t6
    test v0 t7
    test v0 t8
    test v0 t9

    putStrLn "insert cases - high preference"
    
    test v1 t4
    test v1 t5
    test v1 t6
    test v1 t7
    test v1 t8
    test v1 t9
    
    putStrLn "insert cases - low preference"
    
    test v2 t4
    test v2 t5
    test v2 t6
    test v2 t7
    test v2 t8
    test v2 t9
        
    putStrLn "insert cases - mid/equal preference"
    
    test v3 t4
    test v3 t5
    test v3 t6
    test v3 t7
    test v3 t8
    test v3 t9