module Main where
{-
import Data.List(delete)

inc [] _ = True
inc ax [] | not (null ax) = False
inc ax (b:bx) = inc (delete b ax) bx
-}
import Common

main = mapM incTest [
    ([1],[2,3],False),
    ([2],[2,3],True),
    ([],[2,3],True),
    ([],[],True),
    ([2],[],False),
    ([1,2],[1,2,3],True),
    ([1,2],[2,3],False),
    ([1,2],[2,3,4,5,6,7,1],True),
    ([1,2],[2,3,4,5,6,7,8],False)]

incTest (a,b,p) = do
    let p' = included a b
    if p == p'  then
        putStr "OK "
    else
        putStr "*** Fail "
    print (a,b,p)
