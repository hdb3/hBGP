module Main where
import Open
import Common
import Capabilities
import RFC4271
import BGPparse

main = mapM_ runTest [test1,test2,test3]

test1 = ("test1",
         BGPOpen 65520 40 (read "192.168.0.1") [ CapAS4 65520,  CapGracefulRestart False 0],
         BGPOpen 65521 20 ( read "0.0.0.0") [CapGracefulRestart False 0],
         BGPOpen 65521 30 (read "192.168.0.2") [ CapGracefulRestart False 0],
         BGPKeepalive)

test2 = ("test2", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { caps = [CapGracefulRestart False 0, CapAS4 65521]}
        rec' = rec 
        res' = BGPNotify NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) [CapAS4 65521]

test3 = ("test3", loc',req',rec',res') where
        (_, loc,req,rec,res) = test1
        loc' = loc
        req' = req { holdTime = 40 }
        rec' = rec 
        res' = BGPNotify NotificationOPENMessageError (encode8 UnacceptableHoldTime) []


runTest (desc,loc,req,rec,expect) = do 
    let sm = makeOpenStateMachine loc req
    let sm' = updateOpenStateMachine sm rec
    let resp =  getResponse sm'


    if expect == resp
        then putStrLn $ desc ++ ": success"
        else do
            putStrLn $ desc ++ ": ***fail***"
            putStrLn "expected: "
            print expect
            putStrLn "got: "
            print resp
            putStr "initialState: "
            print sm

            putStrLn ""
            putStr "receivedOffer: "
            print rec

            putStrLn ""
            putStr "updatedState: "
            print sm'

            putStrLn ""
            putStr "holdTime: "
            print $ getNegotiatedHoldTime sm'
