module FromHandle where
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import System.IO.Streams
import System.IO (Handle)
import Control.Monad.IO.Class (liftIO)

actionYield a = putStrLn $ "read complete" ++ show a
actionEOF = putStrLn "at EOF"

generator :: (Binary a, Show a) => Handle -> Generator a ()
generator h = let
    start = go ( runGetIncremental get )
    go (Fail _ consumed s) = fail $ "generator failed at offset " ++ show consumed ++ " : " ++ s
    go (Partial f) = do chunk <- liftIO $ BS.hGet h 4096
                        if BS.null chunk
                        then return () -- return from the generator yields end-of-stream 'Nothing'
                        else let mChunk = if BS.null chunk
                                          then Nothing
                                          else Just chunk
                                 cont = f mChunk
                             in go cont
    go (Done bs _ a) = do yield a
                          if BS.null bs
                          then start
                          else go (newDecoder (Just bs)) where (Partial newDecoder) = runGetIncremental get
    in start

streamBinary h = fromGenerator (generator h)
