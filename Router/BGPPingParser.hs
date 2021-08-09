module Router.BGPPingParser
  ( getGenerator,
    getNext,
  )
where

import Data.Attoparsec.ByteString (IResult (..), Parser, feed, parse)
import qualified Data.ByteString as ByteString
import Data.IORef

type BS = ByteString.ByteString

getNext :: IO BS -> Parser a -> BS -> IO (Maybe (BS, a))
getNext getter myParser bs = go (parse myParser bs)
  where
    go result = case result of
      (Done i r) -> return $ Just (i, r)
      f@Partial {} -> do
        bs <- getter
        if ByteString.null bs then return Nothing else go (feed f bs)
      (Fail _ context description) -> do
        putStrLn $ "parse failed: " ++ description
        mapM_ putStrLn context
        return Nothing

-- An example of this strategy:

-- example1 =
--   let go bs = do
--         next <- getNext getter parser bs
--         maybe
--           (return ())
--           (\(bs', val) -> action val >> go bs')
--           next
--    in go ByteString.empty

-- This completes a circle of reasoning - the next step here is to recognize the immutable pattern erected around the ‘action val’ expression and build a control flow abstraction, which returns to the first proposed approach!  Nonetheless, a complete instance providing getNext as a simple IO action has value, and requires an IORef bound to an IO function to produce a handle:

getNextIORef :: IO BS -> Parser a -> IORef BS -> IO (Maybe a)
getNextIORef getter parser ioref = do
  bs <- readIORef ioref
  next <- getNext getter parser bs
  maybe
    (return Nothing)
    ( \(bs', val) -> do
        writeIORef ioref bs'
        return $ Just val
    )
    next

-- this can be initialised into a simple genrator as follows:
getGenerator :: IO BS -> Parser a -> IO (IO (Maybe a))
getGenerator getter parser = getNextIORef getter parser <$> newIORef ByteString.empty

-- example = do
--   putStrLn "start of function"
--   gen <- getGenerator getter parser
--   let go = do
--         next <- gen
--         maybe
--           (putStrLn "end of stream")
--           ( \val -> do
--               action val
--               go
--           )
--           next
--   go
--   putStrLn "end of function"
