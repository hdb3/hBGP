module Router.UpdateSource where

import BGPRib.BGPRib hiding (group, update)
import BGPlib.BGPlib
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IP
import Data.Word

type UpdateSource = IO [BGPMessage]

initSourceDefault peer = initSource peer startPrefix tableSize groupSize burstSize burstDelay oneShotMode
  where
    startPrefix = "172.16.0.0/30"
    tableSize = 100
    groupSize = 4
    burstSize = 10
    burstDelay = 0
    oneShotMode = True

nullInitSource :: IO UpdateSource
nullInitSource = return f
  where
    f = do
      threadDelay $ 10 ^ 12 -- 1M seconds - but the caller will time us out according to its own keepalive timer...
      return []

initSource :: PeerData -> AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> Int -> Bool -> Int -> IO UpdateSource
initSource peer startPrefix tableSize groupSize burstSize burstDelay oneShotMode repeatDelay = do
  ---mv <- newMVar maxBound
  mv <- newMVar 0
  let f mv = do
        n <- fromIntegral <$> takeMVar mv
        --- !!!!!! note this is NoOp as long as the initial value is 0 not maxBound
        if n == maxBound
          then do
            putMVar mv 0
            return [endOfRib, BGPKeepalive]
          else do
            ---let initDelay = 2000 -- mS  - arbitary one second delay to allow a BGP peer to settle after reeceiving inintial EoR
            ---                     -- this needs to be made configurable, too.... ;-)
            ---when (n == 0) (threadDelay $ 10^3 * initDelay)
            putMVar mv $ n + burstSize
            if oneShotMode
              then
                if n < tableSize
                  then do
                    when (burstDelay /= 0) (threadDelay $ 10 ^ 3 * burstDelay)
                    let burst = concatMap (update peer startPrefix tableSize groupSize) [n .. min tableSize (n + burstSize) - 1]

                        burst' = if n == 0 then markFirst burst else burst
                        burst'' = if n + burstSize >= tableSize then markLast burst' else burst'
                        markFirst (a : ax) = mark a : ax
                        markLast ax = init ax ++ [mark $ last ax]
                        mark = modifyPathAttributes (setOrigin _BGP_ORIGIN_EGP)

                    -- default behaviour before marking first and last updates
                    -- return burst

                    -- modified behaviour, marking first and last updates
                    return $ encodeUpdates burst''
                  else
                    if repeatDelay > 0
                      then do
                        _ <- takeMVar mv
                        putMVar mv 0
                        threadDelay $ 10 ^ 6 * repeatDelay
                        -- this is not going to work for small values of hold timer
                        f mv
                      else -- return []

                      -- empty list tells CustomRib that the update stream is now empty
                        return []
              else do
                when (burstDelay /= 0) (threadDelay $ 10 ^ 3 * burstDelay)
                return $ encodeUpdates $ concatMap (update peer startPrefix tableSize groupSize) [n .. n + burstSize - 1]

  return (f mv)

group :: AddrRange IPv4 -> Word32 -> Word32 -> [AddrRange IPv4]
group startPrefix groupSize index = map ip4 $ seeds (ip4' ip) groupSize index
  where
    (ip, prefixLength) = addrRangePair startPrefix
    ip4 = flip makeAddrRange prefixLength . fromHostAddress . byteSwap32 . flip shiftL (32 - prefixLength)
    ip4' = flip shiftR (32 - prefixLength) . byteSwap32 . toHostAddress
    seeds base groupSize index = map (base + index * groupSize +) [0 .. groupSize - 1]

main = do
  s <- initSource dummyPeerData "192.168.0.0/24" 8 2 4 0 True 0
  replicateM 10 s >>= print

update :: PeerData -> AddrRange IPv4 -> Word32 -> Word32 -> Word32 -> [ParsedUpdate] -- the return value is only multiple to cope with the fact that a very large prefix list couls spill over the BGP message limit
-- ideally, a lower level encoder should capture this, i.e. at the first binary conversion stage......
update peer startPrefix tableSize groupSize n =
  buildUpdate
    peer
    (1 + n `mod` tableSize) -- these are the AS numbers to build the AS path
    (1 + n `div` tableSize) -- these are the AS numbers to build the AS path
    (group startPrefix groupSize $ n `mod` tableSize)
  where
    buildUpdate :: PeerData -> Word32 -> Word32 -> [AddrRange IPv4] -> [ParsedUpdate]
    buildUpdate peer as1 as2 prefixes =
      let aspath = if isExternal peer then [as1, as2] ++ [myAS $ globalData peer] else [as1, as2]
          localPrefOrMED = if isExternal peer then PathAttributeMultiExitDisc 0 else PathAttributeLocalPref 0
          nextHop = PathAttributeNextHop (localIPv4 peer)
       in makeUpdate
            (map fromAddrRange prefixes)
            []
            [PathAttributeOrigin _BGP_ORIGIN_IGP, PathAttributeASPath $ ASPath4 [ASSequence aspath], nextHop, localPrefOrMED]
