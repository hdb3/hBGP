{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module BGPlib.Update
  ( ParsedUpdate (..),
    parseUpdate,
    iBGPUpdate,
    bgpWithdraw,
    BGPOutput,
    makeUpdate,
    deparseBGPOutputs,
    endOfRib,
    originateUpdate,
  )
where

import BGPlib.BGPMessage
import BGPlib.PathAttributeBuilder
import BGPlib.PathAttributeUtils
import BGPlib.PathAttributes
import BGPlib.PrefixBuilder
import BGPlib.Prefixes
import BGPlib.RFC4271
import ByteString.StrictBuilder (builderBytes)
import qualified Data.ByteString as B
import Data.IP
import Data.Word
import FarmHash (hash64)

-- TODO - consistent regular naming to distinguish the 'in' and 'out' formats

{-
  'input' message formats: -> ParsedUpdate
-}

data ParsedUpdate = ParsedUpdate {puPathAttributes :: [PathAttribute], nlri :: [Prefix], withdrawn :: [Prefix], hash :: Int} | NullUpdate -- NullUpdate is used as a proxy for Keepalive, however probably not for any good reason - TODO? remove

instance Show ParsedUpdate where
  show NullUpdate = "< - >"
  show ParsedUpdate {..} =
    "<"
      ++ if null puPathAttributes
        then "*"
        else
          show (getASPath puPathAttributes)
            ++ " "
            ++ show nlri
            ++ "|"
            ++ show withdrawn
            ++ ">"

modifyPathAttributes :: ([PathAttribute] -> [PathAttribute]) -> ParsedUpdate -> ParsedUpdate
modifyPathAttributes f pu = pu {puPathAttributes = f $ puPathAttributes pu}

bgpWithdraw :: [AddrRange IPv4] -> ParsedUpdate
bgpWithdraw prefixes = makeBGPUpdate [] (map fromAddrRange prefixes) []

eor :: ParsedUpdate
eor = makeBGPUpdate [] [] []

iBGPUpdate :: [Word32] -> [AddrRange IPv4] -> IPv4 -> Word32 -> ParsedUpdate
iBGPUpdate = xBGPUpdate False

eBGPUpdate :: [Word32] -> [AddrRange IPv4] -> IPv4 -> Word32 -> ParsedUpdate
eBGPUpdate = xBGPUpdate True

xBGPUpdate isExternal aspath prefixes nextHop varpar =
  makeBGPUpdate
    (map fromAddrRange prefixes)
    []
    [ PathAttributeOrigin _BGP_ORIGIN_IGP,
      PathAttributeASPath [ASSequence aspath],
      PathAttributeNextHop nextHop,
      if isExternal then PathAttributeMultiExitDisc varpar else PathAttributeLocalPref varpar
    ]

-- Warning - makeBGPUpdate creates 'fake' input messages, not network outputs
-- somewaht ineffeicient because the function encodes and decodes attributes on the way to building a hash - but a hash is certainly needed, though not neccesarily this one.....
makeBGPUpdate nlri withdrawn attributes = parseUpdate $ BGPUpdate {withdrawn = withdrawn, attributes = attributes, nlri = nlri}

{-# INLINE parseUpdate #-}
parseUpdate :: BGPMessage -> ParsedUpdate
parseUpdate BGPUpdate {..} = ParsedUpdate {puPathAttributes = attributes, nlri = nlri, withdrawn = withdrawn, hash = pathHash attributes}

{-# INLINE myHash #-}
myHash :: B.ByteString -> Int
myHash = fromIntegral . hash64

{-
  'output' message formats: -> BGPOutput
-}

data BGPOutput = BGPOutput {withdrawn :: [Prefix], attributes :: [PathAttribute], nlri :: [Prefix]} deriving (Show)

deparseBGPOutputs :: [BGPOutput] -> B.ByteString
deparseBGPOutputs = builderBytes . foldMap builder
  where
    builder BGPOutput {..} = updateBuilder withdrawn (buildPathAttributes attributes) nlri

endOfRib :: BGPOutput
endOfRib = makeUpdate [] [] []

originateWithdraw :: [Prefix] -> BGPOutput
originateWithdraw prefixes = makeUpdate [] prefixes []

originateUpdate :: Word8 -> [Word32] -> IPv4 -> [Prefix] -> BGPOutput
originateUpdate origin path nextHop prefixes =
  makeUpdate prefixes [] [PathAttributeOrigin origin, PathAttributeASPath [ASSequence path], PathAttributeNextHop nextHop]

makeUpdateSimple :: [PathAttribute] -> [Prefix] -> [Prefix] -> BGPOutput
makeUpdateSimple p n w = makeUpdate n w p

{-# INLINE makeUpdate #-}
makeUpdate :: [Prefix] -> [Prefix] -> [PathAttribute] -> BGPOutput
makeUpdate nlri withdrawn attributes = BGPOutput withdrawn attributes nlri
