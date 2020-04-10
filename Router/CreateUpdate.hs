module Router.CreateUpdate(iBGPUpdate,eBGPUpdate,bgpWithdraw,eor) where
import System.IO(stdout,stderr)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (hPut)
import Data.Binary(encode,Word32)
import Data.IP

import BGPlib.BGPlib (wireFormat , fromAddrRange, _BGP_ORIGIN_IGP, PathAttribute(..),  ASSegment(..), ASPath(..) )
import BGPRib.BGPRib (ParsedUpdate, makeUpdate , makeUpdate )


bgpWithdraw :: [ AddrRange IPv4] -> [ParsedUpdate]
bgpWithdraw  prefixes  = makeUpdate [] ( map fromAddrRange prefixes ) []

eor :: [ParsedUpdate]
eor = eorBGPUpdate

iBGPUpdate :: [Word32] -> [ AddrRange IPv4] -> IPv4 -> Word32 -> [ParsedUpdate]
iBGPUpdate = xBGPUpdate False

eBGPUpdate :: [Word32] -> [ AddrRange IPv4] -> IPv4 -> Word32 -> [ParsedUpdate]
eBGPUpdate = xBGPUpdate True

xBGPUpdate isExternal aspath prefixes nextHop varpar = makeUpdate
                    ( map fromAddrRange prefixes )
                    []
                    [ PathAttributeOrigin _BGP_ORIGIN_IGP
                    , PathAttributeASPath [ASSequence aspath]
                    , PathAttributeNextHop nextHop
                    , if isExternal then PathAttributeMultiExitDisc varpar else PathAttributeLocalPref varpar
                    ]

eorBGPUpdate = makeUpdate [] [] []
