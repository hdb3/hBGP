{-# LANGUAGE RecordWildCards #-} 
module Open where
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Maybe(isJust,fromJust)
import Data.IP(fromHostAddress)

import BGPlib

-- parse/deparse the Open message, especially the optional parametes//capabilities
-- the optional parameter field has a (8bit) length sub-field followed by 0 or more 'parameters
-- parameters are coded as TLVs with 8bit T and L
-- only one parameter is currently used - '2' == Capabilities Optional Parameter
-- (see RFC5492)
-- within the 'Capabilities Optional Parameter' is another TLV encoding,
-- although there is no length field to start
--
-- open processing consists of a function useable within the FSM to process and generate Open messages including optional capabilities
-- this includes the logic to handle incompatible capability objectives
-- it also includes processing of the Hold timer values, AS number and BGPID
--
-- from the 'application level' it accepts parameters and shows what has been negotiated
-- 
-- the application level builds an initial state object
-- this is called by the FSM when it receives Open or wants to send Open (either ordering)
-- the response after receiving Open can be +ve or -ve, which should result in either Keepalive or Notification message
-- the underlying logic uses a copy of the resepective 'offers'
-- once keepalive or notification has been confirmed from remote side then the result is confirmed
--
-- the semanitics of the capabilities etc is transparent to this mechanism - i.e.
-- the application layer should simply provide a list of tuples representing
-- the objectives
--
-- Structure of an 'offer'
-- Hold Time
-- My AS
-- My BGPID
-- an (opaque) list of type/value tuples - 'offer'
--
-- additionally the application level can/should specify requirements
-- remote AS
-- remote BGPID
-- required type/value tuples
-- excluded type/value tuples
--
-- these may all be null in which case any offer is accepted
--
-- once an Open has been received a response is available
-- the Open populates a 'remote' offer structure, of identical form to the 'local' one
--
-- getResponse calculates the compliance of the remote offer with the local requirement
-- the response is a BGPMessage, either Keepalive or Notification
-- note that RFC4271 only allows a single error to be supplied in the Notification message
-- which is reported is determined by the orser of checks in the getResponse function
--
-- note: zero values are used to denote unenforced constraints in the 'required' section...
data OpenStateMachine = OpenStateMachine {localOffer :: BGPMessage
                                         , remoteOffer :: Maybe BGPMessage
                                         , required :: BGPMessage
                                         } deriving Show

makeOpenStateMachine :: BGPMessage -> BGPMessage -> OpenStateMachine
makeOpenStateMachine local required | isOpen local = OpenStateMachine local Nothing required

updateOpenStateMachine :: OpenStateMachine -> BGPMessage -> OpenStateMachine
updateOpenStateMachine osm remoteOpen | isOpen remoteOpen = osm { remoteOffer = Just remoteOpen }

getNegotiatedHoldTime :: OpenStateMachine -> Int
getNegotiatedHoldTime = fromIntegral . getNegotiatedHoldTime'
getNegotiatedHoldTime' OpenStateMachine {..} | isJust remoteOffer = min ( holdTime remoteOffer') ( holdTime localOffer) where remoteOffer' = fromJust remoteOffer

getKeepAliveTimer :: OpenStateMachine -> Int
getKeepAliveTimer osm = 1 + fromIntegral (getNegotiatedHoldTime osm) `div` 3

checkAS4Capability :: OpenStateMachine -> Bool
checkAS4Capability OpenStateMachine {..} = hasAS4 (caps remoteOffer') && hasAS4 (caps localOffer)
    where
    remoteOffer' = fromJust remoteOffer
    -- ugly - better done in Capabilities.hs
    hasAS4 = any (eq_ (CapAS4 0))

-- getResponse should not be called before an OPEN message has been received

getResponse :: OpenStateMachine -> BGPMessage
getResponse osm@OpenStateMachine {..} | isJust remoteOffer = firstMaybe [checkmyAS , checkBgpID , checkHoldTime , checkOptionalCapabilities, keepalive] where
        firstMaybe [] = undefined
        firstMaybe (Just m : mx) = m
        firstMaybe (Nothing : mx) = firstMaybe mx

        remoteOffer' = fromJust remoteOffer
        localBGPID = bgpID localOffer
        remoteBGPID = bgpID remoteOffer'
        requiredBGPID = bgpID required
        nullBGPID = fromHostAddress 0
        nullAS = 0

        keepalive = Just BGPKeepalive

        checkBgpID :: Maybe BGPMessage
        -- includes a sanity check that remote BGPID is different from the local value even if there is no explicit requirement
        checkBgpID | remoteBGPID == localBGPID = Just (BGPNotify NotificationOPENMessageError (encode8 BadBGPIdentifier) L.empty)
                   | requiredBGPID == nullBGPID || requiredBGPID == remoteBGPID = Nothing
                   | otherwise = Just (BGPNotify NotificationOPENMessageError (encode8 BadBGPIdentifier) L.empty)

        checkHoldTime :: Maybe BGPMessage
        checkHoldTime = if holdTime required > getNegotiatedHoldTime' osm
                        then Just (BGPNotify NotificationOPENMessageError (encode8 UnacceptableHoldTime) L.empty)
                        else Nothing

        checkmyAS :: Maybe BGPMessage
        checkmyAS = if nullAS == myAutonomousSystem required || myAutonomousSystem remoteOffer' == myAutonomousSystem required
                    then Nothing
                    else Just (BGPNotify NotificationOPENMessageError (encode8 BadPeerAS) L.empty)

-- a naive check looks for identical values in capabilities,
-- which is how the RFC is worded
-- However, in practice the requirement differs for each specific case, and in fact is not
-- clearly defined in some cases.  The minimal requirement appears to be a check for simple presence, with no comparison
-- of value.  This is clearly true for two common cases; AS4/32-bit ASNs, and Graceful Restart.
-- Note - there is no negotiation concept for Optional capabilities, and the responsibility for rejecting a peering lies with the prposer of a capability
-- which should arise when the peer has not advertised a capability which is required.
-- The present implementation consists simply of a check that the remote offer contains at least the capabilities in the required list.
--  
--  
-- this is the mentioned check for presecnce in remote offer of required parameters
-- return a list of capabilities required but not found in the offer
        checkOptionalCapabilities :: Maybe BGPMessage
        checkOptionalCapabilities = if null missingCapabilities then Nothing else Just (BGPNotify NotificationOPENMessageError (encode8 UnsupportedOptionalParameter) (encode missingCapabilities)) where
            offered  = caps remoteOffer'
            missingCapabilities = check (caps required)
            check [] = []
            check (c:cx) = if any (eq_ c) offered then check cx else c : check cx
