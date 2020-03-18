# Revision history for MRT

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 18/03/2020

Patched up to be independent of Router, BGPRib and BGPlib, especially the Prefix definitions.
May need to be reveresed selectively in order to generate wire format BGP, but the apps need to be visibly functional before making any more changes.
There is a remaining dependency for 'GroupRIBReport' in the ../bluster library
