# bluster
## BGP Route Group Clusters
Implmentation of a group cluster based RIB for multi-peer BGP route tables
Internet route tables typically consist of groups of prefixes which have a common origin and BGP attribute list.  Such prefix groups can be treated as a single unit for routing policy purposes.  This offers the prospect of accelerating almost any processing task which requires prefix level processing - the main exception being filtering or other prefix value specific processing - however, such eventiality can be accomodated by introdcuing specific sub groups which partition prefix groups, down to prefix level if required.

This implementation contains a core library which is as generic as practiacble, but complete enough to permit evaluation in acore internet table context.
