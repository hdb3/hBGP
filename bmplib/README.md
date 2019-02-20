# hBMP
Haskell implementation of BGP Management Protocol (BMP) parser 

RFC7854 specifies a protocol, BMP. which allows BGP speakers to mirror their internal state over the network.
BMP is a thin wrapper around BGP - most content is expressed as encapsulated BGP protocol messages.
This BMP wrapper treats the BGP payloads as opaque binary objects.
