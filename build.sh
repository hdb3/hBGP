for D in network-simple ; bgplib bgprib bmplib zserv MRT bluster router ; do pushd $D ; cabal install --only-dependencies ; cabal configure ; cabal build ; cabal install ; popd ; done
