for p in $(<pkg-dependency-order.txt) ; do echo $p ; pushd $p ; cabal configure ; cabal build ; cabal install --force-reinstalls ; popd ; done
