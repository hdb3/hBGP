set -e ; for p in $(<pkg-dependency-order.txt) ; do echo $p ; pushd $p ; cabal install --only-dependencies --force-reinstalls ; cabal configure ; cabal build ; cabal install ; popd ; done
