for p in $(<pkg-dependency-order.txt) ; do echo $p ; pushd $p ; cabal clean ; rm -rf *.o *.hi ; popd ; done
