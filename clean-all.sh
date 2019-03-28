for p in $(<pkg-dependency-order.txt) ; do echo $p ; cabal-uninstall $p --force ; done
for p in $(<pkg-dependency-order.txt) ; do echo $p ; pushd $p ; rm -rf *.o *.hi dist ; cabal clean ; popd ; done
