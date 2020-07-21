#!/bin/bash -xe
cabal update
cabal build hbgp
find dist-newstyle/build -name hbgp -type f -executable -exec cp "{}" . \;
