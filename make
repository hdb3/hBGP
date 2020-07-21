#!/bin/bash -xe
cabal update
cabal build hbgp
find . -name hbgp -type f -executable -exec cp "{}" . \;
