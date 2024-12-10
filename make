#!/bin/bash -xe
cabal update
cabal build
mkdir -p bin
cabal install  --installdir=bin --install-method=copy --overwrite-policy=always 
