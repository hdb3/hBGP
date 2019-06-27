#!/bin/bash -xe
rm -rf cabal.sandbox.config .cabal-sandbox
cabal update
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal exec -- ghc -O2 Router/Router.hs

