for sd in config lib logs packages share store world ; do rm -rf ~/.cabal/$sd ; done ; rm -rf ~/.ghc ; cabal update
