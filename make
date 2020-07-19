#!/bin/bash -e
branches="controller full-addpath simple-prefix-addpath"
for b in $branches ; do git checkout $b ; git pull  ;  done
# for b in $branches ; do git checkout $b ; stack build ; mkdir -p $b ; find .stack-work/install -name hbgp -executable -exec cp "{}" $b \; ;  done
for b in $branches ; do git checkout $b ; cabal build --enable-optimization ; mkdir -p $b ; find dist-newstyle/build -name hbgp -type f -executable -exec mv "{}" "hbgp.${b}" \; ;  done
ls -l hbgp\.*
for b in $branches ; do mv -fv "hbgp.${b}" $HOME/.local/bin ; done
