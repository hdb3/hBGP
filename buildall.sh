for b in controller full-addpath simple-prefix-addpath master ; do git checkout $b ; git pull  ;  done
for b in controller full-addpath simple-prefix-addpath master ; do git checkout $b ; stack build ; mkdir -p $b ; find .stack-work/install -name hbgp -executable -exec cp "{}" $b \; ;  done
