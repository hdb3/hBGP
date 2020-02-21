vmdk=/home/nic/virl/vios-adventerprisek9-m.vmdk.SPA.157-3.M3

function build {
     vm=$1
     intnet="--network network=default2,model=e1000"
     extnet="--network network=default1,model=e1000"
     vmspec="--os-variant freebsd12.0 --graphics none --memory 2048"
     networks="$intnet"
     if (( $# < 2 )) ; then netn=1 ; else netn=$2 ; fi
     for i in $( seq 1 $netn ) 
         do networks="$networks $extnet"
         done
     virsh -q destroy $vm > /dev/null  || :
     virsh -q undefine $vm > /dev/null || :
     sudo rm -f $vm.qcow2
     qemu-img convert -f vmdk -O qcow2 $vmdk $vm.qcow2
     virt-install --quiet --noreboot --noautoconsole $vmspec --name $vm --import --disk $vm.qcow2 $networks
     ./custom.ex $vm $vm.startup.cfg
}

build br1 3
for vm in br3a br3b br3c
  do build $vm 1
  done
