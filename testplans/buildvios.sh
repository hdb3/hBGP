vmdk=/home/nic/virl/vios-adventerprisek9-m.vmdk.SPA.157-3.M3
virsh -q destroy br1 || :
virsh -q undefine br1 || :
sudo rm -f br1.qcow2
qemu-img convert -f vmdk -O qcow2 $vmdk br1.qcow2
virt-install --os-variant freebsd12.0 --noautoconsole --import --disk br1.qcow2 --name br1 --memory 2048 --network network=default2,model=e1000 --network network=default1,model=e1000 --network network=default1,model=e1000 --network network=default1,model=e1000
./custom.ex br1 br1.startup.cfg
# exit 0
for vm in br3a br3b br3c
  do virsh -q destroy $vm || :
     virsh -q undefine $vm || :
     sudo rm -f $vm.qcow2
     qemu-img convert -f vmdk -O qcow2 $vmdk $vm.qcow2
     virt-install --os-variant freebsd12.0 --noautoconsole --import --disk $vm.qcow2 --name $vm --memory 2048 --network network=default2,model=e1000 --network network=default1,model=e1000
     ./custom.ex $vm $vm.startup.cfg
  done
