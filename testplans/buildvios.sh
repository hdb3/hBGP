vmdk=/home/nic/virl/vios-adventerprisek9-m.vmdk.SPA.157-3.M3
virsh destroy vios1 || echo "nowt to do"
virsh undefine vios1 || echo "nowt to do again"
sudo rm -f vios1.qcow2
qemu-img convert -f vmdk -O qcow2 $vmdk vios1.qcow2
virt-install --os-variant freebsd12.0 --noautoconsole --import --disk vios1.qcow2 --name vios1 --memory 2048 --network network=default1,model=e1000 --network network=default2,model=e1000 --network network=default2,model=e1000 --network network=default2,model=e1000
#virsh console vios1
./custom.ex vios1 br1.startup.cfg
#./bootstrap.ex vios1 br1.startup.cfg
exit 0
for vm in vios3a vios3b vios3c
  do qemu-img convert -f vmdk -O qcow2 $vios.vmdk $vm.qcow2
     virt-install --os-variant freebsd12.0 --noautoconsole --import --disk $vm.qcow2 --name $vm --memory 2048 --network network=default1,model=e1000 --network network=default2,model=e1000
     virsh console $vm
     virsh destroy $vm
  done
