qemu-img convert -f vmdk -O qcow2 /home/nic/virl/vios-adventerprisek9-m.vmdk.SPA.157-3.M3 vios1.qcow2
virt-install --os-variant freebsd12.0 --noautoconsole --import --disk vios1.qcow2 --name vios1 --memory 2048 --network network=default1,model=e1000 --network network=default2,model=e1000 --network network=default2,model=e1000 --network network=default2,model=e1000
virsh console vios1
exit 0
for vm in vios3a vios3b vios3c
  do qemu-img convert -f vmdk -O qcow2 /home/nic/virl/vios-adventerprisek9-m.vmdk.SPA.157-3.M3 $vm.qcow2
     virt-install --os-variant freebsd12.0 --noautoconsole --import --disk $vm.qcow2 --name $vm --memory 2048 --network network=default1,model=e1000 --network network=default2,model=e1000
     virsh console $vm
     virsh destroy $vm
  done
