for net in default1 default2
  do virsh net-define $net.xml
     virsh net-autostart $net
     virsh net-start $net
  done
virsh net-list

sudo ip addr add 100.64.0.2/30 dev virbr1
sudo ip addr add 100.64.0.6/30 dev virbr1
sudo ip addr add 100.64.0.10/30 dev virbr1

exit 0

for net in default1 default2
  do virsh net-destroy $net
     virsh net-undefine $net
  done
