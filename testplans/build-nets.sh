for net in default1 default2
  do virsh net-define $net.xml
     virsh net-autostart $net
     virsh net-start $net
  done
virsh net-list

exit 0

for net in default1 default2
  do virsh net-destroy $net
     virsh net-undefine $net
  done
