sudo groupadd pcap
sudo usermod -a -G pcap $USER
for app in tcpdump dumpcap ; do
  sudo chgrp pcap `which $app`
  sudo setcap cap_net_raw,cap_net_admin=eip `which $app`
done

sudo sysctl net.ipv4.ip_unprivileged_port_start=179
