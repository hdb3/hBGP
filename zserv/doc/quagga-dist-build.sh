git clone git://git.savannah.nongnu.org/quagga.git -b quagga-1.2.4
cd quagga
./bootstrap.sh
./configure
make dist
rm -rf rpmbuild ; mkdir rpmbuild ; mkdir rpmbuild/SOURCES ; mkdir rpmbuild/SPECS ; cp redhat/*.spec rpmbuild/SPECS/ ; cp quagga*.tar.gz rpmbuild/SOURCES/ ; cp quagga.spec rpmbuild/SPECS/ ; rpmbuild --define "_topdir `pwd`/rpmbuild" -ba rpmbuild/SPECS/quagga.spec ; ls ./rpmbuild/RPMS/x86_6 ; tar cfzv quagga-rpm.tgz -C rpmbuild/RPMS/x86_64 . ; mv quagga-rpm.tgz .. 

# debian reqs : autoconf automake libtool make gawk libreadline-dev  libc-ares-dev texinfo dejagnu  groff libsnmp-dev  libcap-dev texi2html pkg-config

# FOR RPM BUILD ONLY!!!
# additional debian reqs : librpmbuild3
# deian - also note that the quagga.spec file BuildRequires should be removed as the debian packages are not detected correctly
# debian also note i never made this work(on debian)

# rhel deps : git autoconf automake libtool make gawk readline-devel  texinfo dejagnu net-snmp-devel groff rpm-build net-snmp-devel  libcap-devel texi2html c-ares-devel perl-generators pam-devel
