#!/usr/bin/expect
set vm [lindex $argv 0]
set sshconfig "sshbase.cfg"
set config [lindex $argv 1]
set data [read [open $config]]
set sshdata [read [open $sshconfig]]
# set 0 -> 1 for console logging of the process
log_user 0

proc ts {s} {
  set t [timestamp -format %X]
  send_user "\n$t $s "
}

spawn /usr/bin/virsh start $vm
expect "Domain $vm started"

ts "vm started"

spawn /usr/bin/virsh console $vm
ts "console started"

expect { "Escape character is ^]" { ts "console connected" } 
         -re ".*\n" { send_user . }
         timeout { send_user ~ ; exp_continue }
       }

ts "waiting for config dialog"

set timeout 120

expect {
        -re "Would you like to enter the initial configuration dialog" { ts "console active, waiting for command prompt"; send "no\r\n" }
         timeout { ts " timed out (120 seconds)\n" ; exit 1 }
       }

set timeout 60
send "\r"
expect {
         -re "Press RETURN to get started" { send "\r"; exp_continue -continue_timer }
         -re "Router>" {  send "enable\r" ; ts "command prompt active, requesting privileged mode" } 
         -re "\nCompiled" { send "\r" ; exp_continue -continue_timer}
         timeout {  ts "phase 2 timeout" ; exit 1 }
       }

expect {
         -re "Router#" {  ts "got privileged mode prompt"} 
         timeout {  ts "phase 3 timeout" ; exit 1 }
       }

expect *

ts "initialisation complete , sending config $config"

send "\rterminal no monitor\rconfig terminal\r"

send "$data"
send "$sshdata"
send "end"
expect *
send "\r\rwrite memory\r\r"
ts "configuration complete , waiting for confirmation"

set timeout 20
expect {
         "\[OK\]" { ts "got OK"}
         -re "\n" { exp_continue }
         timeout { ts "timeout waiting for OK after save startup-config" ; exit 1 }
       }

ts "startup configuration saved, waiting for console prompt"

expect *
send "\r"
set timeout 1
expect {
         -re "\n(.*)#" { set rname $expect_out(1,string) ; ts "matched device name is $rname" }
         timeout { send_user "." ; send "\r" ; exp_continue }
       }

ts "reload request"
send "\rreload\r"

expect { "[confirm]" { send "\r" } }

ts "reload request accepted"

sleep 2

send "\035"
expect eof

ts "console disconnected, stopping VM"

# delay needed before stopping VM to avoid configuration loss
sleep 5

spawn /usr/bin/virsh destroy $vm 

expect -re "Domain $vm destroyed"
expect eof
ts "bootstrap complete for $vm using $config\n"
