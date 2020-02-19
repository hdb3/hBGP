#!/usr/bin/expect
set vm [lindex $argv 0]
set config [lindex $argv 1]
set f [open $config]
set data [read $f]
log_user 0
#log_file -noappend bootstrap.log

proc ts {s} {
  set t [timestamp -format %X]
  send_user "\n$t $s "
}

spawn /usr/bin/virsh start $vm
expect "Domain $vm started"

ts "domain started"

spawn /usr/bin/virsh console $vm

expect { "Escape character is ^]" { ts "console connected" } 
         -re ".*\n" { send_user . }
         timeout { send_user ~ ; exp_continue }
       }

ts "waiting for config dialog"

set timeout 120

expect {
        -re "Would you like to enter the initial configuration dialog" { ts "console active, waiting for command prompt"; send "no\r\n" }
        -re ".*\n" { send "\r" ; send_user "." ; exp_continue -continue_timer }
         timeout { ts " timed out (120 seconds)\n" ; exit 1 }
       }

set timeout 60
send "\r"
expect {
         -re "Press RETURN to get started" { send "\r"; exp_continue -continue_timer }
         -re "Router>" {  send "enable\r" ; ts "command prompt active, requesting privileged mode" } 
         -re "\n" { send "\r" ; send_user . ; exp_continue -continue_timer}
         timeout {  ts "phase 2 timeout" ; exit 1 }
       }

expect {
         -re "Router#" {  ts "got privileged mode prompt"} 
         -re "\n" { send "\r" ; send_user . ; exp_continue -continue_timer}
         timeout {  ts "phase 3 timeout" ; exit 1 }
       }

expect *
ts "initialisation complete , sending config $config"
# log_user 1
send "\rterminal no monitor\rconfig terminal\r"

send "$data"
sleep 1
expect *
send "\r\rwrite memory\r\r"
ts "configuration complete , waiting for confirmation"

set timeout 20
expect {
         "\[OK\]" { ts "got OK"}
         -re "\n" { exp_continue }
         -re "\r" { exp_continue }
         timeout { ts "timeout waiting for OK after save startup-config" ; send_user "==$expect_out(buffer)==\n" ; exit 1 }
       }

ts "startup configuration saved, waiting for console prompt"

set timeout 5
expect {
         -re "\n\[\[:alnum:\]\]+#$" { ts "console active after configuration" }
         -re ".*\n" { exp_continue }
         -re "\r" { exp_continue }
         timeout { send_user "!" ; send "\r" ; exp_continue }
#         timeout { ts "!" ; send_user "==$expect_out(buffer)==\n" ; send "\r" ; exp_continue }
       }

ts "reload request"
send "\rreload\r"

expect { "[confirm]" { send "\r" } }

ts "reload request accepted"

sleep 2

send "\035"
expect eof

ts "console disconnected, stopping VM"

spawn /usr/bin/virsh destroy $vm 

expect -re "Domain $vm destroyed"
expect eof
ts "bootstrap complete for $vm using $config"
