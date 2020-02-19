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
        -re "Would you like to enter the initial configuration dialog" { ts "phase 1 complete"; send "no\r\n" }
        -re ".*\n" { send "\r" ; send_user "." ; exp_continue -continue_timer }
         timeout { ts " timed out (120 seconds)\n" ; exit 1 }
       }


ts "move to phase 2"

set timeout 60
send "\r"
expect {
         -re "Press RETURN to get started" { send "\r"; exp_continue -continue_timer }
         -re "Router>" {  send "enable\r" } 
         -re "\n" { send "\r" ; send_user . ; exp_continue -continue_timer}
         timeout {  ts "phase 2 timeout" ; exit 1 }
       }

expect {
         -re "Router#" {  ts "enter privelege mode"} 
         -re "\n" { send "\r" ; send_user . ; exp_continue -continue_timer}
         timeout {  ts "phase 3 timeout" ; exit 1 }
       }

expect *
ts "initialisation complete , sending config $config"
log_user 0
send "\rterminal no monitor\rconfig terminal\r"

send "$data"
sleep 1
expect *
send "\r\rwrite memory\r\r"

set timeout 20
expect {
         "\[OK\]" { ts "got OK"}
         -re "\n" { exp_continue }
         -re "\r" { exp_continue }
         timeout { ts "phase 4 timeout" ; send_user "==$expect_out(buffer)==\n" ; exit 1 }
       }

ts "sending config complete, waiting for console prompt"

set timeout 5
expect {
         -re "\n\[\[:alnum:\]\]+#$" { ts "Done" }
         -re ".*\n" { exp_continue }
         -re "\r" { exp_continue }
         timeout { send_user "!" ; send "\r" ; exp_continue }
#         timeout { ts "!" ; send_user "==$expect_out(buffer)==\n" ; send "\r" ; exp_continue }
       }

# interact 

sleep 2

ts "reload request"
sleep 2
send "\rreload\r"

expect { "[confirm]" { send "\r" } }

ts "reload request accepted"

sleep 2

send "\035"
ts "console disconnected"

# spawn /usr/bin/virsh destroy $vm 
