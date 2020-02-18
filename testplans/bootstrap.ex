#!/usr/bin/expect
set vm [lindex $argv 0]
set config [lindex $argv 1]
set f [open $config]
set data [read $f]
log_user 0
#log_file -noappend bootstrap.log

proc ts {} {
  set s [timestamp -format %X]
  send_user "$s "
}


spawn /usr/bin/virsh start $vm
expect "Domain $vm started"

ts

send_user "domain started\n"

spawn /usr/bin/virsh console $vm

expect { "Escape character is ^]" { ts ; send_user "console connected\n" } 
         -re ".*\n" { send_user . }
         timeout { send_user ~ ; exp_continue }
       }

ts

send_user "waiting for config dialog\n"

expect {
        -re "Would you like to enter the initial configuration dialog" { ts ; send_user "phase 1 complete\n"; send "no\r\n" }
        -re ".*\n" { send_user "." ; exp_continue }
         timeout { send_user ~ ; exp_continue }
       }

ts

send_user "\nmove to phase 2\n"

expect {
         -re "Press RETURN to get started" {ts ; send "\r\n"; exp_continue }
         -re "Router>" {ts ; send_user "initialisation complete\n"}
         -re "\n" { send_user . ; exp_continue }
         timeout { ts ; send_user ^ ; send "\r\n" ; exp_continue }
       }

ts

send_user "login complete, sending config $config\n"
log_user 1
send "\renable\r\rconfig terminal\r"

send "$data"
send "\r\rwrite memory\r\r"

ts

send_user "config complete\n"

expect {
         -re "# *\n" { ts ; send_user "Done\n" }
         -re "#\n" { ts ; send_user "Done\n" }
         -re "\n" { send_user + ; exp_continue }
         timeout { ts ; send_user "==$exp_output(buffer)==\n" ; send "\r\n" ; exp_continue }
       }

# interact 

sleep 2

ts
send_user "reload request\n"
sleep 2
send "\rreload\r"

expect { "[confirm]" { send "\r" } }

sleep 2

send "\035"

# spawn /usr/bin/virsh destroy $vm 
