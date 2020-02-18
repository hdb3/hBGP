#!/bin/bash -x

if [[ -d $1 ]] ; then wd="$1"; else wd="/home/nic/src/hBGP.master/testplans" ; fi
if [[ -f $2 ]] ; then script="$2"; else script="script1.sh" ; fi
if [[ -f "$wd/$script" ]] ; then echo "script OK"; else echo "$wd/$script is not a regular file"; exit 1 ; fi

echo "wd is $wd, script is $script"
if tmux kill-session -t test ; then echo "killed old session" ; else echo "no session to kill" ; fi

# gnome-terminal --full-screen -- tmux new-session -s test -c $wd /bin/bash "$wd/$script"
tmux new-session -d -s test -c $wd /bin/bash "$wd/$script"
#tmux set-window-option -g remain-on-exit on
#tmux new-window -t test:1 -c $wd /bin/bash "$wd/$script"
#tmux new-window -t test:1 -c $wd /bin/bash "$wd/$script"
