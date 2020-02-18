tmux splitw -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent1.conf
tmux splitw -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent2.conf
tmux splitw -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent3.conf

read -p "load initial?"
tmux send-keys -t test:0.1 "h 10.0.0.2" Enter "p [100]" Enter "n [172.16.0.99/32]" Enter u Enter
tmux send-keys -t test:0.2 "h 10.0.0.3" Enter "p [666]" Enter "n [172.16.0.99/32]" Enter u Enter

read -p "remove 666"

tmux send-keys -t test:0.2 "p [999]" Enter u Enter

read -p "reinstate 666"

tmux send-keys -t test:0.2 "p [666]" Enter u Enter

read -p "send withdraw?"

tmux send-keys -t test:0.2 w Enter

read -p "all done"
tmux kill-session -t test
