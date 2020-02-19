tmux kill-session -t test || :
tmux new-session -d -s test
tmux set mouse on
# tmux set -t test:0 remain-on-exit on
tmux neww -d -t test:0 -c /home/nic/src/hBGP.master/testplans bash test2.sh
tmux splitw -P -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent1.conf > agent1.pane
tmux splitw -P -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent2.conf > agent2.pane
tmux splitw -P -d -t test:0 -c /home/nic/src/hBGP.master/ stack run testplans/agent3.conf > agent3.pane
tmux attach -t test:0

# read -p "load initial?"
# tmux send-keys -t test:0.1 "h 100.64.0.2" Enter "p [100]" Enter "n [172.16.0.99/32]" Enter u Enter
# tmux send-keys -t test:0.2 "h 100.64.0.3" Enter "p [666]" Enter "n [172.16.0.99/32]" Enter u Enter
# 
# read -p "remove 666"
# 
# tmux send-keys -t test:0.2 "p [999]" Enter u Enter
# 
# read -p "reinstate 666"
# 
# tmux send-keys -t test:0.2 "p [666]" Enter u Enter
# 
# read -p "send withdraw?"
# 
# tmux send-keys -t test:0.2 w Enter
# 
# read -p "all done"
# tmux kill-session -t test
