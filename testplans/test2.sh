agent1=$(<agent1.pane)
agent2=$(<agent2.pane)
agent3=$(<agent3.pane)
read -p "load initial?"
tmux send-keys -t $agent2 "h 100.64.0.2" Enter "p [100]" Enter "n [172.16.0.99/32]" Enter u Enter
tmux send-keys -t $agent3 "h 100.64.0.3" Enter "p [666]" Enter "n [172.16.0.99/32]" Enter u Enter

read -p "remove 666"

tmux send-keys -t $agent3 "p [999]" Enter u Enter

read -p "reinstate 666"

tmux send-keys -t $agent3 "p [666]" Enter u Enter

read -p "send withdraw?"

tmux send-keys -t $agent3 w Enter

read -p "all done"
#tmux kill-session -t test
