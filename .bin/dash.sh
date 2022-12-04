#!/bin/bash

tmux rename-session "dash"
tmux split-window -h -p 65 
tmux split-window -h -p 40
tmux split-window -t 1 -v -p 50

tmux send-keys -t 3 "burndown" C-m
tmux send-keys -t 2 "urgent" C-m
tmux send-keys -t 1 "ttoday" C-m
