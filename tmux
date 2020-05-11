#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i bash

echo $'\nSetting up tmux...'
SERVER="purplechain-server"
SESSION="purplechain-session"
TMUX="tmux -L $SERVER"

echo $'\nRemoving pre-existing session...'
$TMUX kill-session -t $SESSION
$TMUX new -d -s $SESSION
$TMUX detach

set -euo pipefail

echo $'\nBuilding purplechain... might take a while if not cached...'
nix-build

echo $'\nBuilding docker image... might take a while if not cached...'
nix-build project.nix -A dockerImage

echo $'\nLoading docker image...'
docker load -i $(nix-build project.nix -A dockerImage)

echo $'\nSetting up panels...'
# 0 1
# 2 3
#  4

$TMUX split-window -p 20
$TMUX select-pane -t top
$TMUX split-window -p 50
$TMUX split-window -h -p 50
$TMUX select-pane -t top
$TMUX split-window -h -p 50

$TMUX set -g mouse on

echo $'\nSetting up prompts...'
TEMP=$(mktemp -d /tmp/foo.XXXX)

$TMUX select-pane -t 0
$TMUX send-keys "docker-compose run -v ${TEMP}/node0:/tendermint --service-ports node0"

$TMUX select-pane -t 1
$TMUX send-keys "docker-compose run -v ${TEMP}/node1:/tendermint --service-ports node1"

$TMUX select-pane -t 2
$TMUX send-keys "docker-compose run -v ${TEMP}/node2:/tendermint --service-ports node2"

$TMUX select-pane -t 3
$TMUX send-keys "docker-compose run -v ${TEMP}/node3:/tendermint --service-ports node3"

$TMUX select-pane -t 4
$TMUX send-keys 'make repl' ENTER
$TMUX send-keys ENTER
$TMUX send-keys "nodes@[node0, node1, node2, node3] <- fmap (mkPurplechainNode NodeEnvironment_Container) <$> initNetwork NodeEnvironment_Container \"${TEMP}\" 4" ENTER
$TMUX send-keys "testScenario nodes"

echo $'\nAttaching...'
$TMUX attach
