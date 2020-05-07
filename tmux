#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i bash

echo "Setting up tmux..."
SERVER="purplechain-server"
SESSION="purplechain-session"
TMUX="tmux -L $SERVER"

echo "Removing pre-existing session..."
$TMUX kill-session -t $SESSION
$TMUX new -d -s $SESSION
$TMUX detach

set -euo pipefail

echo "Building purplechain... might take a while if not cached..."
nix-build

echo "Setting up panels..."
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

echo "Setting up prompts..."
TEMP=$(mktemp -d /tmp/foo.XXXX)

$TMUX select-pane -t 0
$TMUX send-keys "$(nix-build)/bin/purplechain ${TEMP}/node0"

$TMUX select-pane -t 1
$TMUX send-keys "$(nix-build)/bin/purplechain ${TEMP}/node1"

$TMUX select-pane -t 2
$TMUX send-keys "$(nix-build)/bin/purplechain ${TEMP}/node2"

$TMUX select-pane -t 3
$TMUX send-keys "$(nix-build)/bin/purplechain ${TEMP}/node3"

$TMUX select-pane -t 4
$TMUX send-keys 'make repl' ENTER
$TMUX send-keys 'import qualified Network.Tendermint.Client as RPC' ENTER
$TMUX send-keys 'import Purplechain.Client' ENTER
$TMUX send-keys ENTER
$TMUX send-keys "nodes@[node0, node1, node2, node3] <- fmap mkPurplechainNode <$> initNetwork \"${TEMP}\" 4" ENTER
$TMUX send-keys "testScenario nodes"

echo "Attaching..."
$TMUX attach
