all: build

# Build everything
build:
	./make build

# Launch a network in a temporary directory, and run each node in a separate thread
# Auto-reloads on changes to source
# Runs a hoogle server in the background
dev:
	./make dev

# Launch a GHCi repl for interactive evaluation/execution of nodes/networks
repl:
	./make repl

# Launch a nix-shell with all dependencies for the project setup in the environment
shell:
	./make shell

# Auto-reloads on changes to source, checks for errors and warnings
watch:
	./make watch

# Launches a hoogle server on http://localhost:8080/ for name and type search across the dependency set, as well as browsing haddock documentation
hoogle:
	./make hoogle

# Run a tmux session with a node on each pane, plus a repl node for broadcasting transaction/queries
tmux: force
	./tmux

force:
