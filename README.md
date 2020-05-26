# purplechain
[Maker Purple paper](https://makerdao.com/purple/) on [Tendermint](https://tendermint.com/core/)

Purplechain is a port of Maker Purple that runs on top of Tendermint consensus. It integrates with the [Cosmos SDK](https://docs.cosmos.network/master/) [`auth`](https://docs.cosmos.network/master/modules/auth/) and [`bank`](https://docs.cosmos.network/master/modules/bank/) modules by using their equivalents from the [Kepler SDK](https://github.com/f-o-a-m/kepler/).

This repo provides
- a Purplechain Cosmos SDK module
- a Purplechain node binary
- a docker image for purplechain nodes
- a tmux script for running a network

## Demo
An [asciinema](https://asciinema.org/) recording is available showcasing the devnet.
The tmux session shown has one panel for each node (run in its own Docker container), where the middle-right pane is the only non-validator node.
The bottom panel initializes the devnet and is used for issuing transactions/queries.

[![asciicast](https://asciinema.org/a/eXBTxfr5GrAG5JKmjxT7hG78V.svg)](https://asciinema.org/a/eXBTxfr5GrAG5JKmjxT7hG78V)

## Dependencies
### Docker
You will need to have Docker installed and configured on your machine if you want to run each node in a separate container.

### Nix
The Nix package manager is required for building or working on this project. You can get it from its [homepage](https://nixos.org/nix/).
All other dependencies (except Docker) are handled by Nix. Note that Nix does not yet work on Windows - you will need either a virtual machine or [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

## Development
For development purposes, it is possible to simply execute each node in a separate thread listening on localhost ports.
Once you have Nix installed, you can run `make dev` to get a working environment with a hoogle server and an auto-reloading devnet.

You can also run `nix-shell` to enter a nix shell with dependencies for the project setup in the environment, and then use a more custom workflow.

If you have Docker setup, you can then run `make tmux` for a "production-like" environment.

For more fine-grained commands, see the [Makefile](Makefile).
