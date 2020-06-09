let project = import ./project.nix {};
in with project; {
  inherit (pkgs) docker-compose tmux;
  inherit (pkgs.haskellPackages) purplechain;
  inherit dockerImage shell;
}
