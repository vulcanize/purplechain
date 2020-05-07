{}:
let pkgs = (import ./project.nix {}).pkgs;
in with pkgs.haskellPackages; shellFor {
  withHoogle = true;
  packages = p: [ p.purplechain ];
  nativeBuildInputs = [ cabal-install ghcid hlint ] ++ (with pkgs; [iavl tmux]);
}
