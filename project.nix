{ }:
let
  hostNixpkgs = import <nixpkgs> {};

  sources = {
    pinnedNixpkgs = builtins.fetchTarball {
      name = "release-19.09";
      url = https://github.com/nixos/nixpkgs/archive/64a3ccb852d4f34abb015503affd845ef03cc0d9.tar.gz;
      sha256 = "0jigsyxlwl5hmsls4bqib0rva41biki6mwnswgmigwq41v6q7k94";
    };

    purple = hostNixpkgs.fetchFromGitHub {
      owner = "vulcanize";
      repo = "purple";
      rev = "8047c026109fe92ff4640e82b18d67a12a760b0a";
      sha256 = "12wm357lkpdak0a6d5bbxla8d4scvfjh5mmrraia3la8kd0b8mbr";
    };

    kepler = hostNixpkgs.fetchFromGitHub {
      owner = "f-o-a-m";
      repo = "kepler";
      rev = "d042c3a780a75d67e5b42fbc3b56a8992443b998";
      sha256 = "0pjhg203lrvz9rji8iwfpfsdvqxjnygm4m7jrd030zxjkhjngfqy";
    };

    gitignore = hostNixpkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
      sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
    };

    purplechain = gitignoreSource ./.;
  };

  gitignoreSource = (import sources.gitignore {}).gitignoreSource;

  overlay = self: super: {
    haskellPackages =
      super.haskellPackages.override (old: {
        overrides = self.lib.composeExtensions
          (old.overrides or (_: _: {}))
          (self: super: {
            purplechain = self.callCabal2nix "purplechain" sources.purplechain {};
          });
      });
  };

  overlays = builtins.concatLists [
    (import (sources.purple + /project.nix) {}).overlays
    (import (sources.kepler + /default.nix) {}).overlays
    [overlay]
  ];

  pkgs = import sources.pinnedNixpkgs { inherit overlays; };

in {
  inherit pkgs overlays;
}
