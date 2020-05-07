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
      rev = "d14fd7335e2a25c7fa1dd700edad7edefd813b38";
      sha256 = "1wj7f9rpi7wqnlvss7gm8p2pb44cyiiivazhaxp36bn2dms8vhn0";
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

    which = hostNixpkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "which";
      rev = "3cf0bfb835732848697173c32696168541648df2";
      sha256 = "0khnczrrcw4ywb750iqfd8v8z2p871b2s6rxbi67inkdc85y1dn4";
    };

    purplechain = gitignoreSource ./purplechain;
  };

  gitignoreSource = (import sources.gitignore {}).gitignoreSource;

  overlay = self: super: {
    haskellPackages = with pkgs.haskell.lib;
      super.haskellPackages.override (old: {
        overrides = self.lib.foldr self.lib.composeExtensions (old.overrides or (_: _: {})) [
          (self: super: {
            purplechain = self.callCabal2nix "purplechain" sources.purplechain {};
            which = self.callCabal2nix "which" sources.which {};
          })
          (self: super: {
            purplechain = overrideCabal super.purplechain (drv: {
              executableSystemDepends = (drv.executableSystemDepends or []) ++ (with pkgs; [iavl tendermint]);
            });
          })
        ];
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
