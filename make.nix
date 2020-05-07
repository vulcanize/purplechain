{}:
let
  pkgs = (import ./project.nix {}).pkgs;
  snippets = {
    repl = "cd purplechain && cabal new-repl --repl-options='-ignore-dot-ghci' --repl-options='-ghci-script .ghci' ";
    hoogle = "hoogle server -p 8080 --local";
    build = "nix-build";
    shell = "nix-shell";
    watch = ''ghcid -c "${snippets.repl}" --restart="purplechain/purplechain.cabal" --lint '';
  };

  mkScript = name: text: pkgs.writeScript ("purplechain-${name}") ''
    #!/usr/bin/env bash
    set -euo pipefail
    ${text}
  '';

in {
  build = mkScript "build.sh" snippets.build;
  dev = mkScript "dev.sh" ''
    ${snippets.hoogle} > /dev/null &
    HOOGLE_PID=$!
    ${snippets.watch} --run="testNetwork"
    kill "$HOOGLE_PID"
  '';
  hoogle = mkScript "hoogle.sh" snippets.hoogle;
  repl = mkScript "repl.sh" snippets.repl;
  shell = mkScript "shell.sh" snippets.shell;
  watch = mkScript "watch.sh" snippets.watch;
}
