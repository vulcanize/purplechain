{}:
let
  pkgs = (import ./project.nix {}).pkgs;
  snippets = {
    repl = "cabal new-repl";
    hoogle = "hoogle server -p 8080 --local";
    build = "nix-build";
    watch = "ghcid -c '${snippets.repl}' --restart='purplechain.cabal' --lint";
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
    ${snippets.watch} --run='test'
    kill "$HOOGLE_PID"
  '';
  hoogle = mkScript "hoogle.sh" snippets.hoogle;
  repl = mkScript "repl.sh" snippets.repl;
  watch = mkScript "watch.sh" snippets.watch;
}
