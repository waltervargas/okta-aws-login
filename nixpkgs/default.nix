{ system ? builtins.currentSystem }:
let
  er-nix = import (
    builtins.fetchGit {
      url = "https://github.com/EarnestResearch/er-nix.git";
      ref = "refs/heads/master";
      # git ls-remote git@github.com:EarnestResearch/er-nix refs/heads/master | awk '{ print "rev = \""$1"\";" }'
      rev = "56738b126f912d0618869ce32ed2d01e3da90fa2";
    }
  );
in
er-nix.pkgsForSystem (system)
