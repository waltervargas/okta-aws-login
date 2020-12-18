{ system ? builtins.currentSystem }:
let
  sources = import ../nix/sources.nix;
  er-nix = import sources.er-nix;
in
er-nix.pkgsForSystem (system)
