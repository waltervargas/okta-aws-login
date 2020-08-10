let
  pkgs = import ./nixpkgs {};
  hsPkgs = import ./default.nix { inherit pkgs; };
in
hsPkgs.shellFor {
  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "3.2.0.0";
    ghcide = "0.2.0";
  };

  buildInputs = with pkgs;
    [
      pkgs.haskellPackages.hpack.out # same version is used by Git commit hook
      git
      gnumake
      ghcid
      zlib
    ];

  exactDeps = true;

  shellHook = ''
    ${hsPkgs.pre-commit-check.shellHook}
    export LANG=en_US.UTF-8
  '';
}
