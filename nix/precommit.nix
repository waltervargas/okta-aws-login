{ pre-commit-hooks, stylish-haskell }:
pre-commit-hooks.run {
  src = ../.;
  hooks = {
    hlint.enable = true;
    hpack.enable = true;
    nixpkgs-fmt = {
      enable = true;
      excludes = [ "nix/okta-aws-login.materialized/.*" ];
    };
    stylish-haskell-er = {
      enable = true;
      name = "stylish-haskell";
      description = "Run stylish-haskell on haskell files";
      entry = "${stylish-haskell}/bin/stylish-haskell --inplace";
      files = "\\.l?hs$";
    };
  };
}
