{

  description = "Graphql Parser";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    let
      ghcVersion = "8107";
      compiler = "ghc${ghcVersion}";
      # default systems compatible with pre-commit-hooks.nix
      # https://github.com/cachix/pre-commit-hooks.nix/pull/122
      defaultSystems = [
        "aarch64-linux"
        # "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to do the evalPkgs trick so that IFD works with `nix flake check`
        # https://github.com/NixOS/nix/issues/4265
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };

        # Our haskell packages override, needs to use evalPkgs because
        # cabal2nix uses IFD
        hsPkgs = evalPkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: {
            graphql-parser = hfinal.callCabal2nix "graphql-parser" ./. { };
          };
        };
      in
      rec {

        # Note: cannot reference anything that depends on `evalPkgs` like `hsPkgs`
        # otherwise non-x86_64-linux users will not be able to build the dev env
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Editor Tooling 
            (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
            cabal-install
            ghc
            ghcid
            cabal2nix

            # Build Deps
            haskellPackages.alex
            haskellPackages.happy
          ];
        };

        packages = flake-utils.lib.flattenTree {
          graphql-parser = hsPkgs.graphql-parser;
        };

        defaultPackage = packages.graphql-parser;
      });
}
