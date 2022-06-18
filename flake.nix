{
  description = "Graphql Parser";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;

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
        "aarch64-darwin"
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
            haskell.packages."${compiler}".haskell-language-server
            cabal-install
            ghc
            ghcid
            cabal2nix

            # Build Deps
            haskell.packages.${compiler}.alex
            haskell.packages.${compiler}.happy
          ];
        };

        packages = flake-utils.lib.flattenTree {
          graphql-parser = hsPkgs.graphql-parser;
        };

        # TODO: Get golden tests in scope so we don't have to skip tests here:
        defaultPackage = pkgs.haskell.lib.compose.dontCheck packages.graphql-parser;
      });
}
