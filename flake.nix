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
      overlay = import ./nix/overlay.nix;
      overlays = [ overlay ];
    in
    flake-utils.lib.eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      rec {
        devShell = pkgs.haskell.packages.${compiler}.shellFor {
          packages = p: [ p.graphql-parser ];
          buildInputs = with pkgs; [
            haskell.packages."${compiler}".haskell-language-server
            cabal-install
            ghcid
          ];
        };

        packages = flake-utils.lib.flattenTree {
          graphql-parser = pkgs.haskell.packages.${compiler}.graphql-parser;
        };

        defaultPackage = packages.graphql-parser;
      });
}
