final: prev:

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghc8107 = prev.haskell.packages.ghc8107.override {
        overrides = hfinal: hprev:
          with prev.haskell.lib.compose; {
            graphql-parser = dontCheck (hprev.callCabal2nix "graphql-parser" ../. { });
          };
      };
    };
  };
}

