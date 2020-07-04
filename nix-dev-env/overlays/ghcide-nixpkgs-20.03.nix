pself: psuper: {
  haskellPackages = psuper.haskellPackages.override {
    overrides = let
        dc = psuper.haskell.lib.dontCheck;
    in self: super: {
      ghcide = dc (self.callCabal2nix "ghcide" (fetchTarball "https://github.com/digital-asset/ghcide/tarball/v0.2.0") {});
      ghc-check = dc (self.callCabal2nix "ghc-check" (fetchTarball "https://hackage.haskell.org/package/ghc-check-0.3.0.1/ghc-check-0.3.0.1.tar.gz") {});
      haddock-library = dc (self.callCabal2nix "haddock-library" (fetchTarball "http://hackage.haskell.org/package/haddock-library-1.9.0/haddock-library-1.9.0.tar.gz") {});
      hie-bios = dc (self.callCabal2nix "hie-bios" (fetchTarball "http://hackage.haskell.org/package/hie-bios-0.5.1/hie-bios-0.5.1.tar.gz") {});
      haskell-lsp = dc (self.callCabal2nix "haskell-lsp" (fetchTarball "http://hackage.haskell.org/package/haskell-lsp-0.22.0.0/haskell-lsp-0.22.0.0.tar.gz") {});
      haskell-lsp-types = dc (self.callCabal2nix "haskell-lsp-types" (fetchTarball "http://hackage.haskell.org/package/haskell-lsp-types-0.22.0.0/haskell-lsp-types-0.22.0.0.tar.gz") {});
      regex-tdfa = dc (self.callCabal2nix "regex-tdfa" (fetchTarball "http://hackage.haskell.org/package/regex-tdfa-1.3.1.0/regex-tdfa-1.3.1.0.tar.gz") {});
      regex-base = dc (self.callPackage ../../vendored/regex-base-0.94.0.0 {});
      regex-posix = dc (self.callPackage ../../vendored/regex-posix-0.96.0.0 {});
      monoid-subclasses = dc super.monoid-subclasses;
    };
  };
}
