(import ./nixpkgs-commit.nix {
  overlays = [ (import ./overlays/ghcide-nixpkgs-20.03.nix) ];
}).haskellPackages.ghcide
