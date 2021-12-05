{ ... }:
let
  sources = import ./nix/sources.nix { };
  nixpkgs = import sources.nixpkgs { };
  haskellDeps = ps: with ps; [ base containers split unordered-containers ];
  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellDeps;
in with nixpkgs;
mkShell {
  buildInputs = [
    ghc
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.ormolu
  ];
}
