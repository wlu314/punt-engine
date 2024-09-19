{ pkgs ? import <nixpkgs> { overlays = [ (import ./overlay.nix ) ]; } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.clash-ghc
    pkgs.haskellPackages.cabal-install
  ];
}
