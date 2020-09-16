{ pkgs ? import <nixpkgs> {},
  compiler ? "ghc883" }:
let
  default = import ./default.nix {};
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    mtl
  ]);
in default // pkgs.mkShell {
  name = "happy-learn";
  buildIputs = [ ghc ];
}

