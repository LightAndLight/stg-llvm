{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix { inherit nixpkgs compiler; };

in

  f.env
