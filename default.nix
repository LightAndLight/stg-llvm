{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./stg-llvm.nix;

  haskellPackages =
    (if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler}).override {
       overrides = self: super: {
         llvm-hs-pure = super.callPackage ./nix/llvm-hs-pure.nix {};
         llvm-hs = self.callPackage ./nix/llvm-hs.nix {
           llvm-config = pkgs.llvm_5;
         };
         llvm-hs-pretty = pkgs.haskell.lib.dontCheck super.llvm-hs-pretty;
         happy = pkgs.haskell.lib.dontCheck super.happy;
       };
     };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  drv
