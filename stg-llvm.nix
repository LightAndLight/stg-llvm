{ mkDerivation, base, containers, hedgehog, llvm-hs, llvm-hs-pretty
, llvm-hs-pure, mtl, process, stdenv, text
}:
mkDerivation {
  pname = "stg-llvm";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers llvm-hs llvm-hs-pure mtl
  ];
  testHaskellDepends = [
    base hedgehog llvm-hs llvm-hs-pretty llvm-hs-pure mtl process text
  ];
  license = stdenv.lib.licenses.bsd3;
}
