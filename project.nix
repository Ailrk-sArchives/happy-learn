{ mkDerivation, base, deepseq, mtl, parallel, repa, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "happy-learn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deepseq mtl parallel repa transformers vector
  ];
  testHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
