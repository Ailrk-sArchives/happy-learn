{ mkDerivation, base, deepseq, hspec, hspec-discover, mtl, parallel
, repa, stdenv, transformers, vector
}:
mkDerivation {
  pname = "happy-learn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base deepseq mtl parallel repa transformers vector
  ];
  testHaskellDepends = [ base hspec hspec-discover ];
  testToolDepends = [ hspec-discover ];
  license = stdenv.lib.licenses.mit;
}
