{ mkDerivation, aeson, base, bytestring, deepseq, hspec
, hspec-discover, matplotlib, mtl, parallel, random, repa, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "happy-learn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring deepseq matplotlib mtl parallel random repa
    transformers vector
  ];
  testHaskellDepends = [
    base hspec hspec-discover matplotlib repa transformers vector
  ];
  testToolDepends = [ hspec-discover ];
  license = stdenv.lib.licenses.mit;
}
