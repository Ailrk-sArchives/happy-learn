{ mkDerivation, base, hmatrix, stdenv }:
mkDerivation {
  pname = "happy-learn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hmatrix ];
  testHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
