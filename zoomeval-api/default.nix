{ mkDerivation, aeson, base, servant, servant-lucid, stdenv }:
mkDerivation {
  pname = "zoomeval-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base servant servant-lucid ];
  license = stdenv.lib.licenses.bsd3;
}
