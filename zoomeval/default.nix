{ mkDerivation, aeson, base, ghc, hint, http-client, http-types
, lucid, optparse-applicative, servant, servant-client
, servant-lucid, servant-server, stdenv, stm, transformers, unix
, wai, warp
}:
mkDerivation {
  pname = "zoomeval";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base servant servant-lucid ];
  executableHaskellDepends = [
    base ghc hint http-client http-types lucid optparse-applicative
    servant servant-client servant-lucid servant-server stm
    transformers unix wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
