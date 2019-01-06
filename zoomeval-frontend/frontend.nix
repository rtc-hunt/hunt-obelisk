{ mkDerivation, aeson, base, bytestring, containers, data-default
, ghcjs-base, ghcjs-dom, lens, mtl, reflex, reflex-dom
, reflex-dom-contrib, reflex-dom-helpers, semigroups, servant
, servant-reflex, stdenv, text, transformers, zoomeval-api
}:
mkDerivation {
  pname = "zoomeval-frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default ghcjs-base ghcjs-dom
    lens mtl reflex reflex-dom reflex-dom-contrib reflex-dom-helpers
    semigroups servant servant-reflex text transformers zoomeval-api
  ];
  license = stdenv.lib.licenses.bsd3;
}
