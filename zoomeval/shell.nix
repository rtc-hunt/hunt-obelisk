{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc, hint, http-client, http-types
      , optparse-applicative, servant, servant-client, servant-lucid
      , servant-server, stdenv, stm, transformers, unix, wai, warp
      }:
      mkDerivation {
        pname = "zoomeval";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base servant servant-lucid ];
        executableHaskellDepends = [
          base ghc hint http-client http-types optparse-applicative servant
          servant-client servant-lucid servant-server stm transformers unix
          wai warp
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
