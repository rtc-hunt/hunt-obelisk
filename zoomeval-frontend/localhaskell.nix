pkgs: let super = pkgs;
in
with import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; inherit (pkgs) lib;};
{
    ghcjsPackages = hself: lsuper: with hself; rec {
              "reflex" = doJailbreak (hself.callPackage /home/jonored/haac/reflex {});
              "reflex-dom" = (hself.callPackage /home/jonored/haac/reflex-dom/reflex-dom {});
              "reflex-dom-core" = (hself.callPackage /home/jonored/haac/reflex-dom/reflex-dom-core { jsaddle = jsaddle; });
              "reflex-dom-contrib" = doJailbreak (hself.callPackage /home/jonored/haac/reflex-dom-contrib {});
              "jsaddlenot" = lsuper.jsaddle.overrideAttrs (oldAttrs: rec {
                              libraryHaskellDepends = [
                              aeson attoparsec base base64-bytestring bytestring containers
                              deepseq filepath ghc-prim http-types lens primitive process random
                              ref-tf scientific stm text time transformers unordered-containers
                              vector ghcjs-base
                              ];
                        });
               "jsaddle" = hself.callPackage ./jsaddle-0.9.4.0.nix { };
               "reflex-dom-helpers" = hself.callPackage ./reflex-dom-helpers { };
               "free" = dontHaddock lsuper.free;
               "fail" = dontHaddock lsuper.fail;
               "bytestring-builder" = dontHaddock lsuper.bytestring-builder;
               "zoomeval-api" = hself.callPackage ../../zoomeval-api { };
               "servant-reflex" = hself.callPackage ./servant-reflex { };
               "frontend" = hself.callPackage ./frontend.nix { };
#              "jsaddle" = hself.callPackage ./jsaddle-0.8.3.0.nix { };
#              "ghcjs-dom" = hself.callPackage ./ghcjs-dom-0.8.0.0.nix { };
#              "ghcjs-dom-jsffi" = hself.callPackage ./ghcjs-dom-jsffi-0.8.0.0.nix { };
    };
}

