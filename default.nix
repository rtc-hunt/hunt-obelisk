{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  packages = {zoomeval-api = ./zoomeval-api; 
    zoomeval = ./zoomeval;
    hunttools = ./hunttools;
    hunttools-dicts-if = ./hunttools-dicts-if;
    packed-dawg-big = ./packed-dawg-big;
    servant-snap = ./servant-snap;
    reflex-dom-ace = ./reflex-dom-ace;
  };
  # shells.ghc = ["hunttools" "hunttools-dicts-if" "zoomeval" "backend" "frontend"];
  overrides = self: super: {
    servant-reflex = nixpkgs.haskell.lib.doJailbreak super.servant-reflex;
#    reflex-dom-ace = nixpkgs.fetchFromGitHub
#       { owner = "reflex-frp"; 
#         repo = "reflex-dom-ace"; 
#         rev = "22350114b0f21c0d7542af3654652f1d329ef970"; 
#         sha256 = "10ldq69sp0wqlxsl0pyklnnj51lx8mk3ihkd6zjffbdp95146fi2"; 
#       }; 
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
