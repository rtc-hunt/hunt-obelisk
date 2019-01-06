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
  };
  # shells.ghc = ["hunttools" "hunttools-dicts-if" "zoomeval" "backend" "frontend"];
  overrides = self: super: {
    servant-reflex = nixpkgs.haskell.lib.doJailbreak super.servant-reflex;
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
