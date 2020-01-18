{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, obelisk ? import ./.obelisk/impl { inherit system iosSdkVersion; config = { allowBroken = true; }; }
}:
with obelisk;
let 
  servantSrc = nixpkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "b17c8bb8bd59ef8341bad07f9f7d0e230603612b";
    sha256 = "0dyn50gidzbgyq9yvqijnysai9hwd3srqvk8f8rykh09l375xb9j";
  };
  lib = nixpkgs.haskell.lib;
 proj = project ./. ({ hackGet, ... }: {
  packages = {zoomeval-api = ./zoomeval-api; 
    zoomeval = ./zoomeval;
    hunttools = hackGet ./hunttools;
    hunttools-dicts-if = hackGet ./hunttools-dicts-if;
    packed-dawg-big = hackGet ./packed-dawg-big;
    # servant-snap = hackGet ./servant-snap;
    sheetwalker = ./sheetwalker;
    gogol-core = (hackGet ./dep/gogol) + "/core";
    gogol = (hackGet ./dep/gogol) + "/gogol";
    gogol-sheets = (hackGet ./dep/gogol) + "/gogol-sheets";
    gogol-drive = (hackGet ./dep/gogol) + "/gogol-drive";
  };
  # shells.ghc = ["hunttools" "hunttools-dicts-if" "zoomeval" "backend" "frontend"];
  overrides = let globalOver = name: value: value; # nixpkgs.haskell.lib.appendConfigureFlags value ["--enable-executable-dynamic" "--enable-shared" "--ghc-options=-dynamic"];
    in self: super: nixpkgs.lib.mapAttrs globalOver (super // {
    servant-reflex = nixpkgs.haskell.lib.doJailbreak super.servant-reflex;
    lens-aeson = nixpkgs.haskell.lib.dontCheck super.lens-aeson;
    http-media = nixpkgs.haskell.lib.dontCheck super.http-media;
    servant = lib.overrideCabal (lib.dontCheck super.servant) (old: {
      postInstall = "";
    });
    servant-snap = lib.doJailbreak (lib.dontCheck super.servant-snap);
    # servant = self.callCabal2nix "servant" "${servantSrc}/servant" { };
    #servant = self.callHackage "servant" "0.12.1" { };
    #servant-client = self.callHackage "servant-client" "0.12.0.1" { };
    #servant-client-core = self.callHackage "servant-client-core" "0.12" { };
    #servant-server = self.callHackage "servant-server" "0.12" { };
    mueval = nixpkgs.haskell.lib.overrideCabal super.mueval (_: { enableSharedExecutables = true; configureFlags = ["--enable-executable-dynamic" "--ghc-option=-dynamic"]; });
    zoomeval = nixpkgs.haskell.lib.overrideCabal super.zoomeval (_: { enableSharedExecutables = true; });
    backend = nixpkgs.haskell.lib.overrideCabal super.backend (_: { enableSharedExecutables = true; configureFlags = ["--enable-executable-dynamic" "--ghc-option=-dynamic"]; });
  });
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  # __closureCompilerOptimizationLevel = "WHITESPACE_ONLY";
});
 envPkgs = (pkgs: with pkgs; [hunttools hunttools-dicts-if aeson show simple-reflect QuickCheck mtl]);
 env = proj.ghc.ghcWithHoogle envPkgs;
 libDir = "${env}/lib/ghc-${env.version}";
 dicts = nixpkgs.callPackage ./hunttools-dicts-nonhask.nix { };
in 
  proj // rec {
    hooglePathModule = {hostName, ...}: {
          services.hoogle = { packages = envPkgs; enable=true; haskellPackages=proj.ghc; };
          services.nginx = {
            enable = true;
            virtualHosts = {
              "${hostName}" = {
                           locations."/hoogle/" = {
                                   proxyPass = "http://localhost:8080/";
                                   extraConfig = ''
                                     location ~ /hoogle/file/.*\.\. {
                                       deny all;
                                     }
                                     location ~ /hoogle/(file/nix/store/.*/share/doc/.*/html/)$ {
                                       proxy_pass http://localhost:8080/$1;
                                     }
                                     location ~ /hoogle/(file/nix/store/.*/share/doc/.*/html/.*)$ {
                                       proxy_pass http://localhost:8080/$1;
                                     }
                                     location /hoogle/file/ {
                                       deny all;
                                     }
                                   '';
                           };
            };

            "huntlive.tcita.com" = {
              globalRedirect = "hunt.tcita.com";
            };
          };
        };
    };
    serverImpl = { exe, hostName, adminEmail, routeHost, enableHttps, version }@args:
    let
      nixos = import (nixpkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (obelisk.serverModules.mkBaseEc2 args)
          (obelisk.serverModules.mkObeliskApp args)
          (hooglePathModule args)
        ];
      };
    };
    server = args@{ hostName, adminEmail, routeHost, enableHttps, version }:
      serverImpl (args // { exe = wrapLinuxExe (proj.linuxExeConfigurable version); });

    linExe = wrapLinuxExe (proj.linuxExeConfigurable "dummyVersion");
    wrapLinuxExe = obPackage: nixpkgs.symlinkJoin { name = "linuxExeWithPaths"; paths = [proj.linuxExe]; nativeBuildInputs = [nixpkgs.makeWrapper]; postBuild = ''
      ln -sft $out/ '${obPackage}'/*
      rm $out/backend
      makeWrapper ${proj.ghc.backend}/bin/backend $out/backend --set "NIX_GHC_LIBDIR" "${libDir}" --set "ZE" "${zeExe}/bin/zoomeval" --set "HUNTTOOLS_DICTS_DIR" "${dicts}/"
      ln -s ${env}/bin/hoogle $out/hoogle
  '';}; 
    zeExe = nixpkgs.symlinkJoin { name = "linuxExeWithPaths"; paths = [ proj.ghc.mueval proj.ghc.zoomeval ]; nativeBuildInputs = [nixpkgs.makeWrapper]; postBuild = ''
      makeWrapper ${proj.ghc.mueval}/bin/mueval-core $out/backend-new --set "NIX_GHC_LIBDIR" "${libDir}" --set "HUNTTOOLS_DICTS_DIR" "${dicts}/"
      makeWrapper ${proj.ghc.zoomeval}/bin/zoomeval $out/zeBase --set "NIX_GHC_LIBDIR" "${libDir}" --set "HUNTTOOLS_DICTS_DIR" "${dicts}/"
      '';}; 
    }
