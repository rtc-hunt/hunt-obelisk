{ stdenv, makeWrapper, haskellPackages, packages ? (pkgs: []), staticSite
}:
  let defaultPkgs = pkgs: [ pkgs.show
                            pkgs.simple-reflect
                            pkgs.QuickCheck
                            pkgs.mtl
                            ];
     env = haskellPackages.ghcWithPackages (pkgs: defaultPkgs pkgs ++ packages pkgs);
     libDir = "${env}/lib/ghc-${env.version}";
  in
  stdenv.mkDerivation {
    name = "zoomeval-env";
    inherit (haskellPackages) zoomeval;
    inherit staticSite;
    nativeBuildInputs = [ makeWrapper ];
    buildCommand = ''
      mkdir -p $out/bin
      makeWrapper $zoomeval/bin/mueval $out/bin/mueval
      makeWrapper $zoomeval/bin/zoomeval $out/bin/zoomeval \
        --set "NIX_GHC_LIBDIR" "${libDir}" \
        --set "ZE_SITE" "${staticSite}/bin/zoomeval-frontend.jsexe"
    '';
    passthru = { inherit defaultPkgs; };
  }
 

