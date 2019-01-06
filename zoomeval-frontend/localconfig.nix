{
  allowBroken = true;
  packageOverrides = super: 
  let self = super.pkgs;
  in
  rec {
    ghcjsHaskellPackages = super.haskell.packages.ghcjsHEAD.override {
        overrides = (import ./localhaskell.nix super.pkgs).ghcjsPackages;
    };
    # haskell.packages.ghcjsHEAD = ghcjsHaskellPackages;
    haskellPackages = super.haskell.packages.ghcjsHEAD.override {
        overrides = (import ./localhaskell.nix super.pkgs).ghcjsPackages;
    };
  };
}
