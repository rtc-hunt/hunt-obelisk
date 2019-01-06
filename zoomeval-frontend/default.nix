{ nixpkgs ? import <nixpkgs> { } }: 
let 
        haskellGhcjsPackages = (import ./localhaskell.nix nixpkgs.pkgs).ghcjsPackages ;
in
        (nixpkgs.haskell.packages.ghcjsHEAD.override { overrides = haskellGhcjsPackages; }).frontend
