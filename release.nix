let
  pkgs = import <nixpkgs> {};
  # inherit (nixpkgs) pkgs;
  #   ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #     http-conduit aeson mtl
  #   ]);
in
  pkgs.haskellPackages.callPackage ./default.nix { }
