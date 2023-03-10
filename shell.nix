{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc94"}:
with nixpkgs;
let
  haskellPackages = (if   compiler == "default"
                     then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override {
    overrides = self: super: {
    };
  };
  developPackage = haskellPackages.developPackage { root = ./.; };
  hoogle         = haskellPackages.ghcWithHoogle (hs: with hs;
                     [ ]);
in
  developPackage.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      # hoogle
      nixpkgs.hlint
    ];
  })
