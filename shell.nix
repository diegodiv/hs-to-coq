{ ghcVersion  ? "ghc884"

, rev    ? "4c2e7becf1c942553dadd6527996d25dbf5a7136"
, sha256 ? "10dzi5xizgm9b3p5k963h5mmp0045nkcsabqyarpr7mj151f6jpm"

, nixpkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
}:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (ps: with ps; [
          ghc-typelits-knownnat
          ghc-typelits-natnormalise
          ghc-typelits-extra
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc pkgs.cabal-install pkgs.stack ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
