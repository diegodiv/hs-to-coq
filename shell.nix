{ ghcVersion  ? "ghc884"

, rev    ? "4c2e7becf1c942553dadd6527996d25dbf5a7136"
, sha256 ? "10dzi5xizgm9b3p5k963h5mmp0045nkcsabqyarpr7mj151f6jpm"

, nixpkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
  }
}:
let
  inherit (nixpkgs) pkgs;
  dontOptimize = drv: pkgs.haskell.lib.appendConfigureFlag drv "--ghc-option=-O0";
  haskellPkgs = pkgs.haskell.packages.${ghcVersion}.extend (hself: hsuper: {
    singletons = hself.callHackage "singletons" "2.6" {};
    clash-prelude = pkgs.haskell.lib.dontCheck (hself.callHackage "clash-prelude" "1.2.5" {});
    th-desugar = hself.callHackage "th-desugar" "1.10" {};
    recursion-schemes = dontOptimize hsuper.recursion-schemes;
  });
  ghc = haskellPkgs.ghcWithPackages (ps: with ps; [
          ghc-typelits-knownnat
          ghc-typelits-natnormalise
          ghc-typelits-extra
          clash-prelude
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc pkgs.cabal-install pkgs.stack ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
