{ pkgs ? (import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz";}) {}).pkgsMusl} :

with pkgs;
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # 0ihr0av55kfg36igb1dn5q132q4gnyaf041xqi4rw7n67525qdap
      hjsonpointer = dontCheck (doJailbreak super.hjsonpointer) ;
      hjsonschema = dontCheck (doJailbreak super.hjsonschema) ;
      Ranged-sets = dontCheck (doJailbreak (self.callCabal2nix "Ranged-sets"
        (pkgs.fetchFromGitHub {
          owner = "PaulJohnson" ;
          repo = "Ranged-sets" ;
          rev = "RELEASE-0.4.0" ;
          sha256 = "0wd67pm1js9ws30zfxhm8s15nc4jb3668z59x2izi7cvckbymwdf" ;
        }) {})) ;

      # configurator-ng = dontCheck (doJailbreak (self.callCabal2nix "configurator-ng"
      #   (pkgs.fetchFromGitHub {
      #     owner = "lpsmith" ;
      #     repo = "configurator-ng" ;
      #     rev = "861863e476dd73e6c7db5fbc06d175950575ee93" ;
      #     sha256 = "1dr56k6p560s1aq90glf80k30hkdnbyqapnr15qy0gmmhwb08pjx" ;
      #   }) {})) ;
      # critbit = dontCheck (doJailbreak super.critbit) ;

    } ;
  } ;
  pkg = haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [haskellPackages.cabal-install] ;

      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--disable-executable-stripping"
      ];

    }) ;
  } ;
  buildInputs = [ ] ;
in pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs ;
})
