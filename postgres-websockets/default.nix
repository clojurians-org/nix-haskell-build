# export NIX_REMOTE=daemon
# { pkgs ? (import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz";}) {}).pkgsMusl} :
# nix-channel --add https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz nixpkgs-static
# nix-channel --update nixpkgs-static
# export NIX_PATH=~/.nix-defexpr/channels
# 0ihr0av55kfg36igb1dn5q132q4gnyaf041xqi4rw7n67525qdap
{ pkgs ? (import <nixpkgs-static> {}).pkgsMusl} :

with pkgs;
let
  postgresql_static = pkgs.postgresql.overrideAttrs (old: { dontDisableStatic = true; });
  openssl_static = pkgs.openssl.override { static = true; };
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hasql-pool = dontCheck (doJailbreak super.hasql-pool) ;
      stm-containers = dontCheck (doJailbreak (self.callCabal2nix "stm-containers"
        (pkgs.fetchFromGitHub {
          owner = "nikita-volkov" ;
          repo = "stm-containers" ;
          rev = "1.1.0.4" ;
          sha256 = "19662sf92hf6rqmanqal9lsbxv64bznxm6yl7sc00w0iqwl13hcj" ;
        }) {})) ;
      stm-hamt = dontCheck (doJailbreak (self.callCabal2nix "stm-hamt"
        (pkgs.fetchFromGitHub {
          owner = "nikita-volkov" ;
          repo = "stm-hamt" ;
          rev = "1.2.0.1" ;
          sha256 = "018zqgpa91qar4q5jxiay4knwl6nqri583l7ys241pbmh46zsnpd" ;
        }) {})) ;
      deferred-folds = dontCheck (doJailbreak (self.callCabal2nix "deferred-folds"
        (pkgs.fetchFromGitHub {
          owner = "metrix-ai" ;
          repo = "deferred-folds" ;
          rev = "0.9.10" ;
          sha256 = "07n2j4pw37kxkfka98l2n3pr92x8xmbr9gk0xq85q7ax6icz5al6" ;
        }) {})) ;
      primitive = dontCheck (doJailbreak (self.callCabal2nix "primitive"
        (pkgs.fetchFromGitHub {
          owner = "haskell" ;
          repo = "primitive" ;
          rev = "v0.6.4.0" ;
          sha256 = "0snaw0psjbbpzmif4gk4fa11221rpm4pqr7hsxqy714390nd7v2y" ;
        }) {})) ;
      primitive-extras = dontCheck (doJailbreak (self.callCabal2nix "primitive-extras"
        (pkgs.fetchFromGitHub {
          owner = "metrix-ai" ;
          repo = "primitive-extras" ;
          rev = "0.7.1" ;
          sha256 = "1m1iya6q3kiqcndxxfkxhadd52innk6c6wmsw0axbgpf95ia83yx" ;
        }) {})) ;
      focus = dontCheck (doJailbreak (self.callCabal2nix "focus"
        (pkgs.fetchFromGitHub {
          owner = "nikita-volkov" ;
          repo = "focus" ;
          rev = "1.0.1.3" ;
          sha256 = "126ac172mn5jrry5vdny64ca8wwqvg7mhqkayglnp0fy273vbynh" ;
        }) {})) ;
      postgresql-libpq = super.postgresql-libpq.override { postgresql = postgresql_static; };

    } ;
  } ;
in haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal 
                      # (haskell.lib.appendConfigureFlag drv ["--ld-option=--start-group"])
                      drv
                      (old: {
      buildTools = (old.buildTools or []) ++ [haskellPackages.cabal-install] ;
      # executableSystemDepends = [ openssl_static postgresql_static.lib postgresql_static ] ;
      preConfigure = builtins.concatStringsSep "\n" [
        (old.preConfigure or "")
        ''
          set -e
          echo "larluo->" $(pkg-config --static --libs openssl)
          configureFlags+=$(for flag in $(pkg-config --static --libs openssl); do echo -n " --ld-option=$flag"; done)
        ''
      ];
      libraryPkgconfigDepends = (old.libraryPkgconfigDepends or []) ++ [openssl_static] ;

      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--disable-executable-stripping"
      ];

    }) ;
  }
