{ pkgs ? (import <nixpkgs-static> {}).pkgsMusl} :
# { pkgs ? import <nixpkgs> {}} :

# 1472 commit: bcc317db81243dcd5ce488f445e3bb5e9a0fa62e
with pkgs;
let
  postgresql_static = pkgs.postgresql.overrideAttrs (old: { dontDisableStatic = true; });
  openssl_static = pkgs.openssl.override { static = true; };

  haskellPackages = pkgs.haskell.packages.ghc843.override {
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
      hasql-pool = dontCheck super.hasql-pool ;
      postgresql-libpq = super.postgresql-libpq.override { postgresql = postgresql_static; };                                                                          
    } ;
  } ;
in
  haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (old: {
      buildTools = (old.buildTools or []) ++ [haskellPackages.cabal-install] ;

      isLibrary = false;
      isExecutable = true;
      executableSystemDepends = [ postgresql_static ] ;

      preConfigure = builtins.concatStringsSep "\n" [
        (old.preConfigure or "")
        ''
          set -e
          echo "larluo->" $(pkg-config --static --libs openssl)
          configureFlags+=$(for flag in $(pkg-config --static --libs openssl); do echo -n " --ld-option=$flag"; done)                                                  
        ''
      ];
      libraryPkgconfigDepends = (old.libraryPkgconfigDepends or []) ++ [openssl_static] ;                                                                              

      configureFlags = [
           "--ghc-option=-optl=-static"
           "--ghc-option=-optl=-pthread"
           "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
           "--extra-lib-dirs=${pkgs.zlib.static}/lib"
           "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
           "--disable-executable-stripping"
      ];
      enableSharedExecutables = false;
      enableSharedLibraries = false;

    }) ;
  }

