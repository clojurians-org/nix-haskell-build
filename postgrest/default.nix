# export NIX_REMOTE=daemon
# { nixpkgs ? (import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz";}) {}).pkgsMusl} :
# nix-channel --add https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz nixpkgs-static
# nix-channel --update nixpkgs-static
# export NIX_PATH=~/.nix-defexpr/channels
# 0ihr0av55kfg36igb1dn5q132q4gnyaf041xqi4rw7n67525qdap
{ nixpkgs ? (import <nixpkgs-static> {}).pkgsMusl } :
# 1472 commit: bcc317db81243dcd5ce488f445e3bb5e9a0fa62e
with nixpkgs;
let
  openssl_static = pkgs.openssl.override { static = true; };
  postgresql_static = pkgs.postgresql.overrideAttrs (old: { dontDisableStatic = true; });

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
  makeCabalPatch = { name, url, sha256 }:
    let
      # We use `runCommand` on a plain patch file instead of using
      # `fetchpatch`'s `includes` or `stripLen` features to not run
      # into the perils of:
      #   https://github.com/NixOS/nixpkgs/issues/48567
      plainPatchFile = pkgs.fetchpatch { inherit name url sha256; };

      # Explanation:
      #   * A patch created for the cabal project's source tree will
      #     always have subdirs `Cabal` and `cabal-install`; the
      #     `Cabal` nix derivation is already the `Cabal` subtree.
      #   * We run `filterdiff -i` to keep only changes from the patch
      #     that apply to the `Cabal` subtree.
      #   * We run `filterdiff -x` to remove Changelog files which
      #     almost always conflict.
      #   * `-p1` strips away the `a/` and `b/` before `-i`/`-x` apply.
      #   * `strip=2` removes e.g `a/Cabal` so that the patch applies
      #     directly to that source tree, `--add*prefix` adds back the
      #     `a/` and `b/` that `patch -p1` expects.
      patchOnCabalLibraryFilesOnly = pkgs.runCommand "${name}-Cabal-only" {} ''
        ${pkgs.patchutils}/bin/filterdiff \
          -p1 -i 'Cabal/*' -x 'Cabal/ChangeLog.md' \
          --strip=2 --addoldprefix=a/ --addnewprefix=b/ \
          ${plainPatchFile} > $out
        if [ ! -s "$out" ]; then
          echo "error: Filtered patch '$out' is empty (while the original patch file was not)!" 1>&2
          echo "Check your includes and excludes." 1>&2
          echo "Normalizd patch file was:" 1>&2
          cat "${plainPatchFile}" 1>&2
          exit 1
        fi
      '';
    in
      patchOnCabalLibraryFilesOnly;

  applyPatchesToCabalDrv = cabalDrv: pkgs.haskell.lib.overrideCabal cabalDrv (old: {
    patches =
      # Patches we know are merged in a certain cabal version
      # (we include them conditionally here anyway, for the case
      # that the user specifies a different Cabal version e.g. via
      # `stack2nix`):
      (builtins.concatLists [
        # -L flag deduplication
        #   https://github.com/haskell/cabal/pull/5356
        (lib.optional (pkgs.lib.versionOlder cabalDrv.version "2.4.0.0") (makeCabalPatch {
          name = "5356.patch";
          url = "https://github.com/haskell/cabal/commit/fd6ff29e268063f8a5135b06aed35856b87dd991.patch";
          sha256 = "1l5zwrbdrra789c2sppvdrw3b8jq241fgavb8lnvlaqq7sagzd1r";
        }))
      # Patches that as of writing aren't merged yet:
      ]) ++ [
        # TODO Move this into the above section when merged in some Cabal version:
        # --enable-executable-static
        #   https://github.com/haskell/cabal/pull/5446
        (if pkgs.lib.versionOlder cabalDrv.version "2.4.0.0"
          then
            # Older cabal, from https://github.com/nh2/cabal/commits/dedupe-more-include-and-linker-flags-enable-static-executables-flag-pass-ld-options-to-ghc-Cabal-v2.2.0.1
            (makeCabalPatch {
              name = "5446.patch";
              url = "https://github.com/haskell/cabal/commit/748f07b50724f2618798d200894f387020afc300.patch";
              sha256 = "1zmbalkdbd1xyf0kw5js74bpifhzhm16c98kn7kkgrwql1pbdyp5";
            })
          else
            (makeCabalPatch {
              name = "5446.patch";
              url = "https://github.com/haskell/cabal/commit/cb221c23c274f79dcab65aef3756377af113ae21.patch";
              sha256 = "02qalj5y35lq22f19sz3c18syym53d6bdqzbnx9f6z3m7xg591p1";
            })
        )
        # TODO Move this into the above section when merged in some Cabal version:
        # ld-option passthrough
        #   https://github.com/haskell/cabal/pull/5451
        (if pkgs.lib.versionOlder cabalDrv.version "2.4.0.0"
          then
            # Older cabal, from https://github.com/nh2/cabal/commits/dedupe-more-include-and-linker-flags-enable-static-executables-flag-pass-ld-options-to-ghc-Cabal-v2.2.0.1
            (makeCabalPatch {
              name = "5451.patch";
              url = "https://github.com/haskell/cabal/commit/b66be72db3b34ea63144b45fcaf61822e0fade87.patch";
              sha256 = "0hndkfb96ry925xzx85km8y8pfv5ka5jz3jvy3m4l23jsrsd06c9";
            })
          else
            (makeCabalPatch {
              name = "5451.patch";
              url = "https://github.com/haskell/cabal/commit/0aeb541393c0fce6099ea7b0366c956e18937791.patch";
              sha256 = "0pa9r79730n1kah8x54jrd6zraahri21jahasn7k4ng30rnnidgz";
            })
        )
      ];
  });
in haskellPackages.developPackage {
    root = ./.;

    modifier = drv: haskell.lib.overrideCabal drv (old: {
      preConfigure = builtins.concatStringsSep "\n" [
       (old.preConfigure or "")
       ''
         set -e
         configureFlags+=$(for flag in $(${pkgconfig}/bin/pkg-config --static --libs openssl); do echo -n " --ld-option=$flag"; done)
       ''
      ];
      setupHaskellDepends = (old.setupHaskellDepends or []) ++ [ (applyPatchesToCabalDrv pkgs.haskellPackages.Cabal_2_2_0_1 ) ] ;
      executablePkgconfigDepends = (old.executablePkgconfigDepends or []) ++ [openssl_static] ;

      isLibrary = false;
      isExecutable = true;
      enableSharedExecutables = false;
      enableSharedLibraries = false;

      configureFlags = [
          "--enable-executable-static "
          "--ld-option=--start-group "
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
          "--disable-executable-stripping"
      ];
    }) ;
  }


