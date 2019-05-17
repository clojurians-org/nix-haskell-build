# export NIX_REMOTE=daemon
# { pkgs ? (import (builtins.fetchTarball {url="https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz";}) {}).pkgsMusl} :
# nix-channel --add https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz nixpkgs-static
# nix-channel --update nixpkgs-static
# export NIX_PATH=~/.nix-defexpr/channels
# 0ihr0av55kfg36igb1dn5q132q4gnyaf041xqi4rw7n67525qdap
{ nixpkgs ? (import <nixpkgs-static> {
              config.packageOverrides = pkgs: rec {
                nix = pkgs.nix.overrideDerivation (old: {
                  doInstallCheck = false ;
                }) ;
              } ;
            }).pkgsMusl} :

with nixpkgs;
let
  openssl_static = pkgs.openssl.override { static = true; };
  postgresql_static = pkgs.postgresql.overrideAttrs (old: { dontDisableStatic = true; });
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
          "--disable-executable-stripping"
      ];
    }) ;
  }
