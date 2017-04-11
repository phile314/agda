# Nix file for Agda.
#
# To create a development environment, create a shell.nix file with this content:
#
# -----------------------------------
# with (import <nixpkgs> {}).pkgs;
# let
#   agda = pkgs.haskellPackages.callPackage ./. {};
# in agda.Agda.env
# -----------------------------------
#
# To enter the development environment, simply call `nix-shell shell.nix`.
#

{ mkDerivation, alex, array, base, binary, boxes, bytestring
, containers, cpphs, data-hash, deepseq, directory, edit-distance
, emacs, equivalence, filepath, geniplate-mirror, happy, hashable
, hashtables, haskeline, haskell-src-exts, mtl, parallel, pretty
, process, process-extras, QuickCheck, stdenv, strict, tasty
, regex-tdfa, regex-tdfa-text, filemanip, fail
, tasty-silver, template-haskell, temporary, text, time
, transformers, transformers-compat, unordered-containers, xhtml
, murmur-hash, ieee754, gitrev
, zlib, tasty-quickcheck, monadplus, EdisonCore, EdisonAPI
, text-icu
, ghc # we only need the explicit ghc for versions checks
, user-manual ? true, python34Packages, texlive, tex ? texlive.combine {
    inherit (texlive) scheme-full;
  }
, nodejs-6_x
# additional test dependencies
, wdiff, colordiff
}:

let
  version = "2.5.3";
in rec {
  Agda = mkDerivation {
    pname = "Agda";
    version = version;
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    buildDepends = [
      array base binary boxes bytestring containers data-hash deepseq
      directory edit-distance equivalence filepath geniplate-mirror
      hashable hashtables haskeline haskell-src-exts mtl parallel pretty
      process QuickCheck strict template-haskell text time transformers filemanip
      transformers-compat unordered-containers xhtml zlib tasty-quickcheck
      monadplus EdisonCore EdisonAPI murmur-hash ieee754 gitrev text-icu
    ] ++ (if stdenv.lib.versionOlder "8.0" ghc.version then [] else [ fail ]);
    testDepends = [
      base containers directory filepath process-extras tasty
      regex-tdfa regex-tdfa-text
      tasty-silver temporary text
    ];
    configureFlags = [];
    buildTools = [ alex cpphs happy nodejs-6_x wdiff colordiff]
      ++ stdenv.lib.optionals user-manual [ python34Packages.sphinx python34Packages.sphinx_rtd_theme tex ];

    executableToolDepends = [ emacs ];

    postBuild = if user-manual then ''
      pushd doc/user-manual
      make PDFLATEX=xelatex latexpdf
      make html
      popd
    '' else "";

    doCheck = false;

    postInstall = ''
      # Separate loops to avoid internal error
      files=($out/share/*-ghc-*/Agda-*/lib/prim/Agda/{Primitive.agda,Builtin/*.agda})
      for f in "''${files[@]}"
      do
        $out/bin/agda $f
      done
      for f in "''${files[@]}"
      do
        $out/bin/agda -c --no-main $f
      done
      $out/bin/agda-mode compile
      # copy user manual
      mkdir -p $out/share/doc/Agda/user-manual/
      cp -r doc/user-manual/_build/html $out/share/doc/Agda/user-manual/
      cp doc/user-manual/_build/latex/Agda.pdf $out/share/doc/Agda/user-manual/Agda-User-Manual.pdf

      mkdir -p $out/nix-support/
      echo "doc user-manual-html $out/share/doc/Agda/user-manual/html/" >> $out/nix-support/hydra-build-products
      echo "doc user-manual-pdf $out/share/doc/Agda/user-manual/Agda-User-Manual.pdf" >> $out/nix-support/hydra-build-products
    '';

    homepage = "http://wiki.portal.chalmers.se/agda/";
    description = "A dependently typed functional programming language and proof assistant";
    license = "unknown";
  };
  Agda-haddock = (Agda.override (orig: {
    pname = "Agda-haddock";
    doHaddock = true;
  })).overrideDerivation (orig: {
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/doc/Agda/haddock
      cp -r dist/doc/html/Agda/* $out/share/doc/Agda/haddock/

      mkdir -p $out/nix-support/
      echo "doc agda-haddock $out/share/doc/Agda/haddock/" >> $out/nix-support/hydra-build-products
    '';
  });
}
