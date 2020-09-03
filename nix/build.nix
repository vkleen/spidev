{ ghcVersion ? "ghc884" }:
let
  sources = import ./sources.nix;
  haskell-nix-overlays = (import sources."haskell.nix" { sourcesOverride = {}; }).overlays;
  pkgs = import sources.nixpkgs {
    config = {
      allowUnsupportedSystem = true;
    };
    overlays = haskell-nix-overlays ++
      [ (self: super: {
          "libusb-1.0" = super.libusb1;
        })
      ];
  };
  inherit (pkgs) lib;

  inherit (import sources."niv" {}) niv;

  nonReinstallablePkgs= [
    "Cabal"
     "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
    "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
    # ghcjs custom packages
    "ghcjs-prim" "ghcjs-th"
    "ghc-boot"
    "ghc" "Win32" "array" "binary" "bytestring" "containers"
    "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
    # "ghci" "haskeline"
    "hpc"
    "mtl" "parsec" "process" "text" "time" "transformers"
    "unix" "xhtml"
    "stm" "terminfo"
  ];

  hie-pkgs = let
    hsPkgs = pkgs.haskell-nix.stackProject {
      src = pkgs.fetchgit {
        url = sources.haskell-language-server.repo;
        inherit (sources.haskell-language-server) rev fetchSubmodules sha256;
      };
      sha256map = {
        "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
      };
      projectFileName = "stack-8.8.4.yaml";
      compiler-nix-name = ghcVersion;
      pkg-def-extras = [
      ];
      modules = [ ({config, ...}: {
        reinstallableLibGhc = true;
        inherit nonReinstallablePkgs;
        compiler.nix-name = lib.mkForce ghcVersion;
      }) ];
    };
  in {
    haskell-language-server = hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  haskell = let v' = ghcVersion; in { ghcVersion ? v' }: pkgs: pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../.; name = "spidev"; };
    compiler-nix-name = ghcVersion;
    pkg-def-extras = [
    ];
    modules = [ ({config, ...}: {
      compiler.nix-name = lib.mkForce ghcVersion;
      reinstallableLibGhc = true;
      inherit nonReinstallablePkgs;
      configureFlags = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isMusl [
        "--ghc-option=-optl=-static"
        "--extra-lib-dirs=${pkgs.gmp}/lib"
        "--extra-lib-dirs=${pkgs.zlib_both}/lib"
        "--extra-lib-dirs=${pkgs.libffi}/lib"
        "--enable-executable-stripping"
      ];
    }) ];
  };
in rec {
  hsPkgs = haskell { ghcVersion = "ghc884"; } pkgs;
  inherit (hie-pkgs) haskell-language-server;

  shell = hsPkgs.shellFor {
    packages = ps: with ps; [
      spidev
    ];

    additional = ps: with ps; [
    ];

    withHoogle = true;

    tools = {
      "hlint" = "3.1.6";
      "cabal-install" = "3.2.0.0";
      # "niv" = "0.2.16";
      "ghcid" = "0.8.7";
      "hpack" = "0.34.2";
      "ormolu" = "0.1.2.0";
    };
    buildInputs = [ haskell-language-server ];

    shellHook = ''
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  };
}
