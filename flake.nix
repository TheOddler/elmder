{
  description = "Elmder, a dating-app experiment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
    mkElmDerivation.url = github:jeslie0/mkElmDerivation;
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, pre-commit-hooks, flake-utils, mkElmDerivation }:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ mkElmDerivation.overlays.mkDotElmDirectoryCmd ];
          config.allowBroken = true; # For servant-elm
          inherit system;
        };
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};

        devPackages = with pkgs; [
          pkgs-unstable.go-task # Run commands, like a modern make

          # Stuff for the frontend development
          nodejs_18
          elmPackages.elm-format # Formatter for Elm
          elmPackages.elm-json # elm.json management

          # Stuff for the backend development
          cabal-install
          haskell-language-server
          hlint
        ];

        elmParcelNixFix = {
          # To make Elm, Parcel and Nix work together we need do some some fixes:

          # 1. The Elm node package tries to download the binary when installing, this doesn't work in Nix's sandbox, so instead we add it to the nativeBuildPackages, set the npmFlag to not run install scripts, and alter this (https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/installers/npm/bin/elm#L8-L30) script to instead of downloading the binary to just use the one we installed as the native build package
          nativeBuildPackages = [ pkgs.elmPackages.elm ];
          npmFlags = [ "--ignore-scripts" ];
          preBuild = ''
            substituteInPlace node_modules/.bin/elm \
              --replace 'var binaryPath = path.resolve' 'var binaryPath = "${pkgs.lib.getExe pkgs.elmPackages.elm}"; runCommand(); return; //'
          '';

          # 2. Manage the Elm dependencies through nix too, otherwise Parcel will try to download them and that is again not allowed in Nix's sandbox.
          generateElmJsonFiles = pkgs.mkDotElmDirectoryCmd ./frontend/elm.json;
        };
      in
      {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.backend ];
          buildInputs = devPackages;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
            };
            settings = {
              hlint.hintFile = ./.hlint.yaml;
              ormolu.defaultExtensions = [
                "GHC2021" # Let Ormolu know we're using GHC2021
                "LambdaCase"
                "OverloadedRecordDot"
                "OverloadedStrings"
                "StrictData"
              ];
            };
          };
          backend = self.packages.${system}.backend;
          frontend = self.packages.${system}.frontend;
        };

        packages.backend = pkgs.haskellPackages.developPackage {
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              zlib # required for servant
              postgresql # required for tmp-postgres
            ]);
        };

        packages.frontend = pkgs.buildNpmPackage {
          name = "elmder";
          src = ./frontend;
          npmDepsHash = "sha256-NkJQy6gxdpqSFhdswRsYqT69dY7QytFp9iZRAQ++3x8=";

          preBuild = ''
            ${self.packages.${system}.backend}/bin/elm-gen
          '' + elmParcelNixFix.preBuild;

          installPhase = ''
            mkdir $out
            cp -r dist/ $out
          '';

          nativeBuildInputs = elmParcelNixFix.nativeBuildPackages;
          npmFlags = elmParcelNixFix.npmFlags;
          prePatch = elmParcelNixFix.generateElmJsonFiles;
        };
      }
    );
}
