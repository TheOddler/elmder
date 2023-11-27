{
  description = "Elmder, a dating-app experiment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowBroken = true; };
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};

        buildPackages = with pkgs; [
          pkgs-unstable.go-task # Run commands, like a modern make

          # Stuff for the frontend
          nodejs_18

          # Stuff for the backend
          stack
          postgresql
        ];

        devPackages = with pkgs; [
          # Stuff for the frontend development
          elmPackages.elm-format # Formatter for Elm
          elmPackages.elm-json # elm.json management
          elm2nix # needed to build elm with nix

          # Stuff for the backend development, make sure the haskell stuff matches the stackage snapshot we're using
          # The snapshot can be found in `stack.yaml` under the field `resolver`
          # The ghc version that snapshot is using can be found by going to the stackage page for it, for example: https://www.stackage.org/lts-20.25
          haskell.compiler.native-bignum.ghc928
          (haskell-language-server.override { supportedGhcVersions = [ "928" ]; })
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
          # For this we had to use `elm2nix`, see README on how to generate the required files when updating dependencies.
          configurePhase = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./frontend/elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./frontend/registry.dat;
          };
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = buildPackages ++ devPackages;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [
                "frontend/elm-srcs.nix" # Auto generated
              ];
            };
            settings = {
              hlint.hintFile = ./backend/.hlint.yaml;
              ormolu.defaultExtensions = [
                "GHC2021" # Let Ormolu know we're using GHC2021
              ];
            };
          };
          backend = self.packages.${system}.backend;
          frontend = self.packages.${system}.frontend;
        };

        packages.backend = pkgs.haskellPackages.developPackage {
          root = ./backend;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              zlib # required for servant
              postgresql # required for tmp-postgres
            ]);
        };

        packages.frontend = pkgs.buildNpmPackage {
          name = "elmder";
          nativeBuildInputs = buildPackages ++ elmParcelNixFix.nativeBuildPackages;
          src = ./frontend;
          npmDepsHash = "sha256-SvlklTgqGSoDyjlHRIjlhBuB4dyYl4Ro1Sc2aBgx76I=";

          preBuild = ''
            ${self.packages.${system}.backend}/bin/elm-gen
          '' + elmParcelNixFix.preBuild;

          installPhase = ''
            mkdir $out
            cp -r dist/ $out
          '';

          npmFlags = elmParcelNixFix.npmFlags;
          configurePhase = elmParcelNixFix.configurePhase;
        };
      }
    );
}
