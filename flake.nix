{
  description = "Elmder, an experiment in making a dating-app-like interface in Elm";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        buildPackages = with pkgs; [
          nodejs_18
          just
        ];

        devPackages = with pkgs; [
          elmPackages.elm-format # Formatter for Elm
          elmPackages.elm-json # elm.json management
          elm2nix # needed to build elm with nix
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
        };

        packages.frontend = pkgs.buildNpmPackage {
          name = "elmder";
          buildInputs = buildPackages;
          src = ./frontend;
          npmDepsHash = "sha256-SvlklTgqGSoDyjlHRIjlhBuB4dyYl4Ro1Sc2aBgx76I=";

          npmBuild = "npm run --prefix frontend build";
          installPhase = ''
            mkdir $out
            cp -r dist/ $out
          '';

          # Fixes for using Elm and Parcel with nix
          nativeBuildInputs = elmParcelNixFix.nativeBuildPackages;
          npmFlags = elmParcelNixFix.npmFlags;
          preBuild = elmParcelNixFix.preBuild;
          configurePhase = elmParcelNixFix.configurePhase;
        };
      }
    );
}
