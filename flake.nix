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
        ];

        nativeBuildPackages = with pkgs; [
          # Add elm through nix and not node (as a dependency of `@parcel/transformer-elm`) because the node installer tries to download the installer and that fails in the nix sandbox.
          # For the we also had to exclude elm as a dependency of the Parcel Elm tranformer.
          elmPackages.elm
        ];

        devPackages = with pkgs; [
          elmPackages.elm-format # Formatter for Elm
          elmPackages.elm-json # elm.json management
          elm2nix # needed to build elm with nix
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = buildPackages ++ devPackages;
          nativeBuildInputs = nativeBuildPackages;
        };

        packages.default = pkgs.buildNpmPackage {
          name = "elmder";

          buildInputs = buildPackages;
          nativeBuildInputs = nativeBuildPackages;
          src = ./.;
          npmDepsHash = "sha256-SvlklTgqGSoDyjlHRIjlhBuB4dyYl4Ro1Sc2aBgx76I=";

          configurePhase = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./registry.dat;
          };

          # The Elm npm package tries to download the binary, but instead we disable scripts and do a manual fix
          npmFlags = [ "--ignore-scripts" ];
          preBuild = ''
            substituteInPlace node_modules/.bin/elm \
              --replace 'var binaryPath = path.resolve' 'var binaryPath = "${pkgs.lib.getExe pkgs.elmPackages.elm}"; runCommand(); return; //'
          '';

          npmBuild = "npm run build";

          installPhase = ''
            mkdir $out
            cp -r dist/ $out
          '';
        };
      }
    );
}
