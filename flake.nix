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

        packages = with pkgs; [
          # Node to manage the project, the actual building happens with Parcel (installed by node)
          nodejs

          # And some basic stuff for the dev setup, everything to build the project is managed by node
          elmPackages.elm-format # Formatter
          elmPackages.elm-json # Install, upgrade and uninstall Elm dependencies
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = packages;
        };
      }
    );
}
