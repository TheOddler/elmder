with (import <nixpkgs> { });

mkShell {
  buildInputs = [
    # Node to manage the project
    nodejs # Needed for parcel

    # And some basic stuff for my dev setup here, everything for the project is managed by node
    elmPackages.elm-format # Formatter for VS Code
    elmPackages.elm-json # Install, upgrade and uninstall Elm dependencies
  ];
}
