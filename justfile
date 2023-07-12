fe-dev:
  npm run --prefix frontend dev

fe-build:
  nix build .#frontend

fe-install:
  npm --prefix frontend ci

fe-update:
  npm --prefix frontend install
  # elm2nix must be run in the root of the frontend folder
  cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot

