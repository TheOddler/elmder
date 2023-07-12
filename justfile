fe-dev:
  npm run --prefix frontend dev

fe-build:
  nix build .#frontend
