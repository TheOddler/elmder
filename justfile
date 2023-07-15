default:
  just --list --unsorted





# Runs the frontend.
# You'll likely want to start the backend too, there's a just recipe for that too.
fe:
  npm run --prefix frontend dev

fe-build:
  nix build .#frontend

fe-install:
  npm --prefix frontend ci

fe-update:
  npm --prefix frontend install
  # elm2nix must be run in the root of the frontend folder
  cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot





# Runs the backend loop.
# This tests the backend and starts it.
# Also, by default we have `-Werror` but while developing that can be annoying, so for this loop here we set `-Wwarn` to override.
be:
  watchexec -e=hs,yaml --project-origin=./backend --restart --stop-timeout=10s 'clear; stack test --ghc-options="-Wwarn"; stack run'

# Runs the backend tests in a loop.
be-test match='':
  stack test --test-arguments="--match=""{{match}}""" --file-watch

be-golden-test-reset:
  stack test --test-arguments="--golden-reset"
