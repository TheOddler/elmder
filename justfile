default:
  just --list --unsorted





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





# Runs the backend dev loop.
# Uses watchexec instead of stack's file-watch so we can clear the screen, and it picks up changes to the stack files itself.
# Also, by default we have `-Werror` but while developing that can be annoying, so for the dev loop here we set `-Wwarn` to override.
be-dev:
  watchexec -e=hs,yaml --project-origin=./backend 'clear; stack test --ghc-options="-Wwarn"'

be-test match='':
  stack test --test-arguments="--match=""{{match}}"""

be-golden-test-reset:
  stack test --test-arguments="--golden-reset"
