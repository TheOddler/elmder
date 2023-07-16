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

fe-gen-api:
  openapi-generator-cli generate -g elm -i backend/openapi/openapi.json -o frontend/generated
  # There's currently a bug in openapi-generator so we need to do an ugly manual fix
  # https://github.com/OpenAPITools/openapi-generator/issues/16104
  sed -e "s/, UserSectionTag(..), userSectionTagVariants//g" -i frontend/generated/src/Api/Data.elm





# Runs the backend loop.
# This tests the backend and starts it, looping when changes to the backend happen.
be:
  watchexec -e=hs,yaml --project-origin=./backend --restart --stop-timeout=10s 'clear; just be-go-once'

[private]
be-go-once:
  stack test --ghc-options="-Wwarn"
  stack run backend-exe

# Runs the backend tests in a loop.
be-test match='':
  stack test --test-arguments="--match=""{{match}}""" --file-watch

be-golden-test-reset:
  stack test --test-arguments="--golden-reset --golden-start" --file-watch
