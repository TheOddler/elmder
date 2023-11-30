Elmder
------

# Dependencies

1. nix

That's it.
Everything else will be installed through nix when you open your dev shell.

# Dev shell

To open your dev-shell run

```bash
nix develop
```

Alternatively you can install [direnv](https://github.com/direnv/direnv) as I provide an `.envrc` that automatically opens the dev shell for you.

# Development

I'm using [Task](https://taskfile.dev/) to run common commands.

```bash
task fe # Starts the frontend server, this always watches your code and does live updates
task be # With those two commands you have a pretty good setup for developing both backend and frontend
# Or if you only want to run the backend tests:
task be:test --watch # you can add `--watch` to any task that has `sources` defines, many do, but the two tasks above always watch for changes
task --list-all # To see all other commands, or check in the `taskfile.yaml`s
```
