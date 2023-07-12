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

I'm using [just](https://github.com/casey/just) to run common commands:

```bash
just fe-dev # Start the frontend dev loop
just fe-build # Build frontend
just fe-install # Install the frontend dependencies without updating them, you'll likely need to run this first time
just fe-update # Update node/elm dependencies
```
