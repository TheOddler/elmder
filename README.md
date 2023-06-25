# Elmder

# To build

```bash
nix build
```

## Dev

```bash
direnv allow # or nix-shell if you don't have direnv, or just install nodejs directly
npm install # only the first time
npm run live
```

# To update node/elm dependencies

```bash
elm2nix convert > elm-srcs.nix && elm2nix snapshot && npm install
```
