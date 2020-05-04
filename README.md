# fixnix: A nix fixing tool

I have the problem that I want to fix different dependencies in my
nix projects, however this is not always that easy.

# Fixing Nixpkgs:

Create a file `default.nix` which takes `pkgs` as input:

```
{ pkgs ? import ./nix/fix/nixpkgs.nix {}
}: ... your expression ...
```

Now run fixnix, the default behaviour is to fix a version of nixpkgs.

```
> fixnix nix/fix/nixpkgs.nix
```
