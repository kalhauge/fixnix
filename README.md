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

# TODO's

- Make the parsers of the locations explainable. This means that any 
  defined parsser will know how to print it-self.

  For example the github parser should produce, something like:

    '<github-owner>/<github-repo>/<git-commit>'

    where <github-owner> and <github-repo> is a string and <git-commit> is

      - 'tags/<tag>'     where <tag> is a string
      - 'heads/<branch>' where <branch> is a string
      - 'rev/<rev>'      where <rev> is a git revision


- Add E-Tag support: See [wiki](https://en.wikipedia.org/wiki/HTTP_ETag)
