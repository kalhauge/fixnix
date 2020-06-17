# Create a shell with fixnix in it.
{ fix ? import ./fix
, pkgs ? fix.nixpkgs {}
}:
with pkgs;
mkShell {
  buildInputs = [
    (haskell.lib.dontCheck
     (haskell.lib.disableLibraryProfiling
      ((haskellPackages.extend (haskell.lib.packageSourceOverrides {
        fixnix = ./..;
        inherit (fix) grammar;
     })).fixnix)
     ))
  ];
}

