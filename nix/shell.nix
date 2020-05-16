# Create a shell with fixnix in it.
{ pkgs ? import fix/nixpkgs.nix {}
}:
with pkgs;
mkShell {
  buildInputs = [ 
    (haskell.lib.dontCheck 
     (haskell.lib.disableLibraryProfiling 
      ((haskellPackages.extend (haskell.lib.packageSourceOverrides {
       fixnix = ./..;
     })).fixnix)
     ))
  ];
}

