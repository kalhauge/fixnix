# Create a shell with fixnix in it.
{ fix ? import ./fix
, pkgs ? fix.nixpkgs {}
}:
with pkgs;
mkShell {
  buildInputs = [
    ( with haskell.lib;
    dontHaddock (dontCheck (disableLibraryProfiling
      ((haskellPackages.extend (haskell.lib.packageSourceOverrides {
        fixnix = ./..;
        inherit (fix) grammar;
     })).fixnix)
     )))
   ];
  shellHook = "source <(fixnix --bash-completion-enriched --bash-completion-script `which fixnix`)";
}

