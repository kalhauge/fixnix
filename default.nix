{ fix ? import ./nix/fix
, pkgs ? fix.nixpkgs {}
, compiler ? "default"
, grammar ? fix.grammar
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";

in haskellPackages.developPackage {
  root = pkgs.lib.cleanSourceWith
    { filter = path: type: baseNameOf path != ".nix";
      src = pkgs.lib.cleanSource ./.;
    };
  name = "fixnix";
  source-overrides = { inherit grammar; };
  overrides = hsuper: hself: { };
  modifier = drv:
    with pkgs.haskell.lib;
    addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
  ;
}
