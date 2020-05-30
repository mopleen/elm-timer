let src = import nix/sources.nix;
pkgs = import src.nixpkgs {};
derF = import ./auto.nix;
der = derF { nixpkgs=src.nixpkgs; };
in {
  der = der;
}
