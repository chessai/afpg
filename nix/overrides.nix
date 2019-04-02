{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  afpg = (
    with rec {
      afpgSource = pkgs.lib.cleanSource ../.;
      afpgBasic  = self.callCabal2nix "afpg" afpgSource { };
    };
    overrideCabal afpgBasic (old: {
    })
  );
}
