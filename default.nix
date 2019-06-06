{ nixpkgs ? import ./nixpkgs.nix }:
let
  pkgs = import nixpkgs { overlays = [ (import ./overlay.nix) ]; };
in pkgs.haskellPackages.callCabal2nix "github" ./. { }
