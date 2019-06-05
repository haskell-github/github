{ nixpkgs ? import ./nix/nixpkgs.nix
, cabal-v2 ? true
}:

(import ./. {inherit nixpkgs cabal-v2;}).env
