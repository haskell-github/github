{nixpkgs ? import ./nix/nixpkgs.nix}:

(import ./. {inherit nixpkgs;}).env
