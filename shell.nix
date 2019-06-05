{nixpkgs ? import ./nix/nixpkgs.nix}:

(import ./. {inherit nixpkgs;}).env
# let
#   pkgs = import nixpkgs {};
#   ghcWithP = pkgs.haskellPackages.ghcWithPackages (hp: with hp; [
#     pkgs.cabal-install
#     ghcid
#     zlib
#   ]);
# in
#   pkgs.mkShell {
#     buildInputs = [ghcWithP];
#   }
