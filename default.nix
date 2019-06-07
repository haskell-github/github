{ nixpkgs ? import ./nixpkgs.nix }:
let
  pkgs = import nixpkgs { overlays = [ (import ./overlay.nix) ]; };
  github = pkgs.haskellPackages.callCabal2nix "github" ./. { };
  githubOpenSSL = pkgs.haskell.lib.overrideCabal github (drv: {
    libraryHaskellDepends = drv.libraryHaskellDepends ++ (with pkgs.haskellPackages; [
      # These are behind a flag that that is enabled in the `cabal.project` file.
      http-client-openssl
      HsOpenSSL
      HsOpenSSL-x509-system
    ]);
  });
in
  githubOpenSSL
