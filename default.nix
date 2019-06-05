{ nixpkgs ? import ./nix/nixpkgs.nix
, dev ? false
}:
let
  pkgs = import nixpkgs { config.allowBroken = true; };
  hp = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: with pkgs.haskell.lib; {
      binary-instances-1 = doJailbreak self.binary-instances-1;
      QuickCheck = self.QuickCheck_2_13_1;
      quickcheck-instances = super.quickcheck-instances_0_3_21;
      binary-orphans = self.binary-orphans_1_0_1;
      github = super.callCabal2nix "github" ./github.cabal {};
    });
  });
in
  hp.github
