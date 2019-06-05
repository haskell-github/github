{ nixpkgs ? import ./nix/nixpkgs.nix
, dev ? false
}:
let
  pkgs = import nixpkgs {};
  hp = pkgs.haskellPackages.override (with pkgs.haskell.lib; {
    overrides = self: super: {
      # Newer versions of things we need
      ansi-terminal = super.ansi-terminal_0_9_1;
      binary-orphans = super.binary-orphans_1_0_1;
      QuickCheck = super.QuickCheck_2_13_1;
      quickcheck-instances = super.quickcheck-instances_0_3_21;
      tasty = super.tasty_1_2_2;
      time-compat = super.time-compat_1_9_2_2;
      unordered-containers = super.unordered-containers_0_2_10_0;

      # Things that work with our newer versions but don't know it yet
      # hspec test failure looks like it relies on some RNG to be just
      # right, not critical
      ChasingBottoms = doJailbreak super.ChasingBottoms;
      hspec-core = dontCheck (doJailbreak super.hspec-core);
      optparse-applicative = doJailbreak super.optparse-applicative;

      # We subbed in the correct versions of things it needs to work
      binary-instances = overrideCabal super.binary-instances (drv: {
        broken = false;
      });

      # Break infinite recursion through QuickCheck test dep
      splitmix = dontCheck super.splitmix;

    };
  });
  github = hp.callCabal2nix "github" ./. {};
in
  github
