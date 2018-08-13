{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: {
    # cabal-install = ghc.callHackage "cabal-install" "2.0.0.1" {};
    # safe-money = self.callHackage "safe-money" "0.3" {};
    # lens-family-th = self.callHackage "lens-family-th" "0.4.1.0" {};
    safe-money = pkgs.haskell.lib.doJailbreak super.safe-money;
  };
  
})
