self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      clash-ghc = hsuper.callHackage "clash-ghc" "1.8.1" {};  # Example version
    };
  };
}
