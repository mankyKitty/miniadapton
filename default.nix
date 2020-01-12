with import <nixpkgs> {
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override (oldHask: {
        overrides = super.lib.composeExtensions (oldHask.overrides or (_: _: {})) (hself: hsuper: {
          graphite = hsuper.callHackage "graphite" "0.10.0.1" {};
        });
      });
    })
  ];
};
haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  mimidapton = ./mimidapton;
})
