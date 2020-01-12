with import <nixpkgs> {
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override (oldHask: {
        overrides = super.lib.composeExtensions (oldHask.overrides or (_: _: {})) (hself: hsuper: {
          # algebraic-graphs = hsuper.callHackage "algebraic-graphs" "0.4" {};
        });
      });
    })
  ];
};
haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  mimidapton = ./mimidapton;
})
