(import ./default.nix).shellFor {
  packages = p: [ p.mimidapton ];
  withHoogle = true;
}