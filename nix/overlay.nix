final: prev:
with final.lib;
with final.haskell.lib;

{
  upcheck = justStaticExecutables final.haskellPackages.upcheck;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        upcheck = buildStrictly (self.callPackage ../upcheck { });
      }
    );
  });
}
