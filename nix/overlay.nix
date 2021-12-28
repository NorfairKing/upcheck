final: previous:
with final.lib;
with final.haskell.lib;

let
  upcheck = buildStrictly (
    disableLibraryProfiling (
      final.haskellPackages.callCabal2nixWithOptions "upcheck" (final.gitignoreSource ../upcheck) "--no-hpack" { }
    )
  );
in
{
  upcheck = justStaticExecutables upcheck;
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              {
                inherit upcheck;
              }
          );
      }
    );
}
