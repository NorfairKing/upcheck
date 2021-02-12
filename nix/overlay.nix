final: previous:
with final.lib;
with final.haskell.lib;

{
  upcheck =
    failOnAllWarnings (
      disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions "upcheck" (final.gitignoreSource ../upcheck) "--no-hpack" {})
    );
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super:
                {
                  inherit (final) autorecorder;
                }
            );
        }
    );
}
