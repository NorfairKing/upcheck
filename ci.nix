let
  pkgs = import ./nix/pkgs.nix;
in
{
  inherit (pkgs) upcheck;
  pre-commit-check = (import ./nix/pre-commit-hooks.nix).run;
}
