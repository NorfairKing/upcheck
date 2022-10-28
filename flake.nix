{
  description = "upcheck";
  nixConfig = {
    extra-substituters = "https://upcheck.cachix.org";
    extra-trusted-public-keys = "upcheck.cachix.org-1:fOaLBA5CXARqKdW1/JC3iT/c52hU77T9ZX9Fw4F17Ck=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            self.overlays.${system}
            (import (autodocodec + "/nix/overlay.nix"))
            (import (safe-coloured-text + "/nix/overlay.nix"))
            (import (sydtest + "/nix/overlay.nix"))
            (import (validity + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;
      in
      {
        overlays = import ./nix/overlay.nix;
        packages.default = pkgs.upcheck;
        checks = {
          nixos-module-test = import ./nix/nixos-module-test.nix {
            inherit pkgs;
            upcheck-nixos-module = self.nixosModules.${system}.default;
          };
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "upcheck-shell";
          packages = (p:
            [ p.upcheck ]
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = (with pkgs; [
            niv
            zlib
            cabal-install
          ]) ++ (with pre-commit-hooks.packages.${system};
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
        nixosModules.default = import ./nix/nixos-module.nix { upcheck = pkgs.upcheck; };
      });
}
