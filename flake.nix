{
  description =
    "Development environment and build outputs for a GameBoy emulator";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fourmolu = {
      url = "github:fourmolu/fourmolu";
      flake = false;
    };
  };

  outputs = inputs@{ self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: { haskellPackages = prev.haskell.packages.ghc96; })
          self.overlays.default
        ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
      in {
        formatter = pkgs.alejandra;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.game-boy-emulator-hs ];
          nativeBuildInputs = [
            pkgs.alejandra
            pkgs.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-fmt
            (pkgs.haskell.lib.compose.dontCheck
              (pkgs.haskellPackages.callCabal2nix "fourmolu" inputs.fourmolu
                { }))
          ];
        };
      }) // {
        overlays.default = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (hfinal: hprev: {
                game-boy-emulator-hs =
                  hfinal.callCabal2nix "game-boy-emulator-hs"
                  (final.lib.cleanSource ./.) { };
              });
          });
        };
      };
}
