{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fourmolu = {
      url = "github:fourmolu/fourmolu";
      flake = false;
    };
  };

  outputs = inputs @ {...}:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      overlays = [(final: prev: {haskellPackages = prev.haskell.packages.ghc96;})];
      pkgs = import inputs.nixpkgs {inherit system overlays;};
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.haskellPackages.shellFor {
        packages = _: [
          (pkgs.haskellPackages.callCabal2nix "game-boy-emulator-hs"
            (pkgs.lib.cleanSource ./.) {})
        ];
        nativeBuildInputs = [
          pkgs.alejandra
          pkgs.cabal-install
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.cabal-fmt
          (pkgs.haskell.lib.compose.dontCheck
            (pkgs.haskellPackages.callCabal2nix "fourmolu" inputs.fourmolu
              {}))
        ];
      };
    });
}
