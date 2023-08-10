{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {...}:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      overlay = final: prev: {haskellPackages = prev.haskell.packages.ghc92;};
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay];
      };
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.haskellPackages.shellFor {
        packages = _: [
          (pkgs.haskellPackages.callCabal2nix "game-boy-emulator-hs" (pkgs.lib.cleanSource ./.) {})
        ];
        nativeBuildInputs = [
          pkgs.cabal-install
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.cabal-fmt
        ];
      };
    });
}
