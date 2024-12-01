{
  description = "Haskell configuration with flake-utils.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {
        devShell = pkgs.mkShell {
          packages = with pkgs; [
            bashInteractive
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.hlint
            haskellPackages.hindent
          ];
        };
      });
}
