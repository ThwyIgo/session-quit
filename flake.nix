{
  description = "session-quit flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
          session-quit = pkgs.lib.sources.cleanSource ./.;
        });
      in {
        packages.default = haskellPackages.session-quit;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.session-quit ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
          ];
        };
      });
}
