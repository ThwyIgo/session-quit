{
  description = "A simple flake for a Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in {
        packages.default = haskellPackages.callCabal2nix "session-quit" self { };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            (haskellPackages.callCabal2nix "session-quit" self { })
            # Add other dev dependencies here
            pkg-config
            gtk3
            gobject-introspection
          ];
        };
      });
}
