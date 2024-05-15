{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  inputsFrom = [ (pkgs.callPackage ./default.nix {}).env ];

  packages = with pkgs; [
    cabal2nix
    cabal-install
  ];
}
