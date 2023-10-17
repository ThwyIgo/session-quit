{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  inputsFrom = [ (pkgs.callPackage ./default.nix {}).session-quit.env ];

  packages = with pkgs; [
    cabal2nix
    cabal-install
  ];
}
