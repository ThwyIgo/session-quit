{ pkgs ? import <nixpkgs> {} }:

(pkgs.callPackage ./default.nix { }).session-quit.env.overrideAttrs (finalAttrs: previousAttrs: {
  buildInputs = previousAttrs.buildInputs ++ (with pkgs; [
    cabal-install
    cabal2nix
  ]);
})
