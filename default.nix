{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./session-quit.nix {}
