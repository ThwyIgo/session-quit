{ pkgs ? import <nixpkgs> {} }:

{ session-quit = pkgs.haskellPackages.callPackage ./session-quit.nix {}; }
