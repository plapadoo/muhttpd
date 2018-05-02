{ pkgs ? import <nixpkgs> {} }:
let
  drv = pkgs.haskellPackages.callCabal2nix "muhttpd" ./. {};
in
  drv
