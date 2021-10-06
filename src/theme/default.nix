{ stdenv, writeText ? (import <nixpkgs> {}).pkgs.writeText, lib }:
writeText "XMonadTheme.hs" (import ./xmonadTheme.nix { inherit lib; })
