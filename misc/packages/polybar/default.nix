let
  nixpkgs = import <nixpkgs> {};
  pkgs = with nixpkgs; {
    polybar = callPackage ./polybar.nix { };
  };
in pkgs
