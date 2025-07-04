let
  nixpkgs = import <nixpkgs> {};
  pkgs = with nixpkgs; {
    lnav = callPackage ./lnav.nix { };
  };
in pkgs
