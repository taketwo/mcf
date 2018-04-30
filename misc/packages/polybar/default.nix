let
  nixpkgs = import <nixpkgs> {};
  pkgs = with nixpkgs; {
    polybar = nixpkgs.polybar.override {
      mpdSupport = true;
    };
  };
in pkgs
