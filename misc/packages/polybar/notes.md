Why custom derivation
---------------------

Ideally, I would like to install Polybar directly from nixpkgs. However, it
comes with MPD and PulseAudio options disabled by default. To date I have not
found a way to `nix-env -i` a package with certain options enabled. Answer [1]
suggests that this should be possible with:

    nix-env -iA nixpkgs.polybar --arg pulseSupport true --arg mpdSupport true --arg iwSupport true

Unfortunately, does not work for me, I get exactly the same version of Polybar.

How to install
--------------

From this directory run:

    ni -f default.nix

How to update to a new version
------------------------------

Assuming there are no big changes to Polybar, updating to a newer version
amounts to changing "version" and "sha256" fields in `polybar.nix` and running
`mpm polybar` to re-install the package. The actual SHA value can be copied from
the official derivation [2].

References
----------

[1] https://stackoverflow.com/a/51861809/1525865
[2] https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/misc/polybar/default.nix
