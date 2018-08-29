{ cairo, cmake, fetchgit, libXdmcp, libpthreadstubs, libxcb, pcre, pkgconfig
, python2 , stdenv, xcbproto, xcbutil, xcbutilimage, xcbutilrenderutil
, xcbutilwm, xcbutilxrm, fetchpatch

# optional packages-- override the variables ending in 'Support' to enable or
# disable modules
, alsaSupport       ? true,  alsaLib       ? null
, pulseaudioSupport ? true,  libpulseaudio ? null
, iwSupport         ? true,  wirelesstools ? null
, githubSupport     ? false, curl          ? null
, mpdSupport        ? true,  mpd_clientlib ? null
, i3Support         ? false, i3GapsSupport ? false, i3 ? null, i3-gaps ? null, jsoncpp ? null
}:

assert alsaSupport       -> alsaLib       != null;
assert pulseaudioSupport -> libpulseaudio != null;
assert githubSupport     -> curl          != null;
assert iwSupport         -> wirelesstools != null;
assert mpdSupport        -> mpd_clientlib != null;

assert i3Support     -> ! i3GapsSupport && jsoncpp != null && i3      != null;
assert i3GapsSupport -> ! i3Support     && jsoncpp != null && i3-gaps != null;

stdenv.mkDerivation rec {
    name = "polybar-${version}";
    version = "3.2.1-dev";
    src = fetchgit {
      url = "https://github.com/jaagr/polybar";
      rev = "e4b7c96e3f121e7f37dfaa9610c9ecddf07363a7";
      sha256 = "1hj2989fnfpj1znh2wkx5w56psxkx753vc9rw7gf22d75gpp9h9v";
      fetchSubmodules = true;
    };

    meta = with stdenv.lib; {
      description = "A fast and easy-to-use tool for creatin status bars.";
      longDescription = ''
        Polybar aims to help users build beautiful and highly customizable
        status bars for their desktop environment, without the need of
        having a black belt in shell scripting.
      '';
      license = licenses.mit;
      maintainers = [ maintainers.afldcr ];
      platforms = platforms.unix;
    };

    buildInputs = [
      cairo libXdmcp libpthreadstubs libxcb pcre python2 xcbproto xcbutil
      xcbutilimage xcbutilrenderutil xcbutilwm xcbutilxrm

      (if alsaSupport       then alsaLib       else null)
      (if pulseaudioSupport then libpulseaudio else null)
      (if githubSupport     then curl          else null)
      (if iwSupport         then wirelesstools else null)
      (if mpdSupport        then mpd_clientlib else null)

      (if i3Support || i3GapsSupport then jsoncpp else null)
      (if i3Support then i3 else null)
      (if i3GapsSupport then i3-gaps else null)
    ];

    nativeBuildInputs = [
      cmake pkgconfig
    ];
}
