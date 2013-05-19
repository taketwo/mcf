#!/usr/bin/env python

import os
import shutil
import sys

# The script may be run before finishing MCF installation or as root, in whic
# PYTHONPATH may not be set up properly.
from os.path import expanduser, join
sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))
import install

files = [('images/custom_xmonad_badge.png',
          '/usr/share/unity-greeter',
          'Custom Xmonad badge for the login screen'),
         ('entries/xsessions/xmonad.desktop',
          '/usr/share/xsessions',
          'Xmonad xsession configuration'),
         ('entries/applications/xmonad.desktop',
          '/usr/share/applications',
          'Xmonad applications entry'),
         ('entries/sessions/xmonad.session',
          '/usr/share/gnome-session/sessions',
          'Xmonad GNOME session configuration')]

deb_packages = ['cabal-install', 'libx11-dev', 'libxrandr-dev', 'libxft-dev',
                'libxinerama-dev', 'libxpm-dev', 'xdotool', 'conky-all']

cabal_packages = ['xmonad', 'xmonad-contrib']


def remove(dest):
    try:
        os.unlink(dest)
        return True
    except OSError:
        pass
    try:
        os.remove(dest)
        return True
    except OSError:
        pass
    try:
        shutil.rmtree(dest)
        return True
    except OSError:
        pass


def link(src, dest, desc):
    home = os.path.expanduser('~')
    basename = os.path.basename(src)
    if os.path.isabs(dest):
        src = os.path.join(home, '.xmonad', src)
    else:
        dest = os.path.join(home, dest)
        src = os.path.join('.xmonad', src)
    dest = os.path.join(dest, basename)
    if os.path.lexists(dest):
        remove(dest)
    os.symlink(src, dest)
    print '[*]', desc
    print '   ', dest, '->', src


if __name__ == '__main__':
    if os.getuid() != 0:
        print 'This script needs to be run as root.'
        sys.exit(1)

    home = os.path.expanduser('~')
    mcf = os.path.join(home, '.mcf')
    xmonad = os.path.join(home, '.xmonad')

    print 'Xmonad install script.'
    print ''

    print 'Creating symlinks...'
    print ''
    print '[*] Xmonad settings folder'
    if os.path.lexists(xmonad):
        remove(xmonad)
    os.symlink('.mcf/.xmonad', xmonad)
    print '   ', xmonad, '-> .mcf/.xmonad'
    for fs, fd, d in files:
        link(fs, fd, d)
    print ''

    install.deb(deb_packages)
    print ''

    install.cabal(cabal_packages)
    print ''

    print 'Installation ALMOST completed.'
    print 'You will need to install dzen2 manually:'
    print '      $', os.path.join(os.path.split(sys.argv[0])[0], 'install-dzen2.py')
