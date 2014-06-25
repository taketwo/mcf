#!/usr/bin/env python3

import os
import sys
import shutil
import platform

# The script may be run before finishing MCF installation or as root, in which
# case PYTHONPATH may not be set up properly.
sys.path.append('/home/sergey/.mcf/scripts/library')
import install

files = [('',
          '/home/sergey/.xmonad',
          'Xmonad settings folder'),
         ('images/custom_xmonad_badge.png',
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

apt_packages = ['cabal-install',
                'libx11-dev',
                'libxrandr-dev',
                'libxft-dev',
                'libxinerama-dev',
                'libxpm-dev']

cabal_packages = ['xmonad',
                  'xmonad-contrib']

pacman_packages = ['xmonad',
                   'xmonad-contrib',
                   'xorg-xmessage']


if __name__ == '__main__':
    if os.getuid() != 0:
        sys.exit('This script needs to be run as root.')

    print('Xmonad install script.')
    print('')

    if platform.linux_distribution()[0] == 'arch':
        install.pacman(pacman_packages)
    else:
        install.apt(apt_packages)
        print('')
        install.cabal(cabal_packages)
    print('')

    print('Creating symlinks...')
    print('')
    for fs, fd, d in files:
        basename = os.path.basename(fs)
        src = os.path.join('/home/sergey/.mcf/.xmonad', fs)
        dest = os.path.join(fd, basename) if basename else fd
        install.link(src, dest, d)
    print('')

    print('Installation ALMOST completed.')
    print('You will need to install xmonad panel manually:')
    print('      $', os.path.join(os.path.split(sys.argv[0])[0], 'install-xmonad-panel.py'))
