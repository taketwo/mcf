#!/usr/bin/env python

import os
import subprocess
import shutil
import sys


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

deb_packages = ['cabal-install', 'libx11-dev', 'libxrandr-dev', 'libxft-dev', 'libxinerama-dev']

cabal_packages = ['xmonad', 'xmonad-contrib']


def cabal(package):
    print '[*]', package
    cmd = 'cabal install --global --force-reinstalls %s' % package
    subprocess.call(cmd.split())


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


def deb(package):
    print '[*]', package
    cmd = 'sudo apt-get install %s' % package
    subprocess.call(cmd.split())


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

    print 'Installing debian packages...'
    print ''
    for p in deb_packages:
        deb(p)
    print ''

    print 'Installing cabal packages...'
    print ''
    print 'Get the latest list of available cabal packages'
    subprocess.call(['cabal', 'update'])
    print ''
    for p in cabal_packages:
        cabal(p)
    print ''

