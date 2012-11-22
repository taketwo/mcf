#!/usr/bin/env python

import os
import subprocess
import shutil
import sys


files = [('images/custom_xmonad_badge.png',
          '/usr/share/unity-greeter',
          'Custom Xmonad badge for the login screen'),
         ('xmonad.desktop',
          '/usr/share/xsessions',
          'Xmonad desktop entry'),
         ('xmonad.session',
          '/usr/share/gnome-session/sessions',
          'Xmonad GNOME session configuration')]

deb_packages = ['xmonad', 'kupfer']


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

    #print 'Configuring gnome terminal for Solarized palette...'
    #print ''
    #subprocess.call(os.path.join(mcf, 'scripts',
                                 #'install-solarized-colors.bash'))

    #print 'Setting wallpaper...'
    #print ''
    #uri = 'file://' + os.path.join(mcf, 'wallpapers', 'stabilis.jpg')
    #subprocess.call(('gsettings set org.gnome.desktop.background picture-uri'+
                     #' ' + uri).split())
    #print ''
    #print 'Installation completed.'
    #print 'You will need to install clang manually.'

# Clang installation.
# 1) Follow instructions at http://clang.llvm.org/get_started.html
# 2) Configure with options:
#    ../configure --enable-optimized --disable-assertions --prefix=/opt/llvm
