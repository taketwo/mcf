#!/usr/bin/env python

import os
import subprocess


files = [('.Xmodmap', 'Remap CapsLock to Escape'),
         ('.dircolors', 'Dircolors for Solarized palette'),
         ('.fonts', 'Custom fonts'),
         ('.vim', 'VIM settings/plugins folder'),
         ('.vimrc', 'VIM configuration file'),
         ('.bashrc', 'Bash configuration file'),
         ('.mime.types', 'Custom MIME types'),
         ('.mailcap', 'Custom MIME type-to-program mappings'),
         ('.gitconfig', 'GIT configuration file')]

deb_packages = ['python-pip',
                'exuberant-ctags',
                'xsel',
                'ack-grep']

pypi_packages = ['flake8', 'pep8', 'legit']


def link(filename, desc):
    home = os.path.expanduser('~')
    src = os.path.join('.mcf', filename)
    dest = os.path.join(home, filename)
    if os.path.lexists(dest):
        os.unlink(dest)
    os.symlink(src, dest)
    print '[*]', desc
    print '   ', dest, '->', src


def deb(package):
    print '[*]', package
    cmd = 'sudo apt-get install %s' % package
    subprocess.call(cmd.split())


def pypi(package):
    print '[*]', package
    cmd = 'sudo pip install --upgrade %s' % package
    subprocess.call(cmd.split())


if __name__ == '__main__':
    home = os.path.expanduser('~')
    mcf = os.path.join(home, '.mcf')

    print 'MCF install script.'
    print ''

    print 'Creating symlinks...'
    print ''
    for f, d in files:
        link(f, d)
    print ''

    print 'Installing debian packages...'
    print ''
    for p in deb_packages:
        deb(p)
    print ''

    print 'Installing python packages...'
    print ''
    for p in pypi_packages:
        pypi(p)
    print ''

    print 'Configuring gnome terminal for Solarized palette...'
    print ''
    subprocess.call(os.path.join(mcf, 'scripts',
                                 'install-solarized-colors.bash'))

    print 'Setting wallpaper...'
    print ''
    uri = 'file://' + os.path.join(mcf, 'wallpapers', 'stabilis.jpg')
    subprocess.call(('gsettings set org.gnome.desktop.background picture-uri' +
                     ' ' + uri).split())
    print ''
    print 'Installation completed.'
    print 'You will need to install clang manually.'

# Clang installation.
# 1) Follow instructions at http://clang.llvm.org/get_started.html
# 2) Configure with options:
#    ../configure --enable-optimized --disable-assertions --prefix=/opt/llvm
