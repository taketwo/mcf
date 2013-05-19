#!/usr/bin/env python
# encoding: utf-8

import os
import subprocess

import install


files = [('.Xmodmap', 'Remap CapsLock to Ctrl'),
         ('.dircolors', 'Dircolors for Solarized palette'),
         ('.fonts', 'Custom fonts'),
         ('.vim', 'VIM settings/plugins folder'),
         ('.vimrc', 'VIM configuration file'),
         ('.bashrc', 'Bash configuration file'),
         ('.mime.types', 'Custom MIME types'),
         ('.mailcap', 'Custom MIME type-to-program mappings'),
         ('texmf', 'Latex packages'),
         ('.gitconfig', 'GIT configuration file')]

deb_packages = ['python-pip',
                'exuberant-ctags',
                'xsel',
                'vim',
                'tree',
                'gitk',
                'git-gui',
                'powertop',
                'ssh']

pypi_packages = ['pyflakes', 'pep8', 'flake8', 'legit']

encrypted = ['.netrc', '.secrets']


def link(filename, desc):
    home = os.path.expanduser('~')
    src = os.path.join('.mcf', filename)
    dest = os.path.join(home, filename)
    if os.path.lexists(dest):
        os.unlink(dest)
    os.symlink(src, dest)
    print '[*]', desc
    print '   ', dest, '->', src


def pypi(package):
    print '[*]', package
    cmd = 'sudo pip install --upgrade %s' % package
    subprocess.call(cmd.split())


def decrypt(filename):
    home = os.path.expanduser('~')
    dest = os.path.join(home, filename)
    print '[*]', filename
    cmd = 'openssl des3 -d -in %s -out %s' % (filename, dest)
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

    print 'Decrypting secret files...'
    print ''
    for f in encrypted:
        decrypt(f)
    print ''

    install.deb(deb_packages)
    print ''

    print 'Installing python packages...'
    print ''
    for p in pypi_packages:
        pypi(p)
    print ''

    print 'Configuring Gnome terminal...'
    print ''
    terminal = os.path.join(mcf, 'scripts', 'install', 'setup-terminal.bash')
    subprocess.call(terminal)
    print 'Note: run the script manually if the terminal setup failed:'
    print '      $', terminal
    print ''

    print 'Setting wallpaper...'
    print ''
    wallpaper = os.path.join(mcf, 'scripts', 'install', 'setup-wallpaper.bash')
    subprocess.call(wallpaper)
    print 'Note: run the script manually if the wallpaper setup failed:'
    print '      $', wallpaper
    print ''
    print 'Installation completed.'
    print 'You will need to install clang manually.'

# Clang installation.
# 1) Follow instructions at http://clang.llvm.org/get_started.html
# 2) Configure with options:
#    ../configure --enable-optimized --disable-assertions --prefix=/opt/llvm
