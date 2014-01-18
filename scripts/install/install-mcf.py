#!/usr/bin/env python
# encoding: utf-8

import os
import sys
import subprocess

from os.path import expanduser, join
sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))

import install


files = [('.Xmodmap', 'Remap CapsLock to Ctrl'),
         ('.dircolors', 'Dircolors for Solarized palette'),
         ('.fonts', 'Custom fonts'),
         ('.vim', 'VIM settings/plugins folder'),
         ('.vimrc', 'VIM configuration file'),
         ('.bashrc', 'Bash configuration file'),
         ('.inputrc', 'Readline configuration file'),
         ('.pam_environment', 'User environment setup'),
         ('.mime.types', 'Custom MIME types'),
         ('.mailcap', 'Custom MIME type-to-program mappings'),
         ('texmf', 'Latex packages'),
         ('.gitconfig', 'GIT configuration file'),
         ('.tmux.conf', 'Tmux configuration file')]

deb_packages = ['python-pip',
                'exuberant-ctags',
                'xsel',
                'vim',
                'tree',
                'gitk',
                'git-gui',
                'legit',
                'powertop',
                'htop',
                'openvpn',
                'ssh',
                'curl',
                'trayer',
                'tmux',
                'redshift']

pypi_packages = ['pyflakes', 'pep8', 'flake8']

encrypted = ['.netrc', '.secrets', '.kebrum']


def link(filename, desc):
    home = expanduser('~')
    src = join('.mcf', filename)
    dest = join(home, filename)
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
    home = expanduser('~')
    dest = join(home, filename)
    print '[*]', filename
    cmd = 'openssl aes-256-cbc -d -a -in %s -out %s' % (filename, dest)
    subprocess.call(cmd.split())
    os.chmod(dest, 0600)


if __name__ == '__main__':
    home = expanduser('~')
    mcf = join(home, '.mcf')

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
    terminal = join(mcf, 'scripts', 'install', 'setup-terminal.bash')
    subprocess.call(terminal)
    print 'Note: run the script manually if the terminal setup failed:'
    print '      $', terminal
    print ''

    print 'Setting wallpaper...'
    print ''
    wallpaper = join(mcf, 'scripts', 'install', 'setup-wallpaper.bash')
    subprocess.call(wallpaper)
    print 'Note: run the script manually if the wallpaper setup failed:'
    print '      $', wallpaper
    print ''

    print 'Configuring powerline-shell...'
    print ''
    os.chdir(join(mcf, 'scripts', 'bundle', 'powerline-shell'))
    subprocess.call('./install.py')
    print ''

    print 'Installation completed.'
    print 'You will need to install clang manually.'

# Clang installation.
# 1) Follow instructions at http://clang.llvm.org/get_started.html
# 2) Configure with options:
#    ../configure --enable-optimized --disable-assertions --prefix=/opt/llvm
