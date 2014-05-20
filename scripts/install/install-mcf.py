#!/usr/bin/env python3
# encoding: utf-8

import os
import sys
import subprocess

from os.path import expanduser, join
sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))

import install


files = [('.dircolors', 'Dircolors for Solarized palette'),
         ('.fonts', 'Custom fonts'),
         ('.vim', 'VIM settings/plugins folder'),
         ('.vimrc', 'VIM configuration file'),
         ('.bashrc', 'Bash configuration file'),
         ('.inputrc', 'Readline configuration file'),
         ('.pam_environment', 'User environment setup'),
         ('.mime.types', 'Custom MIME types'),
         ('.mailcap', 'Custom MIME type-to-program mappings'),
         ('.xinitrc', 'X client script'),
         ('texmf', 'Latex packages'),
         ('.gitconfig', 'GIT configuration file'),
         ('.tmux.conf', 'Tmux configuration file'),
         (('fish', '.config/fish'), 'Fish configuration folder')]

apt_packages = ['python-pip',
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
                'trayer',
                'tmux',
                'redshift',
                'fish',
                'python-keyring',                          # keyring-password
                'xdotool',                                 # vim paste
               ]

pacman_packages = ['python-pip',
                   'python-keyring',                       # keyring-password
                   'xorg-setxkbmap',                       # keyboard
                   'xorg-xprop',                           # xprop
                   'xdotool',                              # vim paste
                   'git',                                  # gitk
                   'xsel',                                 # vim clipboard
                   'gvim',                                 # vim with python
                   'ctags',                                # exuberant ctags
                   'tree',
                   'powertop',
                   'htop',
                   'openvpn',
                   'tmux',
                   'redshift',
                  ]

pip_packages = ['pyflakes',
                'pep8',
                'flake8',
               ]

encrypted = ['.netrc', '.secrets', '.kebrum']


def link(paths, desc):
    home = expanduser('~')
    paths = paths if isinstance(paths, tuple) else (paths, paths)
    src = join(home, '.mcf', paths[0])
    dest = join(home, paths[1])
    if os.path.lexists(dest):
        os.unlink(dest)
    os.symlink(src, dest)
    print('[*]', desc)
    print('   ', dest, '->', src)


def decrypt(filename):
    home = expanduser('~')
    dest = join(home, filename)
    print('[*]', filename)
    cmd = 'openssl aes-256-cbc -d -a -in %s -out %s' % (filename, dest)
    subprocess.call(cmd.split())
    os.chmod(dest, 0O600)


if __name__ == '__main__':
    home = expanduser('~')
    mcf = join(home, '.mcf')

    print('MCF install script.')
    print('')

    print('Creating symlinks...')
    print('')
    for f, d in files:
        link(f, d)
    print('')

    print('Decrypting secret files...')
    print('')
    for f in encrypted:
        decrypt(f)
    print('')

    install.apt(apt_packages)
    print('')

    install.pip(pip_packages)
    print('')

    print('Configuring Gnome terminal...')
    print('')
    terminal = join(mcf, 'scripts', 'install', 'setup-terminal.bash')
    subprocess.call(terminal)
    print('Note: run the script manually if the terminal setup failed:')
    print('      $', terminal)
    print('')

    print('Setting wallpaper...')
    print('')
    wallpaper = join(mcf, 'scripts', 'install', 'setup-wallpaper.bash')
    subprocess.call(wallpaper)
    print('Note: run the script manually if the wallpaper setup failed:')
    print('      $', wallpaper)
    print('')

    print('Configuring powerline-shell...')
    print('')
    os.chdir(join(mcf, 'scripts', 'bundle', 'powerline-shell'))
    subprocess.call(['python2', 'install.py'])
    print('')

    print('Installation completed.')
