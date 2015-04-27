#!/usr/bin/env python3
# encoding: utf-8

import os
import sys
import argparse
import platform
import subprocess

import mcf
from install import GitRepository, make_install, install_dependencies


DEPENDENCIES = {'apt': ['libxft-dev',
                        'libxinerama-dev',
                        'libxpm-dev',
                        'curl',
                        'conky-all'],
                'pacman': ['curl',                     # gmail.sh
                           'alsa-utils',               # volume.sh
                           'ttf-liberation'            # font used in the panel
                          ],
                'aura': ['conky-cli',
                         'dzen2-xft-xpm-xinerama-git'],
                'pip': ['unidecode',
                        'beautifulsoup4',              # xfm.py
                        'requests'                     # toggl.sh
                       ]}


def run_tests():
    os.chdir(mcf.XMONAD)
    print('1. Testing panel components individually...')
    print('')
    components = ['gmail.sh',
                  'layout.sh',
                  'network.sh',
                  'rhythmbox.sh',
                  'volume.sh',
                  'xfm.py',
                  'toggl.sh']
    for c in components:
        print('[*] %s' % c)
        try:
            print('   ', subprocess.check_output(['./%s' % c]))
        except subprocess.CalledProcessError:
            print('   FAILED')
            return 1
    print('')
    print('2. Testing conky script...')
    print('')
    cmd = 'conky -c conkyrc -i 1'
    try:
        print(subprocess.check_output(cmd.split()))
    except subprocess.CalledProcessError:
        print('FAILED')
        return 1
    return 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Install packages needed to compose and display my Xmonad panel.

    This includes dzen2, conky, and various helper packages.

    On Ubuntu installs dzen2 from source. Steps involved:
     - clone github repository into a temp folder
     - configure wit Xinerama, XPM, and XFT support
     - make install (will go to /usr/local/)
     - remove temp folder
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--test', action='store_true',
                        help='run tests to check if all panel components work')
    args = parser.parse_args()

    if args.test:
        sys.exit(run_tests())

    install_dependencies(DEPENDENCIES)

    if platform.linux_distribution()[0] == 'Ubuntu':
        with GitRepository('robm/dzen') as repo:
            def replace(e, f):
                cmd = ['sed', '-i', '-e', e, f]
                subprocess.call(cmd)
            # Comment option 1
            replace('/No Xinerama no XPM no XFT/,+2s/^/#/', 'config.mk')
            # Uncomment option 7
            replace('/With Xinerama and XPM and XFT/,+2s/^#//', 'config.mk')
            # Make and install
            make_install()

    print('Run this script with \'--test\' option to verify installation.')
