#!/usr/bin/env python
# encoding: utf-8

import os
import argparse
import subprocess

from install import GitRepository, make_install


def replace(e, f):
    cmd = ['sed', '-i', '-e', e, f]
    subprocess.call(cmd)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Install dzen2 from source.

    Steps involved:
     - clone github repository into a temp folder
     - configure wit Xinerama, XPM, and XFT support
     - make install (will go to /usr/local/)
     - remove temp folder
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    args = parser.parse_args()

    with GitRepository('geier/dzen') as repo:
        # Comment option 1
        replace('/No Xinerama no XPM no XFT/,+2s/^/#/', 'config.mk')
        # Uncomment option 7
        replace('/With Xinerama and XPM and XFT/,+2s/^#//', 'config.mk')
        # Make and install
        make_install()
