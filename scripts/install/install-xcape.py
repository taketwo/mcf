#!/usr/bin/env python
# encoding: utf-8

import os
import argparse
import subprocess

from install import GitRepository, make_install, apt


def replace(e, f):
    cmd = ['sed', '-i', '-e', e, f]
    subprocess.call(cmd)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Install xcape from source.

    Steps involved:
     - clone github repository into a temp folder
     - make install (will go to /usr/local/)
     - remove temp folder
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    args = parser.parse_args()

    apt(['libx11-dev', 'libxtst-dev', 'libxi-dev'])

    with GitRepository('alols/xcape') as repo:
        # Change installation path to /usr/local
        replace('s/\/usr/\/usr\/local/', 'Makefile')
        # Make and install
        make_install()
