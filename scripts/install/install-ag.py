#!/usr/bin/env python
# encoding: utf-8

import os
import sys
import argparse
import subprocess

from install import GitRepository, make_install, deb

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Clone ag, configure, build, and install.
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    args = parser.parse_args()

    deb(['automake', 'pkg-config', 'libpcre3-dev', 'liblzma-dev', 'zlib1g-dev'])

    with GitRepository('ggreer/the_silver_searcher') as repo:
        subprocess.call(['./build.sh'])
        make_install()
