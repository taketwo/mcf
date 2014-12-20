#!/usr/bin/env python
# encoding: utf-8

import argparse
import subprocess

from install import GitRepository, apt

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Clone libgit2, configure, build, and install.
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    args = parser.parse_args()

    apt(['cmake'])

    with GitRepository('libgit2/libgit2') as repo:
        subprocess.call('cmake .'.split())
        subprocess.call('cmake --build .'.split())
        subprocess.call('sudo cmake --build . --target install'.split())
