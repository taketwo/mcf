#!/usr/bin/env python
# encoding: utf-8

import argparse
import subprocess

from install import GitRepository, apt


def replace(e, f):
    cmd = ['sed', '-i', '-e', e, f]
    subprocess.call(cmd)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Install Fantasque Sans Mono font from source.

    Steps involved:
     - clone github repository into a temp folder
     - make install
     - remove temp folder
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    args = parser.parse_args()

    apt(['fontforge', 'ttfautohint', 'woff-tools'])

    with GitRepository('belluzj/fantasque-sans') as repo:
        # Completely remove line with ttf2eot command
        replace('s/^\tttf2eot.*$//', 'Makefile')
        # Make install
        subprocess.call(['make', 'install'])
