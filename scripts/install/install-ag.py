#!/usr/bin/env python
# encoding: utf-8

import os
import sys
import argparse
from subprocess import call

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Clone ag, configure, build, and install.
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('path', help='checkout path')
    args = parser.parse_args()

    if not os.path.isdir(args.path):
        sys.exit('Folder "%s" does not exist, create it first.' % args.path)

    cmd = 'sudo apt-get install -y automake pkg-config libpcre3-dev'
    call(cmd.split())

    f = os.path.join(args.path, 'ag')
    cmd = 'git clone https://github.com/ggreer/the_silver_searcher.git %s'
    call((cmd % f).split())

    os.chdir(f)
    call(['./build.sh'])
    call(['sudo', 'make', 'install'])

    print 'Installation completed.'
