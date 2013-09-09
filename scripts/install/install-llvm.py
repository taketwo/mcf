#!/usr/bin/env python
# encoding: utf-8

import os
import sys
import argparse
from subprocess import call

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''
    Check out llvm/clang, configure, build, and install.
    ''', formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('path', help='checkout path (a folder "llvm" will be '
                                     'created there automatically)')
    args = parser.parse_args()

    if not os.path.isdir(args.path):
        sys.exit('Folder "%s" does not exist, create it first.' % args.path)

    repos = [('llvm/trunk', 'llvm'),
             ('cfe/trunk', 'llvm/tools/clang'),
             ('clang-tools-extra/trunk', 'llvm/tools/clang/tools/extra'),
             ('compiler-rt/trunk', 'llvm/projects/compiler-rt')]

    for r, f in repos:
        cmd = 'svn co http://llvm.org/svn/llvm-project/%s %s'
        call((cmd % (r, os.path.join(args.path, f))).split())

    build = os.path.join(args.path, 'llvm/build')
    os.mkdir(build)
    os.chdir(build)

    options = '--enable-optimized --disable-assertions --prefix=/opt/llvm'
    call(['../configure'] + options.split())

    call(['make', '-j2'])
    call(['sudo', 'make', 'install'])

    print 'Installation completed.'
