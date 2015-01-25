#!/usr/bin/env python3
# encoding: utf-8

import os
import sys
import subprocess
from os.path import expanduser, join

print('[*] Bootstrap MCF')
print('')

print('[*] Obtain latest MCF sources')
try:
    if os.path.isdir('.mcf'):
        print('    .mcf folder already exists, pulling the latest version')
        os.chdir('.mcf')
        cmd = 'git pull'
        subprocess.check_call(cmd.split())
        cmd = 'git submodule sync'
        subprocess.check_call(cmd.split())
    else:
        print('    cloning the repository')
        cmd = 'git clone -b mpm --recursive https://code.google.com/p/mcf .mcf'
        subprocess.check_call(cmd.split())
except subprocess.CalledProcessError:
    os.exit('Failed to obtain MCF sources, aborting...')
print('')

print('[*] Import install script')
library = join(expanduser('~'), '.mcf/scripts/library')
os.environ['PYTHONPATH'] = library
sys.path.append(library)
from package_manager import install
print('')

print('[*] Install MCF')
print('')

install('mcf', verbose=True)
