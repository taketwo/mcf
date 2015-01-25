#!/usr/bin/env python
# encoding: utf-8

import sys
from package_manager import install

if __name__ == '__main__':
    install(sys.argv[1], True)
