#!/usr/bin/env python
# encoding: utf-8

import os
import sys
import subprocess


def get_package_name():
    return os.path.basename(os.getcwd())


def get_includes(pack):
    includes = subprocess.check_output(["rospack", "cflags-only-I",
                                        pack]).strip().split(' ')
    include_dir = os.path.join(os.getcwd(), 'include')
    if os.path.isdir(include_dir):
        includes.append(include_dir)
    cfg_dir = os.path.join(os.getcwd(), 'cfg')
    if os.path.isdir(cfg_dir):
        includes.append(os.path.join(cfg_dir, 'cpp'))
    return ['-I' + f for f in sorted(includes)]


def create_vim(pack):
    vim = open('.vim', 'w')
    vim.write('let makeprg="rosmake\ %s"\n' % pack)
    vim.write('au FileType cpp :UltiSnipsAddFiletypes roscpp\n')
    vim.write('au FileType python :UltiSnipsAddFiletypes rospy\n')
    vim.write('au BufRead,BufNewFile */manifest.xml :UltiSnipsAddFiletypes rosmanifest\n')
    vim.write('au BufRead,BufNewFile *.cfg set filetype=python | :UltiSnipsAddFiletypes roscfg.python')
    vim.close()


def create_clang_complete(includes):
    clang = open('.clang_complete', 'w')
    clang.write('-std=c++0x\n')
    clang.write('%s' % '\n'.join(includes))
    clang.close()

if __name__ == '__main__':
    pack = get_package_name()
    try:
        includes = get_includes(pack)
    except:
        sys.exit('Failed to get the includes for the package. Aborting...')
    #create_vim(pack)
    create_clang_complete(includes)
