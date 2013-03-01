#!/usr/bin/env python

import os
import re
import git
import shutil
import subprocess

if __name__ == '__main__':
    info = subprocess.check_output(['git', 'remote', 'show', 'origin'])
    print info
    remote_url = re.search(r'Fetch URL: (.+)\n', info).groups()[0]
    print 'Remote URL:', remote_url
    folder = os.path.split(os.getcwd())[-1]
    print 'Repository folder:', folder
    os.chdir('..')
    shutil.rmtree(folder)
    os.mkdir(folder)
    repo = git.Git().clone(remote_url, folder, recursive=True)
