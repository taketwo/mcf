import os
import re
import tempfile
import shutil
import subprocess

class GitRepository:

    def __init__(self, url):
        """
        Arguments
        ---------
        url: str
            URL of the repository to clone. Github repositories can be given in
            a short form 'USER/REPOSITORY' (e.g. 'taketwo/vim-ros') and will be
            automaticall expanded.
        """
        if re.search(r'^\w+/\w+$', url):
            self.url = 'https://github.com/%s.git' % url
        else:
            self.url = url

    def __enter__(self):
        self.path = tempfile.mkdtemp()
        cmd = 'git clone --recursive %s %s' % (self.url, self.path)
        subprocess.call(cmd.split())
        self.original_cwd = os.getcwd()
        os.chdir(self.path)
        return self

    def __exit__(self, type, value, traceback):
        shutil.rmtree(self.path)
        os.chdir(self.original_cwd)


def make_install():
    subprocess.call(['make'])
    subprocess.call(['sudo', 'make', 'install'])


def deb(package):
    p = package if isinstance(package, list) else [package]
    print 'Installing debian package%s:' % ('s' if len(p) > 1 else '')
    for pk in p:
        print '  [*]', pk
    cmd = 'sudo apt-get install -y %s' % ' '.join(p)
    subprocess.call(cmd.split())
