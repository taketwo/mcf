import os
import re
import shutil
import platform
import tempfile
import subprocess


class GitRepository:

    def __init__(self, url, shallow=True, recursive=True):
        """
        Arguments
        ---------
        url: str
            URL of the repository to clone. Github repositories can be given in
            a short form 'USER/REPOSITORY' (e.g. 'taketwo/vim-ros') and will be
            automatically expanded.
        """
        self.shallow = shallow
        self.recursive = recursive
        if re.search(r'^[\w_-]+/[\w_-]+$', url):
            self.url = 'https://github.com/%s.git' % url
        else:
            self.url = url

    def __enter__(self):
        self.path = tempfile.mkdtemp()
        depth = '--depth 1' if self.shallow else ''
        recursive = '--recursive' if self.recursive else ''
        cmd = 'git clone %s %s %s %s' % (recursive, depth, self.url, self.path)
        subprocess.call(cmd.split())
        self.original_cwd = os.getcwd()
        os.chdir(self.path)
        return self

    def __exit__(self, type, value, traceback):
        shutil.rmtree(self.path)
        os.chdir(self.original_cwd)

    def __getitem__(self, key):
        if isinstance(key, tuple):
            return os.path.join(self.path, *key)
        return os.path.join(self.path, key)


def make_install():
    subprocess.call(['make'])
    subprocess.call(['sudo', 'make', 'install'])


def install_package(manager, package, args=''):
    """
    Install package(s).

    Arguments
    ---------
    manager: str
        Name of a package manager to use (apt, pacman, pip, aura).
    package: str | list
        Package name or list of package names.
    args:
        Additional options to pass to the package manager.
    """
    CMD = {'apt': 'sudo apt-get install --force-yes -y',
           'pacman': 'sudo pacman --noconfirm -S',
           'aura': 'sudo aura --noconfirm -A',
           'pip': 'sudo pip install --upgrade'}
    if not manager in CMD.keys():
        raise Exception('Unsupported manager')
    p = package if isinstance(package, list) else [package]
    print('Installing %s package%s...' % (manager, 's' if len(p) > 1 else ''))
    print('')
    for pk in p:
        print('[*]', pk)
    cmd = CMD[manager] + ' ' + ' '.join(p) + ' ' + args
    subprocess.call(cmd.split())
    print('')


def apt(package):
    """
    Shortcut to install debian packages using apt-get.
    """
    install_package('apt', package)


def pacman(package):
    """
    Shortcut to install pacman packages.
    """
    install_package('pacman', package)


def aura(package):
    """
    Shortuct to install AUR packages using aura.
    """
    install_package('aura', package)


def pip(package):
    """
    Shortcut to install PyPI packages using pip.
    """
    install_package('pip', package)


def cabal(package, local=False):
    p = package if isinstance(package, list) else [package]
    print('Installing cabal package%s:' % ('s' if len(p) > 1 else ''))
    for pk in p:
        print('  [*]', pk)
    if local:
        cmd = 'cabal install --force-reinstalls %s' % ' '.join(p)
    else:
        cmd = 'sudo cabal install --global --force-reinstalls %s' % ' '.join(p)
    subprocess.call(['cabal', 'update'])
    subprocess.call(cmd.split())


def install_dependencies(dependencies):
    """
    Install dependencies taking into account the current linux distribution.

    Arguments
    ---------
    dependencies : dict
        Keys are package manager names, and values are lists of packages. Only
        the managers relevant to the current linux distribution will be used.
    """
    if platform.linux_distribution()[0] == 'arch':
        pacman(dependencies['pacman'])
        aura(dependencies['aura'])
    else:
        apt(dependencies['apt'])
    pip(dependencies['pip'])
