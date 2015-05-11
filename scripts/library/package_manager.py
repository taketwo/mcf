import os
import sys
from os.path import join, isdir
from platform import linux_distribution
from collections import defaultdict
import subprocess

import stow


PACKAGES = '/home/sergey/.mcf/misc/packages'
PLATFORM = linux_distribution()[0].lower()


class PackageManager(object):

    def __init__(self):
        pass

    def resolve(self, package_name):
        """
        Resolve package name into a list of commands to install/configure the
        package and its dependencies.
        """
        directory = join(PACKAGES, package_name)
        if not isdir(directory):
            return [(PLATFORM, package_name)]
        else:
            commands = list()
            deps = join(directory, 'DEPENDENCIES')
            if os.path.isfile(deps):
                for line in open(deps, 'r'):
                    package = self._remove_comments(line).strip()
                    if len(package.split(': ')) == 2:
                        commands.append(tuple(package.split(': ')))
                    elif package:
                        if package != package_name:
                            commands += self.resolve(package)
                        else:
                            commands.append((PLATFORM, package))
            install_script = join(directory, 'install')
            if os.path.isfile(install_script):
                commands.append(('script', install_script))
            setup_script = join(directory, 'setup')
            if os.path.isfile(setup_script):
                commands.append(('setup', setup_script))
            return commands

    def _remove_comments(self, line):
        p = line.find("#")
        if p == -1:
            return line
        return line[:p]

    def _install_with_package_manager(self, manager, package, args=''):
        """
        Install package(s) using given package manager.

        Arguments
        ---------
        manager: str
            Name of a package manager to use (apt, pacman, yaourt, pip, cabal).
        package: str | list
            Package name or list of package names.
        args:
            Additional options to pass to the package manager.
        """
        CMD = {'ubuntu': 'sudo apt-get install --force-yes -y',
               'arch': 'yaourt --noconfirm --needed -Sa',
               'pip': 'sudo pip install --upgrade',
               'cabal': 'sudo cabal install --global --force-reinstalls'}
        if not manager in CMD.keys():
            raise Exception('Unsupported manager')
        if isinstance(package, list):
            p = package
        elif isinstance(package, set):
            p = list(package)
        else:
            p = [package]
        if manager == 'cabal':
            subprocess.check_call(['cabal', 'update'])
        cmd = CMD[manager] + ' ' + ' '.join(p) + ' ' + args
        subprocess.check_call(cmd.split())
        if manager == 'pip':
            stow.adopt_as('pip')

    def install(self, package_name, verbose=False, force_reinstall=False):
        commands = self.resolve(package_name)
        merged = self._merge(commands)
        if verbose:
            self.describe_package(package_name, merged)
        try:
            for pm in [PLATFORM, 'pip', 'cabal']:
                if pm in merged:
                    print('[*] Install {} packages\n'.format(pm.capitalize()))
                    self._install_with_package_manager(pm, merged[pm])
                    print('')
            if 'script' in merged:
                print('[*] Install scripts\n')
                for s in sorted(merged['script']):
                    print('>', s)
                    cmd = [s]
                    if force_reinstall:
                        cmd.append('--reinstall')
                    subprocess.check_call(cmd, env=os.environ)
                    print('')
            if 'setup' in merged:
                print('[*] Setup scripts\n')
                for s in merged['setup']:
                    print('>', s)
                    subprocess.check_call([s], env=os.environ)
                    print('')
        except Exception as e:
            print('Installation of \"{}\" failed'.format(package_name))
            print('Error:', e)

    def describe_package(self, package_name, merged):
        print('Package \"{}\" resolved into:\n'.format(package_name))
        for pm in [PLATFORM, 'pip', 'cabal']:
            if pm in merged:
                print(' - {} packages\n'.format(pm.capitalize()))
                print('  ', ', '.join(merged[pm]))
                print('')
        if 'script' in merged:
            print(' - Custom install scripts\n')
            for s in merged['script']:
                print('  ', s)
            print('')
        if 'setup' in merged:
            print(' - Custom setup scripts\n')
            for s in merged['setup']:
                print('  ', s)
            print('')

    def _merge(self, commands):
        merged = defaultdict(list)
        for c in commands:
            if c[1] not in merged[c[0]]:
                merged[c[0]].append(c[1])
        return dict(merged)


def install(package_name, verbose=False, force_reinstall=False):
    pm = PackageManager()
    pm.install(package_name, verbose, force_reinstall)
