import os
from os.path import join, isdir
from collections import defaultdict
import subprocess

import stow
import mcf
from install import get_platform


PACKAGES = mcf.path("misc", "packages")
PLATFORM = get_platform()


class Install(object):
    def __init__(self, packages, args=""):
        cmd = self.CMD + " " + " ".join(packages) + " " + args
        subprocess.check_call(cmd.split())


class AptGet(Install):
    CMD = "sudo apt-get install --force-yes -y"


class Yaourt(Install):
    CMD = "yaourt --noconfirm --needed -Sa"


class Nix(Install):
    CMD = "nix-env -i"


class Pip(Install):
    CMD = "sudo -H pip install --upgrade"

    def __init__(self, packages, args=""):
        super().__init__(packages, args)
        stow.adopt_as("pip")


class Pipsi(Install):
    CMD = "pipsi install"

    def __init__(self, packages, args=""):
        for package in packages:
            # We support package specs with multiple entries separated by spaces.
            # First entry is the main app that is to be installed with Pipsi.
            # The remaining entries are additional packages that are to be installed
            # into the virtual environment created by Pipsi.
            # Example: "pygments pygments-style-solarized"
            entries = package.split(" ")
            if len(entries) > 1:
                package = entries[0]
            self.install(package, args)
            if len(entries) > 1:
                activate = "source ~/.local/venvs/{}/bin/activate".format(package)
                for entry in entries[1:]:
                    install = "pip install {}".format(entry)
                    cmd = "bash -c '{} && {}'".format(activate, install)
                    os.system(cmd)

    def install(self, package, args):
        """
        Run Pipsi install, ignoring "already installed" error.
        """
        cmd = self.CMD + " " + package + " " + args
        try:
            subprocess.check_output(cmd.split())
        except subprocess.CalledProcessError as e:
            import re
            if not re.findall(r'already installed', str(e.output)):
                raise e


class Cabal(Install):
    CMD = "sudo cabal install --global --force-reinstalls"

    def __init__(self, packages, args=""):
        subprocess.check_call(["sudo", "cabal", "update"])
        for package in packages:
            cmd = self.CMD + " " + package + " " + args
            subprocess.check_call(cmd.split())
        stow.adopt_as("cabal")


INSTALL = {"nix": Nix, "pip": Pip, "pipsi": Pipsi, "cabal": Cabal}
if PLATFORM == "ubuntu":
    INSTALL["ubuntu"] = AptGet
if PLATFORM == "arch":
    INSTALL["arch"] = Yaourt


class PackageManager(object):
    def __init__(self):
        pass

    def resolve(self, package_name):
        """
        Resolve package name into a list of commands to install/configure the package
        and its dependencies.
        """
        directory = join(PACKAGES, package_name)
        if not isdir(directory):
            return [(PLATFORM, package_name)]
        else:
            commands = list()
            deps = join(directory, "DEPENDENCIES")
            if os.path.isfile(deps):
                for line in open(deps, "r"):
                    package = self._remove_comments(line).strip()
                    if len(package.split(": ")) == 2:
                        commands.append(tuple(package.split(": ")))
                    elif package:
                        if package != package_name:
                            commands += self.resolve(package)
                        else:
                            commands.append((PLATFORM, package))
            nix_expression = join(directory, "default.nix")
            if os.path.isfile(nix_expression):
                commands.append(("nix", "-f {}".format(directory)))
            install_script = join(directory, "install")
            if os.path.isfile(install_script):
                commands.append(("script", install_script))
            setup_script = join(directory, "setup")
            if os.path.isfile(setup_script):
                commands.append(("setup", setup_script))
            return commands

    def _remove_comments(self, line):
        p = line.find("#")
        if p == -1:
            return line
        return line[:p]

    def _install_with_package_manager(self, manager, package, args=""):
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

        if manager not in INSTALL.keys():
            raise Exception("Unsupported manager")
        if isinstance(package, list):
            p = package
        elif isinstance(package, set):
            p = list(package)
        else:
            p = [package]
        INSTALL[manager](p, args)

    def install(self, package_name, verbose=False, force_reinstall=False):
        commands = self.resolve(package_name)
        merged = self._merge(commands)
        if verbose:
            self.describe_package(package_name, merged)
        try:
            for pm in INSTALL.keys():
                if pm in merged:
                    print("[*] Install {} packages\n".format(pm.capitalize()))
                    self._install_with_package_manager(pm, merged[pm])
                    print("")
            if "script" in merged:
                print("[*] Install scripts\n")
                for s in sorted(merged["script"]):
                    print("> {}".format(s))
                    cmd = [s]
                    if force_reinstall:
                        cmd.append("--reinstall")
                    subprocess.check_call(cmd, env=os.environ)
                    print("")
            if "setup" in merged:
                print("[*] Setup scripts\n")
                for s in merged["setup"]:
                    print("> {}".format(s))
                    subprocess.check_call([s], env=os.environ)
                    print("")
        except Exception as e:
            print('Installation of "{}" failed'.format(package_name))
            print("Error: {}".format(e))
            if hasattr(e, "output"):
                print("       {}".format(e.output))

    def describe_package(self, package_name, merged):
        print('Package "{}" resolved into:\n'.format(package_name))
        for pm in INSTALL.keys():
            if pm in merged:
                print(" - {} packages\n".format(pm.capitalize()))
                print("  " + ", ".join(merged[pm]))
                print("")
        if "script" in merged:
            print(" - Custom install scripts\n")
            for s in merged["script"]:
                print("  {}".format(s))
            print("")
        if "setup" in merged:
            print(" - Custom setup scripts\n")
            for s in merged["setup"]:
                print("  {}".format(s))
            print("")

    def _merge(self, commands):
        merged = defaultdict(list)
        for c in commands:
            if c[1] not in merged[c[0]]:
                merged[c[0]].append(c[1])
        return dict(merged)


def install(package_name, verbose=False, force_reinstall=False):
    pm = PackageManager()
    pm.install(package_name, verbose, force_reinstall)
