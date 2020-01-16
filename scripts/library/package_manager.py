import os
from os.path import join, isdir
from collections import defaultdict
import subprocess
from pathlib import Path

import stow
import mcf
from install import get_platform
from filesystem import link


PACKAGES = mcf.path("misc", "packages")
PLATFORM = get_platform()


class Install(object):
    def __init__(self, packages, args=""):
        cmd = self.CMD + " " + " ".join(packages) + " " + args
        subprocess.check_call(cmd.split())


class AptGet(Install):
    CMD = "sudo apt-get install --yes"


class Yaourt(Install):
    CMD = "yaourt --noconfirm --needed -Sa"


class Nix(Install):
    CMD = "nix-env -i"

    def __init__(self, packages, args=""):
        # Make sure that LD variables are not set, otherwise Nix insulation from the
        # rest of the system may be compromised
        env = dict(os.environ, LD_LIBRARY_PATH="", LD_PRELOAD="")
        for p in packages:
            cmd = self.CMD + " " + p + " " + args
            subprocess.check_call(cmd.split(), env=env)


class Pip(Install):
    CMD = "sudo -H pip install --upgrade"

    def __init__(self, packages, args=""):
        super().__init__(packages, args)
        stow.adopt_as("pip")


class Pipx(Install):
    CMD = "pipx install"

    def __init__(self, packages, args=""):
        for package in packages:
            # We support package specs with multiple entries separated by spaces.
            # First entry is the main app that is to be installed with Pipx.
            # The remaining entries are additional packages that are to be injected
            # into the virtual environment created by Pipx.
            # Example: "pygments pygments-style-solarized"
            entries = package.split(" ")
            package = self.install(entries[0], args)
            if len(entries) > 1:
                for entry in entries[1:]:
                    subprocess.check_output(["pipx", "inject", package, entry])

    def install(self, package, args):
        """
        Run Pipx install.
        Returns package name (without [] spec).
        """
        import re
        cmd = self.CMD + " " + package + " " + args
        subprocess.check_output(cmd.split())
        m = re.match(r"(.*)\[.*\]", package)
        return package if m is None else m.groups()[0]


class Cabal(Install):
    CMD = "sudo cabal install --global --force-reinstalls"

    def __init__(self, packages, args=""):
        subprocess.check_call(["sudo", "cabal", "update"])
        for package in packages:
            cmd = self.CMD + " " + package + " " + args
            subprocess.check_call(cmd.split())
        stow.adopt_as("cabal")


# A dictionary mapping manager names to classes. The insertion order matters, we
# want to install with the main system manager first and with secondary managers
# (e.g. Cabal) last.
INSTALL = dict()
if PLATFORM == "ubuntu":
    INSTALL["ubuntu"] = AptGet
if PLATFORM == "arch":
    INSTALL["arch"] = Yaourt
INSTALL.update({"nix": Nix, "pip": Pip, "pipx": Pipx, "cabal": Cabal})


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
            if len(package_name.split(": ")) == 2:
                return [tuple(package_name.split(": "))]
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
            pre_install_script = join(directory, "pre-install")
            if os.path.isfile(pre_install_script):
                commands.append(("pre-install", pre_install_script))
            install_script = join(directory, "install")
            if os.path.isfile(install_script):
                commands.append(("install", install_script))
            post_install_script = join(directory, "post-install")
            if os.path.isfile(post_install_script):
                commands.append(("post-install", post_install_script))
            commands.extend(self._parse_symlinks(package_name))
            return commands

    def _parse_symlinks(self, package_name):
        """
        Parse SYMLINKS file (if exists) for a given package.
        Return a list of commands.
        """
        directory = join(PACKAGES, package_name)
        symlinks = join(directory, "SYMLINKS")
        commands = list()
        if os.path.isfile(symlinks):
            for line in open(symlinks, "r"):
                line = self._remove_comments(line).strip()
                tokens = line.split(":")
                if len(tokens) not in [2, 3]:
                    raise Exception('Invalid symlink spec "{}"'.format(line))
                src = self._resolve_path(tokens[0], directory)
                tgt = self._resolve_path(tokens[1], mcf.HOME)
                desc = tokens[2] if len(tokens) > 2 else ""
                commands.append(("symlink", (src, tgt, desc)))
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

    def install(self, package_name, verbose=False, force_reinstall=False, update=False):
        """
        Install package with a given name.

        Installation order:
          1. Pre-install scripts
          2. Package manager dependencies
          3. Install scripts
          4. Post-install scripts
        """
        commands = self.resolve(package_name)
        merged = self._merge(commands)
        if verbose:
            self.describe_package(package_name, merged)
        try:
            if "pre-install" in merged:
                print("[*] Pre-install scripts\n")
                for s in merged["pre-install"]:
                    print("> {}".format(s))
                    subprocess.check_call([s], env=os.environ)
                    print("")
            for pm in INSTALL.keys():
                if pm in merged:
                    print("[*] Install {} packages\n".format(pm.capitalize()))
                    self._install_with_package_manager(pm, merged[pm])
                    print("")
            if "install" in merged:
                print("[*] Install scripts\n")
                for s in sorted(merged["install"]):
                    print("> {}".format(s))
                    cmd = [s]
                    if force_reinstall:
                        cmd.append("--reinstall")
                    if update:
                        cmd.append("--update")
                    subprocess.check_call(cmd, env=os.environ)
                    print("")
            if "symlink" in merged:
                print("[*] Create symlinks\n")
                for s in merged["symlink"]:
                    link(*s)
            if "post-install" in merged:
                print("[*] Post-install scripts\n")
                for s in merged["post-install"]:
                    print("> {}".format(s))
                    subprocess.check_call([s], env=os.environ)
                    print("")
            return True
        except Exception as e:
            print('Installation of "{}" failed'.format(package_name))
            print("Error: {}".format(e))
            if hasattr(e, "output"):
                print("       {}".format(e.output))
            return False

    def describe_package(self, package_name, merged):
        print('Package "{}" resolved into:\n'.format(package_name))
        if "pre-install" in merged:
            print(" - Pre-install scripts\n")
            for s in merged["pre-install"]:
                print("  {}".format(s))
            print("")
        for pm in INSTALL.keys():
            if pm in merged:
                print(" - {} packages\n".format(pm.capitalize()))
                print("  " + ", ".join(merged[pm]))
                print("")
        if "install" in merged:
            print(" - Custom install scripts\n")
            for s in merged["install"]:
                print("  {}".format(s))
            print("")
        if "symlink" in merged:
            print(" - Symlinks\n")
            for s in merged["symlink"]:
                print("  {}".format(s[2]))
            print("")
        if "post-install" in merged:
            print(" - Post-install scripts\n")
            for s in merged["post-install"]:
                print("  {}".format(s))
            print("")

    def _merge(self, commands):
        merged = defaultdict(list)
        for c in commands:
            if c[1] not in merged[c[0]]:
                merged[c[0]].append(c[1])
        return dict(merged)

    def _resolve_path(self, path, base):
        p = Path(os.path.expandvars(path))
        if p.is_absolute():
            return str(p)
        else:
            return str(Path(base) / p)


def install(package_name, verbose=False, force_reinstall=False, update=False):
    pm = PackageManager()
    return pm.install(package_name, verbose, force_reinstall, update)
