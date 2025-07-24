import os
import re
import subprocess
from collections import defaultdict
from os.path import isdir, join
from pathlib import Path

import mcf
from filesystem import link
from install import get_platform


PACKAGES = mcf.path("misc", "packages")
PLATFORM = get_platform()


class Install:
    def __init__(self, packages, args=None):
        args = args or []
        cmd = self.CMD + " " + " ".join(packages) + " " + " ".join(args)
        subprocess.check_call(cmd.split())


class AptGet(Install):
    CMD = "sudo apt-get install --yes"


class Nix(Install):
    def __init__(self, packages, args=None):
        args = args or []
        # Make sure that LD variables are not set, otherwise Nix insulation from the
        # rest of the system may be compromised
        env = dict(os.environ, LD_LIBRARY_PATH="", LD_PRELOAD="")
        for p in packages:
            cmd = "nix-env -i"
            if "." not in p:
                # We use --attr option, thus package names should be attribute paths.
                # If there is no dot in the name, we assume that this package comes
                # from Nixpkgs.
                p = "nixpkgs." + p
            if not p.startswith("-f "):
                # -f indicates that this is a custom derivation, thus -A is not needed
                cmd += "A"
            cmd += " " + p + " " + " ".join(args)
            subprocess.check_call(cmd.split(), env=env)


class Uvx(Install):
    CMD = "uv tool install --force"

    def __init__(self, packages, args=None):
        args = args or []
        for package in packages:
            # We support package specs with multiple entries separated by spaces.
            # First entry is the main app that is to be installed with uv tool.
            # The remaining entries are additional packages that are to be injected
            # into the same virtual environment.
            # Example: "pygments pygments-style-solarized"
            entries = package.split(" ")
            args += [e for e in entries if e.startswith("-")]
            main, *extras = [e for e in entries if not e.startswith("-")]
            if self.is_local_python_package(entries[0]):
                args.append("--editable")
            for extra in extras:
                args.append("--with")
                args.append(extra)
            package = self.install(main, args)

    def install(self, package, args):
        """
        Run uv tool install.
        Returns package name (without [] spec).
        """
        import re

        cmd = self.CMD + " " + package + " " + " ".join(args)
        subprocess.check_output(cmd.split())
        m = re.match(r"(.*)\[.*\]", package)
        return package if m is None else m.groups()[0]

    @classmethod
    def is_local_python_package(cls, package: str) -> bool:
        """Check if the given path contains a Python package.

        Parameters
        ----------
        package: str
            Path to the potential package directory.

        Returns
        -------
        bool
            True if the given path contains a Python package, False otherwise.

        """
        package_path = Path(package)
        return (package_path / "setup.py").is_file() or (
            package_path / "pyproject.toml"
        ).is_file()


class Cabal(Install):
    CMD = "cabal v2-install --overwrite-policy=always"

    def __init__(self, packages, args=None):
        subprocess.check_call(["cabal", "v2-update"])
        cmd = "LC_ALL=C {} --lib {}".format(self.CMD, " ".join(packages))
        subprocess.check_call(cmd, shell=True)
        cmd = "LC_ALL=C {} {}".format(self.CMD, " ".join(packages))
        subprocess.check_call(cmd, shell=True)


class Eget(Install):
    CMD = f"eget --to {mcf.HOME}/.local/bin --upgrade-only"

    def __init__(self, packages, args=None):
        args = args or []
        for package in packages:
            # We support optional additional arguments to be passed to Eget. Such
            # arguments, if provided, should be supplied in square brackets, e.g.
            # "eget[--foo bar]".

            regex = r"([^[\]]+)(?:\[(.+?)\])?"
            m = re.match(regex, package)
            if m is None:
                raise ValueError(f"Invalid package specification: {package}")
            package_name, package_args = m.groups()
            print(package_name)
            cmd = self.CMD + " " + package_name + " " + (package_args or "")
            subprocess.check_call(cmd.split())


class Cargo(Install):
    CMD = "cargo install"

    def __init__(self, packages, args=None):
        args = args or []
        for p in packages:
            cmd = self.CMD + " " + p + " " + " ".join(args)
            subprocess.check_call(cmd.split())


class Fnm(Install):
    NODE_VERSION = "default"
    CMD = f"fnm exec --using {NODE_VERSION} npm install --location=global"

    def __init__(self, packages, args=None):
        args = args or []
        for p in packages:
            cmd = self.CMD + " " + p + " " + " ".join(args)
            subprocess.check_call(cmd.split())


# A dictionary mapping manager names to classes. The insertion order matters, we
# want to install with the main system manager first and with secondary managers
# (e.g. Cabal) last.
INSTALL = {}
if PLATFORM == "ubuntu":
    INSTALL["ubuntu"] = AptGet
INSTALL.update(
    {"nix": Nix, "uvx": Uvx, "cabal": Cabal, "eget": Eget, "cargo": Cargo, "fnm": Fnm}
)


class PackageManager:
    def __init__(self) -> None:
        pass

    def resolve(self, package_name: str):
        """Resolve package name into a list of commands.

        The commands, when executed, will install/configure the package and its
        dependencies.
        """
        if isdir(join(PACKAGES, package_name)):
            directory = join(PACKAGES, package_name)
        elif isdir(package_name):
            directory = package_name
        else:
            if len(package_name.split(": ")) == 2:
                return [tuple(package_name.split(": "))]
            return [(PLATFORM, package_name)]
        commands = list()
        deps = join(directory, "DEPENDENCIES")
        if os.path.isfile(deps):
            with open(deps) as f:
                for line in f:
                    package = self._remove_comments(line).strip()
                    package = os.path.expandvars(package)
                    if len(package.split(": ")) == 2:
                        commands.append(tuple(package.split(": ")))
                    elif package:
                        if package != package_name:
                            commands += self.resolve(package)
                        else:
                            commands.append((PLATFORM, package))
        nix_expression = join(directory, "default.nix")
        if os.path.isfile(nix_expression):
            commands.append(("nix", "-f {}".format(nix_expression)))
        if Uvx.is_local_python_package(directory):
            commands.append(("uvx", directory))
        pre_install_script = join(directory, "pre-install")
        if os.path.isfile(pre_install_script):
            commands.append(("pre-install", pre_install_script))
        install_script = join(directory, "install")
        if os.path.isfile(install_script):
            commands.append(("install", install_script))
        post_install_script = join(directory, "post-install")
        if os.path.isfile(post_install_script):
            commands.append(("post-install", post_install_script))
        commands.extend(self._parse_symlinks(directory))
        return commands

    def _parse_symlinks(self, directory):
        """
        Parse SYMLINKS file (if exists) for a given package.
        Return a list of commands.
        """
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

    def _remove_comments(self, line: str) -> str:
        """Remove comments from a given line.

        Comments are denoted by a hash sign (#) and can appear anywhere in the line.
        This function removes all characters from the first hash sign to the end of
        the line. It also removes leading and trailing whitespace.
        """
        p = line.find("#")
        if p == -1:
            return line
        return line[:p].strip()

    def _install_with_package_manager(self, manager, package, args=None) -> None:
        """Install package(s) using given package manager.

        Arguments:
        ---------
        manager: str
            Name of a package manager to use (apt, uvx, cabal, eget).
        package: str | list
            Package name or list of package names.
        args: list:
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
        INSTALL[manager](p, args or [])

    def install(self, package_name, verbose=False, force_reinstall=False, update=False):
        """Install package with a given name.

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
                    print(f"> {s}")
                    subprocess.check_call([s], env=os.environ)
                    print("")
            for pm in INSTALL:
                if pm in merged:
                    print(f"[*] Install {pm.capitalize()} packages\n")
                    self._install_with_package_manager(pm, merged[pm])
                    print("")
            if "install" in merged:
                print("[*] Install scripts\n")
                for s in sorted(merged["install"]):
                    print(f"> {s}")
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
                    print(f"> {s}")
                    subprocess.check_call([s], env=os.environ)
                    print("")
            return True
        except Exception as e:
            print(f'Installation of "{package_name}" failed')
            print(f"Error: {e}")
            if hasattr(e, "output"):
                print(f"       {e.output}")
            return False

    def describe_package(self, package_name, merged):
        print(f'Package "{package_name}" resolved into:\n')
        if "pre-install" in merged:
            print(" - Pre-install scripts\n")
            for s in merged["pre-install"]:
                print(f"  {s}")
            print("")
        for pm in INSTALL:
            if pm in merged:
                print(f" - {pm.capitalize()} packages\n")
                print("  " + ", ".join(merged[pm]))
                print("")
        if "install" in merged:
            print(" - Custom install scripts\n")
            for s in merged["install"]:
                print(f"  {s}")
            print("")
        if "symlink" in merged:
            print(" - Symlinks\n")
            for s in merged["symlink"]:
                print(f"  {s[2]}")
            print("")
        if "post-install" in merged:
            print(" - Post-install scripts\n")
            for s in merged["post-install"]:
                print(f"  {s}")
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
            return str(Path(base).resolve() / p)


def install(
    package_name: str,
    *,
    verbose: bool = False,
    force_reinstall: bool = False,
    update: bool = False,
) -> bool:
    """Install a package.

    Package argument should be one of:
      * package name of a package that exists in mcf/misc/packages or in native platform
        package manager repository
      * package spec in the format "manager: package"
      * full path to package
    """
    pm = PackageManager()
    return pm.install(package_name, verbose, force_reinstall, update)
