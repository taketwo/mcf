#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys
from pathlib import Path
from shutil import which

if not which("git"):
    sys.exit("Please install git before running bootstrap script")

dest = Path.home() / ".mcf"

parser = argparse.ArgumentParser(
    description="""
Bootstrap MCF.
""",
    formatter_class=argparse.RawDescriptionHelpFormatter,
)
parser.add_argument("--core", action="store_true", help="Install only MCF core")
parser.add_argument(
    "--no-git",
    action="store_true",
    help="Assume MCF repository already cloned, also do not modify remote in the end",
)
args = parser.parse_args()

print("[*] Bootstrap MCF")
print()
print(f"    Installation path: {dest}")
print()

print("[*] Obtain MCF sources")
print()
if args.no_git:
    if dest.is_dir():
        print("    .mcf folder exists")
    else:
        sys.exit(
            "MCF sources are not available and --no-git option was passed, aborting...",
        )
else:
    try:
        if dest.is_dir():
            print("    .mcf folder already exists, pulling the latest version")
            os.chdir(dest)
            cmd = "git pull"
            subprocess.check_call(cmd.split())
            cmd = "git submodule sync"
            subprocess.check_call(cmd.split())
        else:
            print("    cloning the repository")
            cmd = f"git clone --recursive https://github.com/taketwo/mcf {dest}"
            subprocess.check_call(cmd.split())
    except subprocess.CalledProcessError:
        sys.exit("Failed to obtain MCF sources, aborting...")
print()

print("[*] Import MPM")
library = str(dest / "scripts" / "library")
os.environ["PYTHONPATH"] = library
sys.path.append(library)
pm = __import__("package_manager")
print()

if "MCF" not in os.environ:
    print("[*] Install core package managers")
    print()

    if not pm.install("nix", verbose=True):
        sys.exit("First part of bootstrapping procedure failed!")
    else:
        print("First part of bootstrapping procedure is completed.")
        print(f"Run the following command: source {dest / '.profile'}")
        print("Now re-run this script to complete bootstrapping.")
else:
    print("[*] Install extra package managers")
    print()
    if (
        not pm.install("fnm", verbose=True)
        or not pm.install("uv", verbose=True)
        or not pm.install("nix: eget", verbose=True)
        or not pm.install("nix: cargo", verbose=True)
        or not pm.install("bob", verbose=True)
    ):
        sys.exit("Second part of bootstrapping procedure failed!")

    print("[*] Install MCF")
    print()

    mcf_package = "mcf-core" if args.core else "mcf"

    if not pm.install(mcf_package, verbose=True):
        sys.exit("MCF installation failed!")
    else:
        if not args.no_git:
            print("[*] Change MCF remote to use Git")
            print()
            cmd = "git remote set-url origin git@github.com:taketwo/mcf.git"
            subprocess.check_call(cmd.split())

        print("Second part of bootstrapping procedure is completed.")
        print("If the terminal font is screwed, relaunching will solve the issue.")
        print("Reboot the computer to finish installation.")
