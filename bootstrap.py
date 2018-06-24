#!/usr/bin/env python3
# encoding: utf-8

import os
import sys
import subprocess
from os.path import expanduser, join
from shutil import which

if not which("git"):
    sys.exit("Please install git before running bootstrap script")

dest = join(expanduser("~"), ".mcf")

print("[*] Bootstrap MCF")
print("")
print("    Installation path: {}".format(dest))
print("")

print("[*] Obtain latest MCF sources")
try:
    if os.path.isdir(dest):
        print("    .mcf folder already exists, pulling the latest version")
        os.chdir(dest)
        cmd = "git pull"
        subprocess.check_call(cmd.split())
        cmd = "git submodule sync"
        subprocess.check_call(cmd.split())
    else:
        print("    cloning the repository")
        cmd = "git clone --recursive https://github.com/taketwo/mcf {}".format(dest)
        subprocess.check_call(cmd.split())
except subprocess.CalledProcessError:
    os.exit("Failed to obtain MCF sources, aborting...")
print("")

print("[*] Import install script")
library = join(dest, "scripts", "library")
os.environ["PYTHONPATH"] = library
sys.path.append(library)
print("")

print("[*] Install MCF")
print("")

__import__("package_manager").install("mcf", verbose=True)
