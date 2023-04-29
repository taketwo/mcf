import os

HOME = os.path.expanduser("~")
MCF = os.path.join(HOME, ".mcf")
XMONAD = os.path.join(HOME, ".xmonad")


def path(*args):
    """
    Generate a path relative to MCF directory.
    """
    return os.path.join(MCF, *args)
