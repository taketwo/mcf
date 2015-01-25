import os
import os.path
import shutil


def remove(dest):
    """
    Universal remove (handles symlinks, files, and directories).
    """
    try:
        os.unlink(dest)
        return True
    except OSError:
        pass
    try:
        os.remove(dest)
        return True
    except OSError:
        pass
    try:
        shutil.rmtree(dest)
        return True
    except OSError:
        pass


def link(src, dest, description=None, verbose=False):
    """
    Create symlink pointing from dest to src.

    Removes whatever existed in the destination before. Creates directories as
    necessary.
    """
    if os.path.lexists(dest):
        remove(dest)
    path, fl = os.path.split(os.path.realpath(dest))
    if not os.path.isdir(path):
        os.makedirs(path)
    os.symlink(src, dest)
    if description:
        print('[+]', description)
        print('   ', dest, '->', src)
    elif verbose:
        print('[+]', dest)
        print('   ', '->', src)
