import os
import shutil
import subprocess


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

    If dest already exists and points to src, it's a no-op. Otherwise, removes
    whatever existed in the destination before. Creates directories as necessary.
    """
    if os.path.lexists(dest):
        if os.path.islink(dest) and os.readlink(dest) == src:
            if description:
                print("[+]", description)
                print("   ", dest, "->", src, "(already linked)")
            elif verbose:
                print("[+]", dest)
                print("   ", "->", src, "(already linked)")
            return
        remove(dest)
    path, fl = os.path.split(os.path.realpath(dest))
    if not os.path.isdir(path):
        os.makedirs(path)
    try:
        os.symlink(src, dest)
    except PermissionError:
        subprocess.check_call("sudo ln -s {} {}".format(src, dest).split(" "))
    if description:
        print("[+]", description)
        print("   ", dest, "->", src)
    elif verbose:
        print("[+]", dest)
        print("   ", "->", src)


def get_extension(filename):
    """
    Get the extension (with dot) of a given filename.

    >>> get_extension('path/to/file.ext')
    '.ext'
    >>> get_extension('name.h')
    '.h'
    >>> get_extension('foo')
    ''
    """
    fn, e = os.path.splitext(filename)
    return e


def get_stem(filename):
    """
    Get the stem of a given filename.

    >>> get_stem('path/to/file.ext')
    'file'
    >>> get_stem('name.h')
    'name'
    >>> get_stem('foo')
    'foo'
    """
    p, fn = os.path.split(filename)
    fn, e = os.path.splitext(fn)
    return fn


def replace_extension(filename, new_ext):
    """
    Replace the extension in a given filename.

    >>> replace_extension('path/to/file.ext', 'cpp')
    'path/to/file.cpp'
    >>> replace_extension('name.h', '.hpp')
    'name.hpp'
    >>> replace_extension('foo', 'bar')
    'foo.bar'
    """
    fn, e = os.path.splitext(filename)
    e = ("" if new_ext.startswith(".") else ".") + new_ext
    return fn + e


def replace_stem(filename, new_stem):
    """
    Replace the stem in a given filename.

    >>> replace_stem('path/to/file.ext', 'replacement')
    'path/to/replacement.ext'
    >>> replace_stem('name.cpp', 'new')
    'new.cpp'
    >>> replace_stem('foo', 'bar')
    'bar'
    """
    p, fn = os.path.split(filename)
    fn, e = os.path.splitext(fn)
    return os.path.join(p, new_stem + e)


def is_exe(fpath):
    """
    Check if a given file exists and is executable.

    >>> is_exe('/bin/ls')
    True
    >>> is_exe('/total/nonsense')
    False
    """
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)


def which(program):
    """
    Locate a given program (http://stackoverflow.com/a/377028/1525865)

    >>> which('ls')
    '/bin/ls'
    >>> which('total-nonsense')
    None
    """
    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return None


if __name__ == "__main__":
    import doctest

    doctest.testmod()
