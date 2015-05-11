import os
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
    e = ('' if new_ext.startswith('.') else '.') + new_ext
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


if __name__ == '__main__':
    import doctest
    doctest.testmod()
