def get_platform():
    """
    Get platform name in lower case (e.g. 'arch' or 'ubuntu').
    """
    from platform import linux_distribution
    return linux_distribution()[0].lower()


def only(platforms):
    """
    Decorator that enables function only on given platform(s).

    Example
    -------

    @only('arch')
    def foobar():
        print('Doing something, but only on Arch.')
    """
    platforms = platforms if isinstance(platforms, list) else [platforms]
    if get_platform() in platforms:
        def decorator(function):
            return function
        return decorator
    else:
        def nothing():
            pass
        return nothing
