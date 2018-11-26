def get_platform():
    """
    Get platform name in lower case (e.g. 'arch' or 'ubuntu').
    """
    from platform import linux_distribution

    p = linux_distribution()[0].lower()
    if p == "debian":
        print("WARNING: platform was determined to be Debian, not Ubuntu")
        return "ubuntu"
    return p


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

    else:

        def nothing():
            pass

        def decorator(function):
            return nothing

    return decorator
