def get_platform():
    """
    Get platform name in lower case (e.g. 'arch' or 'ubuntu').
    """
    import csv

    RELEASE_DATA = dict()
    with open("/etc/os-release") as f:
        reader = csv.reader(f, delimiter="=")
        for row in reader:
            if row:
                RELEASE_DATA[row[0]] = row[1]
    return RELEASE_DATA["ID"]


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
