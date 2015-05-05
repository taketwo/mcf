def get_platform():
    from platform import linux_distribution
    return linux_distribution()[0].lower()


def only(platforms):
    platforms = platforms if isinstance(platforms, list) else [platforms]
    if get_platform() in platforms:
        def decorator(function):
            return function
        return decorator
    else:
        def nothing():
            pass
        return nothing
