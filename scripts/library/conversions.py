import re

# Regular expressions for camelcase_to_snakecase converter
_underscorer1 = re.compile(r"(.)([A-Z][a-z]+)")
_underscorer2 = re.compile("([a-z0-9])([A-Z])")


def _next(iterator):
    """
    Next item (compatibility with Python 2 and Python 3).
    """
    try:
        return iterator.next()
    except AttributeError:
        return next(iterator)


def to_camelcase(value):
    """
    Convert a string to camelCase.

    >>> to_camelcase('PascalCase')
    'pascalCase'
    >>> to_camelcase('camelCase')
    'camelCase'
    >>> to_camelcase('snake_case')
    'snakeCase'
    >>> to_camelcase('_snake_case')
    'snakeCase'
    >>> to_camelcase('another_snake_case_')
    'anotherSnakeCase'
    >>> to_camelcase('kebab-case')
    'kebabCase'
    """
    if is_pascalcase(value):
        return value[0].lower() + value[1:]
    if is_camelcase(value):
        return value
    if is_snakecase(value):
        separator = "_"
    elif is_kebabcase(value):
        separator = "-"
    else:
        raise Exception("Unknown case")

    def camelcase():
        yield type(value).lower
        while True:
            yield type(value).capitalize

    c = camelcase()
    return "".join(_next(c)(x) if x else "" for x in value.split(separator))


def to_pascalcase(value):
    """
    Convert a string to PascalCase.

    >>> to_pascalcase('PascalCase')
    'PascalCase'
    >>> to_pascalcase('camelCase')
    'CamelCase'
    >>> to_pascalcase('snake_case')
    'SnakeCase'
    >>> to_pascalcase('_snake_case')
    'SnakeCase'
    >>> to_pascalcase('another_snake_case_')
    'AnotherSnakeCase'
    >>> to_pascalcase('kebab-case')
    'KebabCase'
    """
    if is_pascalcase(value):
        return value
    if is_camelcase(value):
        return value[0].capitalize() + value[1:]
    if is_snakecase(value):
        separator = "_"
    elif is_kebabcase(value):
        separator = "-"
    else:
        raise Exception("Unknown case")

    def pascalcase():
        while True:
            yield type(value).capitalize

    c = pascalcase()
    return "".join(_next(c)(x) if x else "" for x in value.split(separator))


def camelcase_to_snakecase(value):
    """
    Convert a string from camelCase to snake_case.

    >>> camelcase_to_snakecase('camelCase')
    'camel_case'
    >>> camelcase_to_snakecase('anotherLongCamelCase')
    'another_long_camel_case'
    >>> camelcase_to_snakecase('word')
    'word'
    """
    subbed = _underscorer1.sub(r"\1_\2", value)
    return _underscorer2.sub(r"\1_\2", subbed).lower()


def is_camelcase(s):
    """
    Test if a string is in camelCase.

    >>> is_camelcase('camelCase')
    True
    >>> is_camelcase('anotherCamelCase')
    True
    >>> is_camelcase('almostCamel_Case')
    False
    >>> is_camelcase('PascalCase')
    False
    >>> is_camelcase('word')
    True
    >>> is_camelcase('snake_case')
    False
    >>> is_camelcase('kebab-case')
    False
    """
    return (
        (s != s.lower() or s != s.upper())
        and s[0].islower()
        and "_" not in s
        and "-" not in s
    )


def is_pascalcase(s):
    """
    Test if a string is in PascalCase.

    >>> is_pascalcase('PascalCase')
    True
    >>> is_pascalcase('AnotherPascalCase')
    True
    >>> is_pascalcase('camelCase')
    False
    >>> is_pascalcase('AlmostPascal-Case')
    False
    >>> is_pascalcase('Word')
    True
    >>> is_pascalcase('word')
    False
    >>> is_pascalcase('snake_case')
    False
    >>> is_pascalcase('kebab-case')
    False
    """
    return (
        (s != s.lower() or s != s.upper())
        and s[0].isupper()
        and "_" not in s
        and "-" not in s
    )


def is_snakecase(s):
    """
    Test if a string is in snake_case.

    >>> is_snakecase('camelCase')
    False
    >>> is_snakecase('PascalCase')
    False
    >>> is_snakecase('word')
    True
    >>> is_snakecase('snake_case')
    True
    >>> is_snakecase('another_snake_case')
    True
    >>> is_snakecase('_snake_case')
    True
    >>> is_snakecase('snake_case_')
    True
    >>> is_snakecase('kebab-case')
    False
    >>> is_snakecase('strange_mixed-case')
    False
    """
    return s == s.lower() and "-" not in s


def is_kebabcase(s):
    """
    Test if a string is in kebab-case.

    >>> is_kebabcase('camelCase')
    False
    >>> is_kebabcase('PascalCase')
    False
    >>> is_kebabcase('word')
    True
    >>> is_kebabcase('snake_case')
    False
    >>> is_kebabcase('kebab-case')
    True
    >>> is_kebabcase('another-kebab-case')
    True
    >>> is_kebabcase('strange_mixed-case')
    False
    """
    return s == s.lower() and "_" not in s


if __name__ == "__main__":
    import doctest

    doctest.testmod()
