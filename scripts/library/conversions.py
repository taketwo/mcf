import re

# Regular expressions for camelcase_to_snakecase converter
_underscorer1 = re.compile(r'(.)([A-Z][a-z]+)')
_underscorer2 = re.compile('([a-z0-9])([A-Z])')


def snakecase_to_camelcase(value, start_with_lowercase=False):
    if start_with_lowercase:
        def camelcase():
            yield type(value).lower
            while True:
                yield type(value).capitalize
    else:
        def camelcase():
            while True:
                yield type(value).capitalize
    c = camelcase()
    return "".join(c.next()(x) if x else '_' for x in value.split("_"))


def camelcase_to_snakecase(value):
    subbed = _underscorer1.sub(r'\1_\2', value)
    return _underscorer2.sub(r'\1_\2', subbed).lower()


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
    return ((s != s.lower() or s != s.upper()) and
            s[0].islower() and
            '_' not in s and
            '-' not in s)


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
    return (s == s.lower() and '-' not in s)


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
    return (s == s.lower() and '_' not in s)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
