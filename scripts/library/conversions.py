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
    return ((s != s.lower() or s != s.upper()) and '_' not in s)


def is_snakecase(s):
    return (s == s.lower() and '_' in s)
