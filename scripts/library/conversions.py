def underscore_to_camelcase(value, start_with_lowercase=False):
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
