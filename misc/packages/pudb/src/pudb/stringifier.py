#!/usr/bin/env python

from pudb.var_view import type_stringifier


def _dtype(dtype):
    m = {"float32": "f32", "float64": "f64", "int32": "i32", "int64": "i64"}
    dtype = str(dtype)
    for k, v in m.items():
        if dtype.endswith(k):
            return v
    return dtype


def pudb_stringifier(obj):
    if type(obj).__name__ == "Tensor" and obj.__module__ == "torch":
        device = "⁙" if obj.device.type == "cuda" else ""
        return f'Tensor ({", ".join(map(str, obj.shape))}) {device} {_dtype(obj.dtype)}'
    if type(obj).__name__ == "ndarray":
        return f'ndarray ({", ".join(map(str, obj.shape))}) {_dtype(obj.dtype)}'
    if type(obj).__name__ == "int64":
        return f'{obj}'
    if type(obj).__name__ == "function":
        return 'function'
    if type(obj) in [set, frozenset, list, tuple, dict]:
        return f"{type(obj).__name__} ({len(obj)})"
    return type_stringifier(obj)