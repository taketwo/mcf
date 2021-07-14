#!/usr/bin/env python

from pudb.var_view import type_stringifier


TYPE_NAME_MAP = {
    "float32": "f32",
    "float64": "f64",
    "int32": "i32",
    "int64": "i64",
    "uint32": "u32",
    "uint64": "u64",
}


def _dtype(dtype):
    dtype = str(dtype)
    for k, v in TYPE_NAME_MAP.items():
        if dtype.endswith(k):
            return v
    return dtype


def pudb_stringifier(obj):
    if type(obj).__name__ == "Image" and obj.__module__ == "PIL.Image":
        return f'PIL.Image {obj.mode} ({", ".join(map(str, obj.size))})'
    if type(obj).__name__ == "Tensor" and obj.__module__ == "torch":
        device = "⁙" if obj.device.type == "cuda" else ""
        return f'Tensor {device} {_dtype(obj.dtype)} ({", ".join(map(str, obj.shape))})'
    if type(obj).__name__ == "ndarray":
        return f'ndarray {_dtype(obj.dtype)} ({", ".join(map(str, obj.shape))})'
    if type(obj).__name__ in TYPE_NAME_MAP.keys():
        return f"{_dtype(type(obj).__name__)} {obj}"
    if type(obj).__name__ == "function":
        return "function"
    if type(obj).__name__ == "module":
        return "module"
    if type(obj).__name__ == "PosixPath":
        return f"PosixPath {obj}"
    if type(obj) in [set, frozenset, list, tuple, dict]:
        return f"{type(obj).__name__} ({len(obj)})"
    if type(obj).__name__ in ("bool", "float", "int"):
        return f"{type(obj).__name__} {obj}"
    return type_stringifier(obj)
