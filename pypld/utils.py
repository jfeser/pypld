from typing import TypeVar

K = TypeVar("K")
V = TypeVar("V")


def bind(ctx: dict[K, V], var: K, type_: V) -> dict[K, V]:
    """Bind a variable to a type in a context."""
    return {**ctx, var: type_}


def fresh(vs):
    i = 0
    v = "x0"
    while v in vs:
        i += 1
        v = f"x{i}"
    return v


def texescape(s):
    return (
        s.replace("_", "\\_")
        .replace("&", "\\&")
        .replace("%", "\\%")
        .replace("$", "\\$")
        .replace("#", "\\#")
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("~", "")
        .replace("^", "")
        .replace("\\", "")
    )
