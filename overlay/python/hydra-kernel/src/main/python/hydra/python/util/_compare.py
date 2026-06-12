"""Total-ordering comparator for arbitrary Python values.

Used by ``PersistentMap`` and ``PersistentSet`` to order keys/elements that may
include Hydra ``Term``/``Type``/``Maybe`` values for which Python's natural
``<`` is undefined. Tries native ``<`` first, then falls back to a structural
comparison so that all Hydra values get a deterministic ordering.

Mirrors ``_structural_compare`` in ``hydra/lib/maps.py``; consolidated here so
the persistent collections can use it without depending on ``lib/maps``.
"""

from __future__ import annotations

import enum
from collections.abc import Mapping
from functools import cmp_to_key
from typing import Any


def compare(x: Any, y: Any) -> int:
    """Return -1 if x < y, 0 if equal, +1 if x > y, using natural < where
    available, structural comparison otherwise."""
    # Identity short-circuit: any value compares equal to itself. Hot in
    # PersistentMap inserts where lookup-then-skip is common.
    if x is y:
        return 0
    # Fast path: native comparison.
    try:
        if x < y:
            return -1
        if x > y:
            return 1
        return 0
    except TypeError:
        pass

    tx, ty = type(x), type(y)
    if tx is not ty:
        return -1 if tx.__name__ < ty.__name__ else 1

    # Enum members: order by name. The __dict__ fallback below would recurse
    # forever via __objclass__ → _member_map_ → members.
    if isinstance(x, enum.Enum):
        return -1 if x.name < y.name else (1 if x.name > y.name else 0)

    # Dataclass-style objects: order by sorted attribute pairs.
    if hasattr(x, "__dict__"):
        xi = sorted(x.__dict__.items())
        yi = sorted(y.__dict__.items())
        for (kx, vx), (ky, vy) in zip(xi, yi):
            if kx != ky:
                return -1 if kx < ky else 1
            r = compare(vx, vy)
            if r != 0:
                return r
        return compare(len(xi), len(yi))

    # Tuples and lists.
    if isinstance(x, (tuple, list)):
        for xi_, yi_ in zip(x, y):
            r = compare(xi_, yi_)
            if r != 0:
                return r
        return compare(len(x), len(y))

    # Mappings.
    if isinstance(x, Mapping):
        xs = sorted(x.items(), key=cmp_to_key(lambda a, b: compare(a[0], b[0])))
        ys = sorted(y.items(), key=cmp_to_key(lambda a, b: compare(a[0], b[0])))
        for (kx, vx), (ky, vy) in zip(xs, ys):
            r = compare(kx, ky)
            if r != 0:
                return r
            r = compare(vx, vy)
            if r != 0:
                return r
        return compare(len(xs), len(ys))

    # Last resort: compare by repr.
    rx, ry = repr(x), repr(y)
    if rx < ry:
        return -1
    if rx > ry:
        return 1
    return 0
