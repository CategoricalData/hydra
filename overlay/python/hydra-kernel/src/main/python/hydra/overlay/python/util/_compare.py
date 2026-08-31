"""Structural total-ordering comparator for arbitrary Python values.

Implements docs/specification/ordering-and-equality.md: records compare
field-by-field in declaration order, unions by declared-variant order then
payload, wrappers by wrapped value, decimals by numeric value then scale,
with no print- or repr-based fallback. Used both by the
``hydra.lib.ordering.compare`` primitive (via ``hydra.overlay.python.lib.ordering``)
and by ``PersistentMap``/``PersistentSet`` to order keys/elements that may
include Hydra ``Term``/``Type`` values for which Python's natural ``<`` is
undefined.
"""

from __future__ import annotations

import enum
import math
from collections.abc import Mapping, Sequence, Set
from decimal import Decimal
from functools import cmp_to_key
from typing import Any

# Declared-variant order for each coder-generated union family, mirroring the
# DSL declaration order (packages/hydra-kernel/.../Sources/Kernel/Types/Core.hs)
# and the ordinal Java/Scala derive natively. Keyed by class name (not the
# class object) to avoid importing hydra.core at module load time.
_VARIANT_ORDER: dict[str, tuple[str, ...]] = {
    "Term": (
        "TermAnnotated", "TermApplication", "TermCases", "TermEither", "TermInject",
        "TermLambda", "TermLet", "TermList", "TermLiteral", "TermMap", "TermOptional",
        "TermPair", "TermProject", "TermRecord", "TermSet", "TermTypeApplication",
        "TermTypeLambda", "TermUnit", "TermUnwrap", "TermVariable", "TermWrap"),
    "Literal": (
        "LiteralBinary", "LiteralBoolean", "LiteralDecimal", "LiteralFloat",
        "LiteralInteger", "LiteralString"),
    "Type": (
        "TypeAnnotated", "TypeApplication", "TypeEffect", "TypeEither", "TypeForall",
        "TypeFunction", "TypeList", "TypeLiteral", "TypeMap", "TypeOptional", "TypePair",
        "TypeRecord", "TypeSet", "TypeUnion", "TypeUnit", "TypeVariable", "TypeVoid",
        "TypeWrap"),
    "LiteralType": (
        "LiteralTypeBinary", "LiteralTypeBoolean", "LiteralTypeDecimal",
        "LiteralTypeFloat", "LiteralTypeInteger", "LiteralTypeString"),
    "IntegerValue": (
        "IntegerValueBigint", "IntegerValueInt8", "IntegerValueInt16", "IntegerValueInt32",
        "IntegerValueInt64", "IntegerValueUint8", "IntegerValueUint16",
        "IntegerValueUint32", "IntegerValueUint64"),
    "FloatValue": ("FloatValueFloat32", "FloatValueFloat64"),
}
# variant class name -> (family name, ordinal), built once from the table above.
_VARIANT_FAMILY_AND_ORDINAL: dict[str, tuple[str, int]] = {
    variant: (family, i)
    for family, variants in _VARIANT_ORDER.items()
    for i, variant in enumerate(variants)
}


def compare(x: Any, y: Any) -> int:
    """Return -1 if x < y, 0 if equal, +1 if x > y, structurally."""
    # Identity short-circuit: any value compares equal to itself. Hot in
    # PersistentMap inserts where lookup-then-skip is common.
    if x is y:
        return 0

    tx, ty = type(x), type(y)

    # Decimals: numeric value first, then scale as tiebreak (spec: 1.1 < 1.10,
    # distinct unequal values) — Decimal's native comparison ignores scale.
    if isinstance(x, Decimal) and isinstance(y, Decimal):
        if x != y:
            return -1 if x < y else 1
        sx, sy = -x.as_tuple().exponent, -y.as_tuple().exponent
        return -1 if sx < sy else (1 if sx > sy else 0)

    # Coder-generated union variants: declared-variant order, then payload.
    fx = _VARIANT_FAMILY_AND_ORDINAL.get(tx.__name__)
    fy = _VARIANT_FAMILY_AND_ORDINAL.get(ty.__name__)
    if fx is not None and fy is not None and fx[0] == fy[0]:
        ox, oy = fx[1], fy[1]
        if ox != oy:
            return -1 if ox < oy else 1
        xv = getattr(x, "value", None)
        yv = getattr(y, "value", None)
        return compare(xv, yv)

    # Enum members (e.g. IntegerType, FloatType): declaration order.
    if isinstance(x, enum.Enum) and isinstance(y, enum.Enum) and tx is ty:
        members = list(tx)
        ox, oy = members.index(x), members.index(y)
        return -1 if ox < oy else (1 if ox > oy else 0)

    # Optionals: None_ < Given(x); Given(x) vs Given(y) compare by payload.
    from hydra.overlay.python.dsl.python import Given, None_, Left, Right
    if isinstance(x, None_) and isinstance(y, None_):
        return 0
    if isinstance(x, None_) and isinstance(y, Given):
        return -1
    if isinstance(x, Given) and isinstance(y, None_):
        return 1
    if isinstance(x, Given) and isinstance(y, Given):
        return compare(x.value, y.value)

    # Eithers: every Left is less than every Right; same side compares by payload.
    if isinstance(x, Left) and isinstance(y, Left):
        return compare(x.value, y.value)
    if isinstance(x, Right) and isinstance(y, Right):
        return compare(x.value, y.value)
    if isinstance(x, Left) and isinstance(y, Right):
        return -1
    if isinstance(x, Right) and isinstance(y, Left):
        return 1

    # Mappings: ascending-key sequence of bindings, each comparing by key then value.
    if isinstance(x, Mapping) and isinstance(y, Mapping):
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

    # Sets: ascending sequence of elements.
    if isinstance(x, Set) and isinstance(y, Set):
        xs = sorted(x, key=cmp_to_key(compare))
        ys = sorted(y, key=cmp_to_key(compare))
        return compare(xs, ys)

    # Lists and other Sequences (e.g. ConsList, backing Record.fields/Term.list):
    # lexicographic, prefix least. str/bytes are excluded -- handled by the
    # native-comparable fallback below.
    if (isinstance(x, Sequence) and isinstance(y, Sequence)
            and not isinstance(x, (str, bytes, bytearray))
            and not isinstance(y, (str, bytes, bytearray))):
        for xi_, yi_ in zip(x, y):
            r = compare(xi_, yi_)
            if r != 0:
                return r
        return compare(len(x), len(y))

    # Dataclass-style objects (records, wrappers): field-declaration order.
    if hasattr(x, "__dataclass_fields__") and tx is ty:
        import dataclasses
        for f in dataclasses.fields(x):
            r = compare(getattr(x, f.name), getattr(y, f.name))
            if r != 0:
                return r
        return 0

    # Zero-payload singleton terms/types (TermUnit, TypeUnit, TypeVoid, ...):
    # a bare __eq__/__hash__ class with no fields, all instances equal.
    if tx is ty and getattr(tx, "__slots__", None) == ():
        return 0

    # Floats (float32 and float64 both surface as Python float): IEEE 754 extended
    # totalOrder per docs/specification/ordering-and-equality.md, not native </>, whose
    # IEEE semantics differ at NaN (unordered) and signed zero (-0.0 == 0.0). Order:
    # -inf < negative finite < -0.0 < +0.0 < positive finite < +inf < NaN (equal to itself).
    if isinstance(x, float) and isinstance(y, float):
        xnan, ynan = math.isnan(x), math.isnan(y)
        if xnan or ynan:
            if xnan and ynan:
                return 0
            return 1 if xnan else -1
        if x != y:
            return -1 if x < y else 1
        # x == y as floats: distinguish signed zero (-0.0 < +0.0); all other
        # equal-valued floats (including equal infinities) are indistinguishable here.
        if x == 0.0:
            xneg, yneg = math.copysign(1.0, x) < 0, math.copysign(1.0, y) < 0
            if xneg != yneg:
                return -1 if xneg else 1
        return 0

    # Native comparables (int, str, bool, bytes, and other Comparable types).
    if x < y:
        return -1
    if x > y:
        return 1
    return 0
