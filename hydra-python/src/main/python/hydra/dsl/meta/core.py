"""Meta-DSL for constructing Hydra core terms and types as first-class values.

Mirrors the Haskell module Hydra.Dsl.Meta.Core, providing phantom-typed constructors,
accessors, and modifiers for all core Hydra types.

Most functions are re-exported from the generated module hydra.dsl.core. This module
adds custom helpers (equality, lifting, convenience constructors) and provides function
wrappers for generated unit-enum constants so they can be called with ().
"""

import hydra.core as C
import hydra.dsl.meta.phantoms as Phantoms
from hydra.core import Name
from hydra.phantoms import TTerm

# Re-export everything from the generated DSL module.
from hydra.dsl.core import *  # noqa: F401, F403

# Import the generated module under a name so we can reference its constants.
import hydra.dsl.core as _Gen


# ============================================================
# Aliases for Python reserved words
# ============================================================

def let_(bindings: TTerm, body: TTerm) -> TTerm:
    """Construct a Let (alias for generated `let` which shadows the Python keyword)."""
    return _Gen.let(bindings, body)


# ============================================================
# Function wrappers for generated unit-enum constants
#
# The generated module defines these as module-level TTerm constants.
# We override them as zero-argument functions for backward compatibility,
# since the meta module historically defined them as functions.
# ============================================================

def float_type_bigfloat() -> TTerm:
    """The bigfloat FloatType variant."""
    return _Gen.float_type_bigfloat


def float_type_float32() -> TTerm:
    """The float32 FloatType variant."""
    return _Gen.float_type_float32


def float_type_float64() -> TTerm:
    """The float64 FloatType variant."""
    return _Gen.float_type_float64


def integer_type_bigint() -> TTerm:
    """The bigint IntegerType variant."""
    return _Gen.integer_type_bigint


def integer_type_int8() -> TTerm:
    """The int8 IntegerType variant."""
    return _Gen.integer_type_int8


def integer_type_int16() -> TTerm:
    """The int16 IntegerType variant."""
    return _Gen.integer_type_int16


def integer_type_int32() -> TTerm:
    """The int32 IntegerType variant."""
    return _Gen.integer_type_int32


def integer_type_int64() -> TTerm:
    """The int64 IntegerType variant."""
    return _Gen.integer_type_int64


def integer_type_uint8() -> TTerm:
    """The uint8 IntegerType variant."""
    return _Gen.integer_type_uint8


def integer_type_uint16() -> TTerm:
    """The uint16 IntegerType variant."""
    return _Gen.integer_type_uint16


def integer_type_uint32() -> TTerm:
    """The uint32 IntegerType variant."""
    return _Gen.integer_type_uint32


def integer_type_uint64() -> TTerm:
    """The uint64 IntegerType variant."""
    return _Gen.integer_type_uint64


def literal_type_binary() -> TTerm:
    """The binary LiteralType variant."""
    return _Gen.literal_type_binary


def literal_type_boolean() -> TTerm:
    """The boolean LiteralType variant."""
    return _Gen.literal_type_boolean


def literal_type_string() -> TTerm:
    """The string LiteralType variant."""
    return _Gen.literal_type_string


def term_unit() -> TTerm:
    """The unit Term variant."""
    return _Gen.term_unit


def type_unit() -> TTerm:
    """The unit Type variant."""
    return _Gen.type_unit


# ============================================================
# Name helpers (custom)
# ============================================================

def name_lift(n: Name) -> TTerm:
    """Lift a Python Name value into a TTerm Name."""
    return Phantoms.wrap(C.Name.TYPE_, Phantoms.string(n.value))


def un_namespace(ns: TTerm) -> TTerm:
    """Unwrap a Namespace to its underlying string."""
    _NAMESPACE_NAME = Name("hydra.module.Namespace")
    return Phantoms.apply(Phantoms.unwrap(_NAMESPACE_NAME), ns)


# ============================================================
# Non-schema helpers (custom)
# ============================================================

def equal_name(left: TTerm, right: TTerm) -> TTerm:
    """Check equality of two Names by comparing their string values."""
    import hydra.dsl.meta.lib.equality as Equality
    return Equality.equal(un_name(left), un_name(right))


def equal_name_list(lefts: TTerm, rights: TTerm) -> TTerm:
    """Check equality of two Name lists."""
    import hydra.dsl.meta.lib.equality as Equality
    import hydra.dsl.meta.lib.lists as Lists
    import hydra.dsl.meta.lib.logic as Logic
    equal_name_fn = Phantoms.lambdas(
        ["left", "right"],
        Equality.equal(un_name(Phantoms.var("left")), un_name(Phantoms.var("right"))))
    return Logic.and_(
        Equality.equal(Lists.length(lefts), Lists.length(rights)),
        Logic.ands(Lists.zip_with(equal_name_fn, lefts, rights)))


def field_with_term_value(ft: TTerm, t: TTerm) -> TTerm:
    """Return a Field with a replacement term (same name)."""
    return _Gen.field(field_name(ft), t)


def field_type_with_type_value(ft: TTerm, t: TTerm) -> TTerm:
    """Return a FieldType with a replacement type (same name)."""
    return _Gen.field_type(field_type_name(ft), t)


# ============================================================
# Convenience: encoded Term constructors
# (Used in meta-level encoding, e.g. for building terms that represent terms)
# ============================================================

def int32_term(v: int) -> TTerm:
    """Create an encoded int32 Term value."""
    return term_literal(literal_integer(integer_value_int32(Phantoms.int32(v))))


def string_term(s: str) -> TTerm:
    """Create an encoded string Term value."""
    return term_literal(literal_string(Phantoms.string(s)))


def false_term() -> TTerm:
    """Create an encoded boolean false Term value."""
    return term_literal(literal_boolean(Phantoms.false))
