"""A domain-specific language for constructing Hydra types in Python."""

from collections.abc import Sequence
from functools import reduce

import hydra.constants
import hydra.dsl.literal_types as lt
from hydra.core import (
    AnnotatedType,
    ApplicationType,
    EitherType,
    FieldType,
    FloatType,
    ForallType,
    FunctionType,
    IntegerType,
    LiteralType,
    MapType,
    Name,
    PairType,
    RowType,
    Term,
    Type,
    TypeAnnotated,
    TypeApplication,
    TypeEither,
    TypeForall,
    TypeFunction,
    TypeList,
    TypeLiteral,
    TypeMap,
    TypeMaybe,
    TypePair,
    TypeRecord,
    TypeScheme,
    TypeSet,
    TypeUnion,
    TypeUnit,
    TypeVariable,
    TypeWrap,
    WrappedType,
)
from hydra.dsl.python import FrozenDict, Nothing


# Operators - TODO: find Python equivalents for these special syntax forms
# (-->) :: Type -> Type -> Type  -- function type constructor operator
# (@@) :: Type -> Type -> Type   -- type application operator
# (>:) :: String -> Type -> FieldType  -- field definition operator


def annot(ann: dict[str, Term], t: Type) -> Type:
    """Attach an annotation to a type.

    Example: annot({"min": int32_term(0), "max": int32_term(100)}, int32())
    """
    return TypeAnnotated(
        AnnotatedType(t, FrozenDict({Name(k): v for k, v in ann.items()}))
    )


def apply(lhs: Type, rhs: Type) -> Type:
    """Apply a type to a type argument.

    Example: apply(var("f"), int32())
    """
    return TypeApplication(ApplicationType(lhs, rhs))


def applys(t: Type, ts: Sequence[Type]) -> Type:
    """Apply a type to multiple type arguments.

    Example: applys(var("Either"), [string(), int32()])
    """
    return reduce(apply, ts, t)


def apply_many(ts: Sequence[Type]) -> Type:
    """Apply a type to multiple type arguments (takes types as a sequence)."""
    return reduce(apply, ts[1:], ts[0])


def var(name: str) -> Type:
    """Create a type variable with the given name (alias for 'variable').

    Example: var("a")
    """
    return variable(name)


def variable(name: str) -> Type:
    """Create a type variable with the given name.

    Example: variable("a")
    """
    return TypeVariable(Name(name))


def function(dom: Type, cod: Type) -> Type:
    """Create a function type.

    Example: function(int32(), string())
    """
    return TypeFunction(FunctionType(dom, cod))


def function_many(ts: Sequence[Type]) -> Type:
    """Create an n-ary function type.

    Example: function_many([int32(), string(), boolean()])
    """
    r = list(reversed(ts))
    return reduce(lambda cod, dom: function(dom, cod), r[1:], r[0])


def forall(v: str, body: Type) -> Type:
    """Create a universally quantified type (polymorphic type) with a single variable.

    Example: forall("a", function(var("a"), var("a")))
    This creates the polymorphic identity function type: forall a. a -> a
    Universal quantification introduces type variables that can be used in the body.
    """
    return TypeForall(ForallType(Name(v), body))


def foralls(vs: Sequence[str], body: Type) -> Type:
    """Universal quantification with multiple variables.

    Example: foralls(["a", "b"], function(var("a"), var("b")))
    """
    return reduce(lambda acc, v: forall(v, acc), reversed(vs), body)


def mono(t: Type) -> TypeScheme:
    """Create a monomorphic type scheme.

    Example: mono(int32())
    """
    return TypeScheme((), t, Nothing())


def poly(vs: Sequence[str], t: Type) -> TypeScheme:
    """Create a polymorphic type scheme with explicit type variables.

    Example: poly(["a", "b"], function(var("a"), var("b")))
    This represents a type forall a b. a -> b that can be instantiated with different types.
    """
    return TypeScheme(tuple(Name(v) for v in vs), t, Nothing())


def scheme(*args) -> TypeScheme:
    """Create a type scheme with the given type variables and body type.

    Example: scheme("a", "b", function(var("a"), var("b")))
    Example: scheme(int32())  # monomorphic scheme
    """
    if len(args) == 1:
        return mono(args[0])
    else:
        return poly(list(args[:-1]), args[-1])


def bigfloat() -> Type:
    """Arbitrary-precision floating point type."""
    return literal(lt.bigfloat())


def bigint() -> Type:
    """Arbitrary-precision integer type."""
    return literal(lt.bigint())


def binary() -> Type:
    """Binary data type."""
    return literal(lt.binary())


def boolean() -> Type:
    """Boolean type."""
    return literal(lt.boolean())


def float32() -> Type:
    """32-bit floating point type."""
    return literal(lt.float32())


def float64() -> Type:
    """64-bit floating point type."""
    return literal(lt.float64())


def float_(ftype: FloatType) -> Type:
    """Create a floating point type with the specified precision."""
    return literal(lt.float_(ftype))


def int8() -> Type:
    """8-bit signed integer type."""
    return literal(lt.int8())


def int16() -> Type:
    """16-bit signed integer type."""
    return literal(lt.int16())


def int32() -> Type:
    """32-bit signed integer type."""
    return literal(lt.int32())


def int64() -> Type:
    """64-bit signed integer type."""
    return literal(lt.int64())


def integer(itype: IntegerType) -> Type:
    """Create an integer type with the specified bit width."""
    return literal(lt.integer(itype))


def literal(t: LiteralType) -> Type:
    """Literal primitive type."""
    return TypeLiteral(t)


def non_negative_int32() -> Type:
    """Non-negative 32-bit integer type."""
    return int32()


def string() -> Type:
    """String type."""
    return literal(lt.string())


def uint8() -> Type:
    """8-bit unsigned integer type."""
    return literal(lt.uint8())


def uint16() -> Type:
    """16-bit unsigned integer type."""
    return literal(lt.uint16())


def uint32() -> Type:
    """32-bit unsigned integer type."""
    return literal(lt.uint32())


def uint64() -> Type:
    """64-bit unsigned integer type."""
    return literal(lt.uint64())


def list_(t: Type) -> Type:
    """List type.

    Example: list_(string())
    """
    return TypeList(t)


def map_(k: Type, v: Type) -> Type:
    """Map/dictionary type with key and value types.

    Example: map_(string(), int32())
    """
    return TypeMap(MapType(k, v))


def maybe(t: Type) -> Type:
    """Maybe (optional/nullable) type.

    Example: maybe(string())
    """
    return TypeMaybe(t)


def optional(t: Type) -> Type:
    """Optional (nullable) type (alias for 'maybe').

    Example: optional(string())
    """
    return maybe(t)


def set_(t: Type) -> Type:
    """Set type.

    Example: set_(string())
    """
    return TypeSet(t)


def flow(state: Type, value: Type) -> Type:
    """Flow type (monadic computation with state, trace, and error handling).

    Example: flow(var("s"), var("x"))  # Flow s x
    """
    return apply(apply(TypeVariable(Name("hydra.compute.Flow")), state), value)


def enum(names: Sequence[str]) -> Type:
    """Create an enum type with the given variant names (conventionally in camelCase).

    Example: enum(["red", "green", "blue"])
    """
    return union([field(n, unit()) for n in names])


def field(fn: str, t: Type) -> FieldType:
    """Create a field with the given name and type.

    Example: field("age", int32())
    """
    return FieldType(Name(fn), t)


def record(fields: Sequence[FieldType]) -> Type:
    """Create a record type with the given fields and the default type name.

    Example: record([field("name", string()), field("age", int32())])
    Use 'record_with_name' to specify a custom type name.
    """
    return record_with_name(hydra.constants.placeholder_name, fields)


def record_with_name(tname: Name, fields: Sequence[FieldType]) -> Type:
    """Create a record type with the given fields and a provided type name.

    Example: record_with_name(Name("Person"), [field("name", string()), field("age", int32())])
    """
    return TypeRecord(RowType(tname, tuple(fields)))


def unit() -> Type:
    """Unit type (empty record type)."""
    return TypeUnit()


def union(fields: Sequence[FieldType]) -> Type:
    """Create a union type with the given variants and the default type name.

    Example: union([field("success", int32()), field("failure", string())])
    This creates a tagged union type (sum type with named variants).
    """
    return TypeUnion(RowType(hydra.constants.placeholder_name, tuple(fields)))


def wrap(t: Type) -> Type:
    """Create a wrapped type (newtype) with a provided base type and the default type name.

    Example: wrap(string())
    Creates a newtype with placeholder name; use 'wrap_with_name' for custom names.
    """
    return wrap_with_name(hydra.constants.placeholder_name, t)


def wrap_with_name(name: Name, t: Type) -> Type:
    """Create a wrapped type (newtype) with a provided base type and type name.

    Example: wrap_with_name(Name("Email"), string())
    """
    return TypeWrap(WrappedType(name, t))


def pair(a: Type, b: Type) -> Type:
    """Create a pair type.

    Example: pair(string(), int32())
    """
    return TypePair(PairType(a, b))


def either(left: Type, right: Type) -> Type:
    """Create an either type (a choice between two types).

    Example: either(string(), int32())
    """
    return TypeEither(EitherType(left, right))


def product(ts: Sequence[Type]) -> Type:
    """Create a product type using nested pairs.

    Example: product([string(), int32(), boolean()]) creates pair(string(), pair(int32(), boolean()))
    """
    if len(ts) == 0:
        return unit()
    elif len(ts) == 1:
        return ts[0]
    elif len(ts) == 2:
        return pair(ts[0], ts[1])
    else:
        return pair(ts[0], product(ts[1:]))
