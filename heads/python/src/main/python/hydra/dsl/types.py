"""A domain-specific language for constructing Hydra types in Python."""

from collections.abc import Sequence
from functools import reduce
from typing import Union

import hydra.constants
import hydra.dsl.literal_types as lt
from hydra.core import (
    AnnotatedType,
    ApplicationType,
    Binding,
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
    TypeVariableMetadata,
    TypeWrap,
)
from hydra.dsl.python import FrozenDict, Just, Nothing

# Type alias: anything that can be used as a Type
# Mirrors Haskell's AsType typeclass: Type, Binding, or str
Typeable = Union[Type, Binding, str]


def _as_type(t: Typeable):
    """Coerce a Typeable to a Type.

    - Binding is converted to TypeVariable(binding.name)
    - str is converted to TypeVariable(Name(s))
    - Everything else passes through unchanged (assumed to be a Type)

    This mirrors Haskell's AsType typeclass.
    """
    if isinstance(t, Binding):
        return TypeVariable(t.name)
    elif isinstance(t, str):
        return TypeVariable(Name(t))
    else:
        return t


def use(b: Binding) -> Type:
    """Convert a Binding to a Type by extracting its name as a TypeVariable.

    This mirrors Haskell's AsType Binding instance, enabling type definitions
    to reference other types by their Binding directly.

    Example: use(name_binding)  # equivalent to variable("hydra.core.Name")
    """
    return TypeVariable(b.name)


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


def function(dom: Typeable, cod: Typeable) -> Type:
    """Create a function type.

    Example: function(int32(), string())
    """
    return TypeFunction(FunctionType(_as_type(dom), _as_type(cod)))


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


def poly_constrained(vs_with_constraints: Sequence[tuple[str, list[Name]]], t: Type) -> TypeScheme:
    """Create a polymorphic type scheme with type variables and class constraints.

    Example: poly_constrained([("k", [Name("hydra.util.TypeClass.ordering")]), ("v", [])],
                              function(var("k"), var("v")))
    """
    vars = tuple(Name(v) for v, _ in vs_with_constraints)
    constraint_map = FrozenDict({
        Name(v): TypeVariableMetadata(frozenset(classes))
        for v, classes in vs_with_constraints if classes
    })
    return TypeScheme(vars, t, Just(constraint_map) if constraint_map else Nothing())


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


def decimal() -> Type:
    """Arbitrary-precision exact decimal type."""
    return literal(lt.decimal())


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


def list_(t: Typeable) -> Type:
    """List type.

    Example: list_(string())
    Example: list_(field_type_binding)  # reference another type by its Binding
    """
    return TypeList(_as_type(t))


def map_(k: Typeable, v: Typeable) -> Type:
    """Map/dictionary type with key and value types.

    Example: map_(string(), int32())
    Example: map_(name_binding, term_binding)
    """
    return TypeMap(MapType(_as_type(k), _as_type(v)))


def maybe(t: Typeable) -> Type:
    """Maybe (nullable) type.

    Example: maybe(string())
    """
    return TypeMaybe(_as_type(t))


def optional(t: Typeable) -> Type:
    """Optional (nullable) type (alias for 'maybe').

    Example: optional(string())
    """
    return maybe(t)


def set_(t: Typeable) -> Type:
    """Set type.

    Example: set_(string())
    """
    return TypeSet(_as_type(t))


def enum(names: Sequence[str]) -> Type:
    """Create an enum type with the given variant names (conventionally in camelCase).

    Example: enum(["red", "green", "blue"])
    """
    return union([field(n, unit()) for n in names])


def field(fn: str, t: Typeable) -> FieldType:
    """Create a field with the given name and type.

    The type argument can be a Type, a Binding (auto-coerced to TypeVariable),
    or a string (auto-coerced to TypeVariable).

    Example: field("age", int32())
    Example: field("name", name_binding)  # reference another type by its Binding
    """
    return FieldType(Name(fn), _as_type(t))


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
    return TypeRecord(tuple(fields))


def unit() -> Type:
    """Unit type (empty record type)."""
    return TypeUnit()


def union(fields: Sequence[FieldType]) -> Type:
    """Create a union type with the given variants and the default type name.

    Example: union([field("success", int32()), field("failure", string())])
    This creates a tagged union type (sum type with named variants).
    """
    return TypeUnion(tuple(fields))


def wrap(t: Typeable) -> Type:
    """Create a wrapped type (newtype) with a provided base type and the default type name.

    Example: wrap(string())
    Creates a newtype with placeholder name; use 'wrap_with_name' for custom names.
    """
    return wrap_with_name(hydra.constants.placeholder_name, _as_type(t))


def wrap_with_name(name: Name, t: Type) -> Type:
    """Create a wrapped type (newtype) with a provided base type and type name.

    Example: wrap_with_name(Name("Email"), string())
    """
    return TypeWrap(t)


def pair(a: Typeable, b: Typeable) -> Type:
    """Create a pair type.

    Example: pair(string(), int32())
    """
    return TypePair(PairType(_as_type(a), _as_type(b)))


def either(left: Typeable, right: Typeable) -> Type:
    """Create an either type (a choice between two types).

    Example: either(string(), int32())
    """
    return TypeEither(EitherType(_as_type(left), _as_type(right)))


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
