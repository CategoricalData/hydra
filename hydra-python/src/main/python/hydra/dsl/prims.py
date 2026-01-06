"""Python implementations of primitive construction utilities following the Haskell pattern."""

from collections.abc import Callable
from decimal import Decimal
from typing import TypeVar, cast

import hydra.lib.flows as flows
import hydra.dsl.terms as terms
import hydra.dsl.types as types
import hydra.extract.core as extract
from hydra.compute import Coder, Flow
from hydra.core import (
    FloatType,
    FloatValue,
    IntegerType,
    IntegerValue,
    Literal,
    LiteralType,
    Name,
    Term,
    Type,
)
from hydra.dsl.python import FrozenDict, Maybe, Just, Nothing, frozenlist, Either, Left, Right
from hydra.graph import Graph, Primitive, TermCoder
from hydra.util import Comparison

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
D = TypeVar("D")
S = TypeVar("S")
X = TypeVar("X")
Y = TypeVar("Y")


# Basic numeric types

def bigfloat() -> TermCoder[Decimal]:
    """TermCoder for arbitrary-precision floating-point."""
    return TermCoder(
        type=types.bigfloat(),
        coder=Coder(
            encode=extract.bigfloat,
            decode=lambda v: flows.pure(terms.bigfloat(float(v)))
        ),
    )


def bigint() -> TermCoder[int]:
    """TermCoder for arbitrary-precision integers."""
    return TermCoder(
        type=types.bigint(),
        coder=Coder(
            encode=extract.bigint,
            decode=lambda v: flows.pure(terms.bigint(v))
        ),
    )


def binary() -> TermCoder[bytes]:
    """TermCoder for binary data."""
    return TermCoder(
        type=types.binary(),
        coder=Coder(
            encode=extract.binary,
            decode=lambda v: flows.pure(terms.binary(v))
        ),
    )


def boolean() -> TermCoder[bool]:
    """TermCoder for booleans."""
    return TermCoder(
        type=types.boolean(),
        coder=Coder(
            encode=extract.boolean,
            decode=lambda v: flows.pure(terms.boolean(v))
        ),
    )


def float32() -> TermCoder[float]:
    """TermCoder for 32-bit floating-point."""
    return TermCoder(
        type=types.float32(),
        coder=Coder(
            encode=extract.float32,
            decode=lambda v: flows.pure(terms.float32(v))
        ),
    )


def float64() -> TermCoder[float]:
    """TermCoder for 64-bit floating-point."""
    return TermCoder(
        type=types.float64(),
        coder=Coder(
            encode=extract.float64,
            decode=lambda v: flows.pure(terms.float64(v))
        ),
    )


def int8() -> TermCoder[int]:
    """TermCoder for 8-bit signed integers."""
    return TermCoder(
        type=types.int8(),
        coder=Coder(
            encode=extract.int8,
            decode=lambda v: flows.pure(terms.int8(v))
        ),
    )


def int16() -> TermCoder[int]:
    """TermCoder for 16-bit signed integers."""
    return TermCoder(
        type=types.int16(),
        coder=Coder(
            encode=extract.int16,
            decode=lambda v: flows.pure(terms.int16(v))
        ),
    )


def int32() -> TermCoder[int]:
    """TermCoder for 32-bit signed integers."""
    return TermCoder(
        type=types.int32(),
        coder=Coder(
            encode=extract.int32,
            decode=lambda v: flows.pure(terms.int32(v))
        ),
    )


def int64() -> TermCoder[int]:
    """TermCoder for 64-bit signed integers."""
    return TermCoder(
        type=types.int64(),
        coder=Coder(
            encode=extract.int64,
            decode=lambda v: flows.pure(terms.int64(v))
        ),
    )


def uint8() -> TermCoder[int]:
    """TermCoder for 8-bit unsigned integers."""
    return TermCoder(
        type=types.uint8(),
        coder=Coder(
            encode=extract.uint8,
            decode=lambda v: flows.pure(terms.uint8(v))
        ),
    )


def uint16() -> TermCoder[int]:
    """TermCoder for 16-bit unsigned integers."""
    return TermCoder(
        type=types.uint16(),
        coder=Coder(
            encode=extract.uint16,
            decode=lambda v: flows.pure(terms.uint16(v))
        ),
    )


def uint32() -> TermCoder[int]:
    """TermCoder for 32-bit unsigned integers."""
    return TermCoder(
        type=types.uint32(),
        coder=Coder(
            encode=extract.uint32,
            decode=lambda v: flows.pure(terms.uint32(v))
        ),
    )


def uint64() -> TermCoder[int]:
    """TermCoder for 64-bit unsigned integers."""
    return TermCoder(
        type=types.uint64(),
        coder=Coder(
            encode=extract.uint64,
            decode=lambda v: flows.pure(terms.uint64(v))
        ),
    )


def string() -> TermCoder[str]:
    """TermCoder for strings."""
    return TermCoder(
        type=types.string(),
        coder=Coder(
            encode=extract.string,
            decode=lambda s: flows.pure(terms.string(s))
        ),
    )


# Core value types

def comparison() -> TermCoder[Comparison]:
    """TermCoder for Comparison values."""
    import hydra.extract.util as extract_util
    return TermCoder(
        type=types.var("Comparison"),
        coder=Coder(
            encode=extract_util.comparison,
            decode=lambda c: flows.pure(terms.comparison(c))
        ),
    )


def float_type() -> TermCoder[FloatType]:
    """TermCoder for FloatType values."""
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    return TermCoder(
        type=types.var("FloatType"),
        coder=Coder(
            encode=decode_core.float_type,
            decode=lambda ft: flows.pure(encode_core.float_type(ft))
        ),
    )


def float_value() -> TermCoder[FloatValue]:
    """TermCoder for FloatValue values."""
    return TermCoder(
        type=types.var("FloatValue"),
        coder=Coder(
            encode=extract.float_value,
            decode=lambda fv: flows.pure(terms.float_(fv))
        ),
    )


def integer_type() -> TermCoder[IntegerType]:
    """TermCoder for IntegerType values."""
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    return TermCoder(
        type=types.var("IntegerType"),
        coder=Coder(
            encode=decode_core.integer_type,
            decode=lambda it: flows.pure(encode_core.integer_type(it))
        ),
    )


def integer_value() -> TermCoder[IntegerValue]:
    """TermCoder for IntegerValue values."""
    return TermCoder(
        type=types.var("IntegerValue"),
        coder=Coder(
            encode=extract.integer_value,
            decode=lambda iv: flows.pure(terms.integer(iv))
        ),
    )


def literal() -> TermCoder[Literal]:
    """TermCoder for Literal values."""
    return TermCoder(
        type=types.var("Literal"),
        coder=Coder(
            encode=extract.literal,
            decode=lambda lit: flows.pure(terms.literal(lit))
        ),
    )


def literal_type() -> TermCoder[LiteralType]:
    """TermCoder for LiteralType values."""
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    return TermCoder(
        type=types.var("LiteralType"),
        coder=Coder(
            encode=decode_core.literal_type,
            decode=lambda lt: flows.pure(encode_core.literal_type(lt))
        ),
    )


def term() -> TermCoder[Term]:
    """TermCoder for Term values."""
    return TermCoder(
        type=types.var("Term"),
        coder=Coder(
            encode=lambda t: flows.pure(t),
            decode=lambda t: flows.pure(t)
        ),
    )


def type_() -> TermCoder[Type]:
    """TermCoder for Type values."""
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    return TermCoder(
        type=types.var("Type"),
        coder=Coder(
            encode=decode_core.type,
            decode=lambda t: flows.pure(encode_core.type(t))
        ),
    )


# Container types

def list_(els: TermCoder[X]) -> TermCoder[frozenlist[X]]:
    """TermCoder for lists."""
    return TermCoder(
        type=types.list_(els.type),
        coder=Coder(
            encode=lambda term: extract.list_of(els.coder.encode, term),
            decode=lambda lst: flows.bind(
                flows.map_list(els.coder.decode, lst),
                lambda decoded: flows.pure(terms.list_(decoded))
            )
        ),
    )


def map_(keys: TermCoder[X], values: TermCoder[Y]) -> TermCoder[FrozenDict[X, Y]]:
    """TermCoder for maps."""
    return TermCoder(
        type=types.map_(keys.type, values.type),
        coder=Coder(
            encode=lambda term: extract.map(keys.coder.encode, values.coder.encode, term),
            decode=lambda m: flows.bind(
                flows.map_list(
                    lambda kv: flows.bind(
                        keys.coder.decode(kv[0]),
                        lambda k: flows.bind(
                            values.coder.decode(kv[1]),
                            lambda v: flows.pure((k, v))
                        )
                    ),
                    list(m.items())
                ),
                lambda pairs: flows.pure(terms.map_(FrozenDict(dict(pairs))))
            )
        ),
    )

from typing import cast

def optional(mel: TermCoder[X]) -> TermCoder[Maybe[X]]:
    """TermCoder for optional values."""
    def to_maybe_term(mv: Maybe[X]) -> Flow[Graph, Maybe[Term]]:
        if isinstance(mv, Nothing):
            # Make the branch type explicit: Flow[Graph, Maybe[Term]]
            return cast(Flow[Graph, Maybe[Term]], flows.pure(Nothing()))
        else:
            # mv: Just[X] -> decode X to Term, then wrap as Just[Term]
            return flows.bind(
                mel.coder.decode(mv.value),            # Flow[Graph, Term]
                lambda v_term: flows.pure(Just(cast(Term, v_term)))  # Flow[Graph, Just[Term]]
            )

    return TermCoder(
        type=types.optional(mel.type),
        coder=Coder(
            encode=lambda term: extract.maybe_term(mel.coder.encode, term),
            decode=lambda mv: flows.bind(
                to_maybe_term(mv),                      # Flow[Graph, Maybe[Term]]
                lambda decoded: flows.pure(terms.optional(decoded))
            ),
        ),
    )


def either(left_coder: TermCoder[X], right_coder: TermCoder[Y]) -> TermCoder[Either[X, Y]]:
    """TermCoder for Either values."""
    def to_either_term(ev: Either[X, Y]) -> Flow[Graph, Term]:
        if isinstance(ev, Left):
            # Decode left value and wrap in left term
            return flows.bind(
                left_coder.coder.decode(ev.value),
                lambda left_term: flows.pure(terms.left(left_term))
            )
        else:  # Right
            # Decode right value and wrap in right term
            return flows.bind(
                right_coder.coder.decode(ev.value),
                lambda right_term: flows.pure(terms.right(right_term))
            )

    return TermCoder(
        type=types.either(left_coder.type, right_coder.type),
        coder=Coder(
            encode=lambda term: extract.either_term(left_coder.coder.encode, right_coder.coder.encode, term),
            decode=to_either_term,
        ),
    )


def pair(first_coder: TermCoder[X], second_coder: TermCoder[Y]) -> TermCoder[tuple[X, Y]]:
    """TermCoder for pairs."""
    return TermCoder(
        type=types.pair(first_coder.type, second_coder.type),
        coder=Coder(
            encode=lambda term: extract.pair(first_coder.coder.encode, second_coder.coder.encode, term),
            decode=lambda p: flows.bind(
                first_coder.coder.decode(p[0]),
                lambda first_term: flows.bind(
                    second_coder.coder.decode(p[1]),
                    lambda second_term: flows.pure(terms.pair(first_term, second_term))
                )
            )
        ),
    )


def set_(els: TermCoder[X]) -> TermCoder[frozenset[X]]:
    """TermCoder for sets."""
    return TermCoder(
        type=types.set_(els.type),
        coder=Coder(
            encode=lambda term: extract.set_of(els.coder.encode, term),
            decode=lambda s: flows.bind(
                flows.map_list(els.coder.decode, list(s)),
                lambda decoded: flows.pure(terms.set_(set(decoded)))
            )
        ),
    )


# Special types

def flow(states: TermCoder[S], values: TermCoder[X]) -> TermCoder[Flow[S, X]]:
    """TermCoder for Flow values (not actually encodable/decodable)."""
    return TermCoder(
        type=types.apply_many([
            types.var("Flow"),
            states.type,
            values.type
        ]),
        coder=Coder(
            encode=lambda _: flows.fail("cannot currently encode flows from terms"),
            decode=lambda _: flows.fail("cannot decode flows to terms")
        ),
    )


def function(dom: TermCoder[X], cod: TermCoder[Y]) -> TermCoder[Callable[[X], Y]]:
    """TermCoder for function values (not actually encodable/decodable)."""
    return TermCoder(
        type=types.function(dom.type, cod.type),
        coder=Coder(
            encode=lambda term: flows.fail(f"cannot encode term to a function: {term}"),
            decode=lambda _: flows.fail("cannot decode functions to terms")
        ),
    )


def variable(v: str) -> TermCoder[Term]:
    """TermCoder for type variables."""
    return TermCoder(
        type=types.var(v),
        coder=Coder(
            encode=lambda t: flows.pure(t),
            decode=lambda t: flows.pure(t)
        ),
    )


# Primitive constructors

def no_interpreted_form(name: Name) -> Flow[Graph, Term]:
    """Helper for primitives without interpreted forms."""
    return flows.fail(
        f"primitive {name.value} does not have an interpreted form; "
        "it can only be used in compiled code"
    )


def prim0(
        name: Name, value: Callable[[], A], variables: list[str], output: TermCoder[A]
) -> Primitive:
    """Create a 0-argument primitive function.

    The value parameter is a callable that returns the constant value.
    """

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        return output.coder.decode(value())

    return Primitive(
        name=name, type=types.poly(variables, output.type), implementation=impl
    )


def prim1(
        name: Name,
        compute: Callable[[A], B],
        variables: list[str],
        input1: TermCoder[A],
        output: TermCoder[B],
) -> Primitive:
    """Create a 1-argument primitive function."""

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        def check_args(_: None) -> Flow[Graph, Term]:
            def process_arg1(arg1: A) -> Flow[Graph, Term]:
                result = compute(arg1)
                return output.coder.decode(result)

            return flows.bind(input1.coder.encode(args[0]), process_arg1)

        return flows.bind(extract.n_args(name, 1, args), check_args)

    return Primitive(
        name=name,
        type=types.poly(variables, types.function(input1.type, output.type)),
        implementation=impl,
    )


def prim2(
        name: Name,
        compute: Callable[[A, B], C],
        variables: list[str],
        input1: TermCoder[A],
        input2: TermCoder[B],
        output: TermCoder[C],
) -> Primitive:
    """Create a 2-argument primitive function."""

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        def check_args(_: None) -> Flow[Graph, Term]:
            def process_arg1(arg1: A) -> Flow[Graph, Term]:
                def process_arg2(arg2: B) -> Flow[Graph, Term]:
                    result = compute(arg1, arg2)
                    return output.coder.decode(result)

                return flows.bind(input2.coder.encode(args[1]), process_arg2)

            return flows.bind(input1.coder.encode(args[0]), process_arg1)

        return flows.bind(extract.n_args(name, 2, args), check_args)

    return Primitive(
        name=name,
        type=types.poly(
            variables,
            types.function(input1.type, types.function(input2.type, output.type)),
        ),
        implementation=impl,
    )


def prim2_interp(
        name: Name,
        compute: Maybe[Callable[[Term, Term], Flow[Graph, Term]]],
        variables: list[str],
        input1: TermCoder[A],
        input2: TermCoder[B],
        output: TermCoder[C],
) -> Primitive:
    """Create a 2-argument primitive function with optional interpreted form."""

    def default_compute(a: Term, b: Term) -> Flow[Graph, Term]:
        return no_interpreted_form(name)

    match compute:
        case Just(comp):
            actual_compute = comp
        case Nothing():
            actual_compute = default_compute

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        def check_args(_: None) -> Flow[Graph, Term]:
            return actual_compute(args[0], args[1])

        return flows.bind(extract.n_args(name, 2, args), check_args)

    return Primitive(
        name=name,
        type=types.poly(
            variables,
            types.function(input1.type, types.function(input2.type, output.type)),
        ),
        implementation=impl,
    )


def prim3(
        name: Name,
        compute: Callable[[A, B, C], D],
        variables: list[str],
        input1: TermCoder[A],
        input2: TermCoder[B],
        input3: TermCoder[C],
        output: TermCoder[D],
) -> Primitive:
    """Create a 3-argument primitive function."""

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        def check_args(_: None) -> Flow[Graph, Term]:
            def process_arg1(arg1: A) -> Flow[Graph, Term]:
                def process_arg2(arg2: B) -> Flow[Graph, Term]:
                    def process_arg3(arg3: C) -> Flow[Graph, Term]:
                        result = compute(arg1, arg2, arg3)
                        return output.coder.decode(result)

                    return flows.bind(input3.coder.encode(args[2]), process_arg3)

                return flows.bind(input2.coder.encode(args[1]), process_arg2)

            return flows.bind(input1.coder.encode(args[0]), process_arg1)

        return flows.bind(extract.n_args(name, 3, args), check_args)

    return Primitive(
        name=name,
        type=types.poly(
            variables,
            types.function(
                input1.type,
                types.function(input2.type, types.function(input3.type, output.type)),
            ),
        ),
        implementation=impl,
    )


def prim3_interp(
        name: Name,
        compute: Maybe[Callable[[Term, Term, Term], Flow[Graph, Term]]],
        variables: list[str],
        input1: TermCoder[A],
        input2: TermCoder[B],
        input3: TermCoder[C],
        output: TermCoder[D],
) -> Primitive:
    """Create a 3-argument primitive function with optional interpreted form."""

    def default_compute(a: Term, b: Term, c: Term) -> Flow[Graph, Term]:
        return no_interpreted_form(name)

    match compute:
        case Just(comp):
            actual_compute = comp
        case Nothing():
            actual_compute = default_compute

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        def check_args(_: None) -> Flow[Graph, Term]:
            return actual_compute(args[0], args[1], args[2])

        return flows.bind(extract.n_args(name, 3, args), check_args)

    return Primitive(
        name=name,
        type=types.poly(
            variables,
            types.function(
                input1.type,
                types.function(input2.type, types.function(input3.type, output.type)),
            ),
        ),
        implementation=impl,
    )
