"""Python implementations of primitive construction utilities following the Haskell pattern."""

from collections.abc import Callable
from decimal import Decimal
from typing import TypeVar, cast

import hydra.dsl.terms as terms
import hydra.dsl.types as types
import hydra.extract.core as extract
from hydra.context import Context, InContext
from hydra.core import (
    Application,
    FloatType,
    FloatValue,
    IntegerType,
    IntegerValue,
    Literal,
    LiteralType,
    Name,
    Term,
    TermApplication,
    Type,
)
from hydra.dsl.python import FrozenDict, Maybe, Just, Nothing, frozenlist, Either, Left, Right
from hydra.error import Error, ErrorOther, OtherError
from hydra.graph import Graph, Primitive, TermCoder
from hydra.util import Comparison

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
D = TypeVar("D")
S = TypeVar("S")
X = TypeVar("X")
Y = TypeVar("Y")


def other_err(cx: Context, msg: str) -> InContext[OtherError]:
    """Create an OtherError InContext from a string message and context."""
    return InContext(object=OtherError(msg), context=cx)


def wrap_other(cx: Context, result: Either[InContext[OtherError], A]) -> Either[InContext[Error], A]:
    """Wrap Either (InContext OtherError) into Either (InContext Error)."""
    match result:
        case Right(value=v):
            return Right(v)
        case Left(value=ic):
            return Left(InContext(object=ErrorOther(ic.object), context=ic.context))



# Type variable specs

class TypeVar_:
    def __init__(self, name: str, classes: list[Name] = None):
        self.name = name
        self.classes = classes or []


def v(name: str) -> TypeVar_:
    return TypeVar_(name)


def v_ord(name: str) -> TypeVar_:
    return TypeVar_(name, [Name("hydra.util.TypeClass.ordering")])


def v_eq(name: str) -> TypeVar_:
    return TypeVar_(name, [Name("hydra.util.TypeClass.equality")])


def build_type_scheme(vars: list[TypeVar_], typ: Type):
    constraints = [(tv.name, tv.classes) for tv in vars if tv.classes]
    if not constraints:
        return types.poly([tv.name for tv in vars], typ)
    else:
        return types.poly_constrained([(tv.name, tv.classes) for tv in vars], typ)


# Basic numeric types

def bigfloat() -> TermCoder[Decimal]:
    return TermCoder(
        type=types.bigfloat(),
        encode=lambda cx, g, t: extract.bigfloat(cx, g, t),
        decode=lambda cx, v: Right(terms.bigfloat(float(v)))
    )


def bigint() -> TermCoder[int]:
    return TermCoder(
        type=types.bigint(),
        encode=lambda cx, g, t: extract.bigint(cx, g, t),
        decode=lambda cx, v: Right(terms.bigint(v))
    )


def binary() -> TermCoder[bytes]:
    return TermCoder(
        type=types.binary(),
        encode=lambda cx, g, t: extract.binary(cx, g, t),
        decode=lambda cx, v: Right(terms.binary(v))
    )


def boolean() -> TermCoder[bool]:
    return TermCoder(
        type=types.boolean(),
        encode=lambda cx, g, t: extract.boolean(cx, g, t),
        decode=lambda cx, v: Right(terms.boolean(v))
    )


def float32() -> TermCoder[float]:
    return TermCoder(
        type=types.float32(),
        encode=lambda cx, g, t: extract.float32(cx, g, t),
        decode=lambda cx, v: Right(terms.float32(v))
    )


def float64() -> TermCoder[float]:
    return TermCoder(
        type=types.float64(),
        encode=lambda cx, g, t: extract.float64(cx, g, t),
        decode=lambda cx, v: Right(terms.float64(v))
    )


def int8() -> TermCoder[int]:
    return TermCoder(
        type=types.int8(),
        encode=lambda cx, g, t: extract.int8(cx, g, t),
        decode=lambda cx, v: Right(terms.int8(v))
    )


def int16() -> TermCoder[int]:
    return TermCoder(
        type=types.int16(),
        encode=lambda cx, g, t: extract.int16(cx, g, t),
        decode=lambda cx, v: Right(terms.int16(v))
    )


def int32() -> TermCoder[int]:
    return TermCoder(
        type=types.int32(),
        encode=lambda cx, g, t: extract.int32(cx, g, t),
        decode=lambda cx, v: Right(terms.int32(v))
    )


def int64() -> TermCoder[int]:
    return TermCoder(
        type=types.int64(),
        encode=lambda cx, g, t: extract.int64(cx, g, t),
        decode=lambda cx, v: Right(terms.int64(v))
    )


def uint8() -> TermCoder[int]:
    return TermCoder(
        type=types.uint8(),
        encode=lambda cx, g, t: extract.uint8(cx, g, t),
        decode=lambda cx, v: Right(terms.uint8(v))
    )


def uint16() -> TermCoder[int]:
    return TermCoder(
        type=types.uint16(),
        encode=lambda cx, g, t: extract.uint16(cx, g, t),
        decode=lambda cx, v: Right(terms.uint16(v))
    )


def uint32() -> TermCoder[int]:
    return TermCoder(
        type=types.uint32(),
        encode=lambda cx, g, t: extract.uint32(cx, g, t),
        decode=lambda cx, v: Right(terms.uint32(v))
    )


def uint64() -> TermCoder[int]:
    return TermCoder(
        type=types.uint64(),
        encode=lambda cx, g, t: extract.uint64(cx, g, t),
        decode=lambda cx, v: Right(terms.uint64(v))
    )


def string() -> TermCoder[str]:
    return TermCoder(
        type=types.string(),
        encode=lambda cx, g, t: extract.string(cx, g, t),
        decode=lambda cx, s: Right(terms.string(s))
    )


# Core value types

def comparison() -> TermCoder[Comparison]:
    import hydra.extract.util as extract_util
    return TermCoder(
        type=types.var("Comparison"),
        encode=lambda cx, g, t: extract_util.comparison(cx, g, t),
        decode=lambda cx, c: Right(terms.comparison(c))
    )


def float_type() -> TermCoder[FloatType]:
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    def encode(cx, g, t):
        result = decode_core.float_type(g, t)
        match result:
            case Left(value=err):
                return Left(InContext(object=OtherError(err.value), context=cx))
            case Right(value=v):
                return Right(v)
    return TermCoder(
        type=types.var("FloatType"),
        encode=encode,
        decode=lambda cx, ft: Right(encode_core.float_type(ft))
    )


def float_value() -> TermCoder[FloatValue]:
    return TermCoder(
        type=types.var("FloatValue"),
        encode=lambda cx, g, t: extract.float_value(cx, g, t),
        decode=lambda cx, fv: Right(terms.float_(fv))
    )


def integer_type() -> TermCoder[IntegerType]:
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    def encode(cx, g, t):
        result = decode_core.integer_type(g, t)
        match result:
            case Left(value=err):
                return Left(InContext(object=OtherError(err.value), context=cx))
            case Right(value=v):
                return Right(v)
    return TermCoder(
        type=types.var("IntegerType"),
        encode=encode,
        decode=lambda cx, it: Right(encode_core.integer_type(it))
    )


def integer_value() -> TermCoder[IntegerValue]:
    return TermCoder(
        type=types.var("IntegerValue"),
        encode=lambda cx, g, t: extract.integer_value(cx, g, t),
        decode=lambda cx, iv: Right(terms.integer(iv))
    )


def literal() -> TermCoder[Literal]:
    return TermCoder(
        type=types.var("Literal"),
        encode=lambda cx, g, t: extract.literal(cx, g, t),
        decode=lambda cx, lit: Right(terms.literal(lit))
    )


def literal_type() -> TermCoder[LiteralType]:
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    def encode(cx, g, t):
        result = decode_core.literal_type(g, t)
        match result:
            case Left(value=err):
                return Left(InContext(object=OtherError(err.value), context=cx))
            case Right(value=v):
                return Right(v)
    return TermCoder(
        type=types.var("LiteralType"),
        encode=encode,
        decode=lambda cx, lt: Right(encode_core.literal_type(lt))
    )


def term() -> TermCoder[Term]:
    return TermCoder(
        type=types.var("Term"),
        encode=lambda cx, g, t: Right(t),
        decode=lambda cx, t: Right(t)
    )


def type_() -> TermCoder[Type]:
    import hydra.decode.core as decode_core
    import hydra.encode.core as encode_core
    def encode(cx, g, t):
        result = decode_core.type(g, t)
        match result:
            case Left(value=err):
                return Left(InContext(object=OtherError(err.value), context=cx))
            case Right(value=v):
                return Right(v)
    return TermCoder(
        type=types.var("Type"),
        encode=encode,
        decode=lambda cx, t: Right(encode_core.type(t))
    )


# Container types

def list_(els: TermCoder[X]) -> TermCoder[frozenlist[X]]:
    def encode(cx, g, t):
        return extract.list_of(cx, lambda term: els.encode(cx, g, term), g, t)
    def decode(cx, lst):
        result_items = []
        for item in lst:
            r = els.decode(cx, item)
            match r:
                case Left(_):
                    return r
                case Right(value=v):
                    result_items.append(v)
        return Right(terms.list_(result_items))
    return TermCoder(
        type=types.list_(els.type),
        encode=encode,
        decode=decode
    )


def map_(keys: TermCoder[X], values: TermCoder[Y]) -> TermCoder[FrozenDict[X, Y]]:
    def encode(cx, g, t):
        return extract.map(cx, lambda term: keys.encode(cx, g, term), lambda term: values.encode(cx, g, term), g, t)
    def decode(cx, m):
        pairs = []
        for k, v_ in m.items():
            kr = keys.decode(cx, k)
            match kr:
                case Left(_):
                    return kr
                case Right(value=kv):
                    pass
            vr = values.decode(cx, v_)
            match vr:
                case Left(_):
                    return vr
                case Right(value=vv):
                    pass
            pairs.append((kv, vv))
        return Right(terms.map_(FrozenDict(dict(pairs))))
    return TermCoder(
        type=types.map_(keys.type, values.type),
        encode=encode,
        decode=decode
    )


def optional(mel: TermCoder[X]) -> TermCoder[Maybe[X]]:
    def encode(cx, g, t):
        return extract.maybe_term(cx, lambda term: mel.encode(cx, g, term), g, t)
    def decode(cx, mv):
        match mv:
            case Nothing():
                return Right(terms.optional(Nothing()))
            case Just(value=v):
                r = mel.decode(cx, v)
                match r:
                    case Left(_):
                        return r
                    case Right(value=decoded):
                        return Right(terms.optional(Just(decoded)))
    return TermCoder(
        type=types.optional(mel.type),
        encode=encode,
        decode=decode,
    )


def either(left_coder: TermCoder[X], right_coder: TermCoder[Y]) -> TermCoder[Either[X, Y]]:
    def encode(cx, g, t):
        return extract.either_term(cx, lambda term: left_coder.encode(cx, g, term), lambda term: right_coder.encode(cx, g, term), g, t)
    def decode(cx, ev):
        match ev:
            case Left(value=x):
                r = left_coder.decode(cx, x)
                match r:
                    case Left(_):
                        return r
                    case Right(value=left_term):
                        return Right(terms.left(left_term))
            case Right(value=y):
                r = right_coder.decode(cx, y)
                match r:
                    case Left(_):
                        return r
                    case Right(value=right_term):
                        return Right(terms.right(right_term))
    return TermCoder(
        type=types.either(left_coder.type, right_coder.type),
        encode=encode,
        decode=decode,
    )


def pair(first_coder: TermCoder[X], second_coder: TermCoder[Y]) -> TermCoder[tuple[X, Y]]:
    def encode(cx, g, t):
        return extract.pair(cx, lambda term: first_coder.encode(cx, g, term), lambda term: second_coder.encode(cx, g, term), g, t)
    def decode(cx, p):
        fr = first_coder.decode(cx, p[0])
        match fr:
            case Left(_):
                return fr
            case Right(value=first_term):
                pass
        sr = second_coder.decode(cx, p[1])
        match sr:
            case Left(_):
                return sr
            case Right(value=second_term):
                pass
        return Right(terms.pair(first_term, second_term))
    return TermCoder(
        type=types.pair(first_coder.type, second_coder.type),
        encode=encode,
        decode=decode
    )


def set_(els: TermCoder[X]) -> TermCoder[frozenset[X]]:
    def encode(cx, g, t):
        return extract.set_of(cx, lambda term: els.encode(cx, g, term), g, t)
    def decode(cx, s):
        result_items = []
        for item in list(s):
            r = els.decode(cx, item)
            match r:
                case Left(_):
                    return r
                case Right(value=v):
                    result_items.append(v)
        return Right(terms.set_(set(result_items)))
    return TermCoder(
        type=types.set_(els.type),
        encode=encode,
        decode=decode
    )


# Special types

def function(dom: TermCoder[X], cod: TermCoder[Y]) -> TermCoder[Callable[[X], Y]]:
    """TermCoder for function values (not actually encodable/decodable)."""
    return TermCoder(
        type=types.function(dom.type, cod.type),
        encode=lambda cx, g, t: Left(other_err(cx, f"cannot encode term to a function: {t}")),
        decode=lambda cx, v: Left(other_err(cx, "cannot decode functions to terms"))
    )


def function_with_reduce(
        reduce: Callable,
        dom: TermCoder[X],
        cod: TermCoder[Y],
) -> TermCoder[Callable[[X], Y]]:
    """TermCoder for function types, using a reducer to bridge term-level functions to native functions.

    The reduce parameter should be (cx, g, term) -> Either[InContext[OtherError], Term].
    """
    def encode(cx, g, fun_term):
        def native_fun(x):
            arg_result = dom.decode(cx, x)
            match arg_result:
                case Left(_):
                    raise RuntimeError("function_with_reduce: failed to encode argument")
                case Right(value=arg_term):
                    pass
            reduce_result = reduce(cx, g, TermApplication(Application(fun_term, arg_term)))
            match reduce_result:
                case Left(_):
                    raise RuntimeError("function_with_reduce: failed to reduce application")
                case Right(value=result_term):
                    pass
            decode_result = cod.encode(cx, g, result_term)
            match decode_result:
                case Left(_):
                    raise RuntimeError("function_with_reduce: failed to decode result")
                case Right(value=result):
                    return result
        return Right(native_fun)

    return TermCoder(
        type=types.function(dom.type, cod.type),
        encode=encode,
        decode=lambda cx, v: Left(other_err(cx, "cannot decode functions to terms"))
    )


def variable(v: str) -> TermCoder[Term]:
    """TermCoder for type variables."""
    return TermCoder(
        type=types.var(v),
        encode=lambda cx, g, t: Right(t),
        decode=lambda cx, t: Right(t)
    )


# Primitive constructors

def prim0(
        name: Name, value: Callable[[], A], variables: list[str], output: TermCoder[A]
) -> Primitive:
    """Create a 0-argument primitive function."""
    def impl(cx: Context, g: Graph, args: frozenlist[Term]) -> Either[InContext[Error], Term]:
        result = output.decode(cx, value())
        return wrap_other(cx, result)

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
    def impl(cx: Context, g: Graph, args: frozenlist[Term]) -> Either[InContext[Error], Term]:
        def go():
            r = extract.n_args(cx, name, 1, args)
            match r:
                case Left(value=ic):
                    return Left(ic)
                case Right(_):
                    pass
            r1 = input1.encode(cx, g, args[0])
            match r1:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg1):
                    pass
            return output.decode(cx, compute(arg1))
        return wrap_other(cx, go())

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
    def impl(cx: Context, g: Graph, args: frozenlist[Term]) -> Either[InContext[Error], Term]:
        def go():
            r = extract.n_args(cx, name, 2, args)
            match r:
                case Left(value=ic):
                    return Left(ic)
                case Right(_):
                    pass
            r1 = input1.encode(cx, g, args[0])
            match r1:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg1):
                    pass
            r2 = input2.encode(cx, g, args[1])
            match r2:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg2):
                    pass
            return output.decode(cx, compute(arg1, arg2))
        return wrap_other(cx, go())

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
    def impl(cx: Context, g: Graph, args: frozenlist[Term]) -> Either[InContext[Error], Term]:
        def go():
            r = extract.n_args(cx, name, 3, args)
            match r:
                case Left(value=ic):
                    return Left(ic)
                case Right(_):
                    pass
            r1 = input1.encode(cx, g, args[0])
            match r1:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg1):
                    pass
            r2 = input2.encode(cx, g, args[1])
            match r2:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg2):
                    pass
            r3 = input3.encode(cx, g, args[2])
            match r3:
                case Left(value=ic):
                    return Left(ic)
                case Right(value=arg3):
                    pass
            return output.decode(cx, compute(arg1, arg2, arg3))
        return wrap_other(cx, go())

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


