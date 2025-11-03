"""Python implementations of primitive construction utilities following the Haskell pattern."""

from collections.abc import Callable
from typing import TypeVar

import hydra.dsl.flows as flows
import hydra.dsl.terms as terms
import hydra.dsl.types as types
from hydra.compute import Coder, Flow
from hydra.core import Name, Term
from hydra.dsl.expect import Expect
from hydra.dsl.python import frozenlist
from hydra.graph import Graph, Primitive, TermCoder

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
D = TypeVar("D")


def string_coder() -> TermCoder[str]:
    """TermCoder for strings."""
    return TermCoder(
        type=types.string(),
        coder=Coder(
            encode=Expect[Graph].string, decode=lambda s: flows.pure(terms.string(s))
        ),
    )


def int32_coder() -> TermCoder[int]:
    """TermCoder for 32-bit integers."""
    return TermCoder(
        type=types.int32(),
        coder=Coder(
            encode=lambda term: flows.fail(
                "int32 expect not implemented"
            ),  # Placeholder
            decode=lambda i: flows.pure(terms.int32(i)),
        ),
    )


def prim0(
    name: Name, value: A, variables: list[str], output: TermCoder[A]
) -> Primitive:
    """Create a 0-argument primitive function."""

    def impl(args: frozenlist[Term]) -> Flow[Graph, Term]:
        return output.coder.decode(value)

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
        if len(args) != 1:
            return flows.fail(f"Expected 1 argument for {name}, got {len(args)}")

        def func(arg1: A) -> Flow[Graph, Term]:
            result = compute(arg1)
            return output.coder.decode(result)

        return flows.bind(input1.coder.encode(args[0]), func)

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
        if len(args) != 2:
            return flows.fail(f"Expected 2 arguments for {name}, got {len(args)}")

        # Chain the encode operations with bind
        arg1_flow = input1.coder.encode(args[0])

        def process_arg1(arg1: A) -> Flow[Graph, Term]:
            arg2_flow = input2.coder.encode(args[1])

            def process_arg2(arg2: B) -> Flow[Graph, Term]:
                result = compute(arg1, arg2)
                return output.coder.decode(result)

            return flows.bind(arg2_flow, process_arg2)

        return flows.bind(arg1_flow, process_arg1)

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
        if len(args) != 3:
            return flows.fail(f"Expected 3 arguments for {name}, got {len(args)}")

        # Chain all three encode operations with bind
        arg1_flow = input1.coder.encode(args[0])

        def process_arg1(arg1: A) -> Flow[Graph, Term]:
            arg2_flow = input2.coder.encode(args[1])

            def process_arg2(arg2: B) -> Flow[Graph, Term]:
                arg3_flow = input3.coder.encode(args[2])

                def process_arg3(arg3: C) -> Flow[Graph, Term]:
                    result = compute(arg1, arg2, arg3)
                    return output.coder.decode(result)

                return flows.bind(arg3_flow, process_arg3)

            return flows.bind(arg2_flow, process_arg2)

        return flows.bind(arg1_flow, process_arg1)

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
