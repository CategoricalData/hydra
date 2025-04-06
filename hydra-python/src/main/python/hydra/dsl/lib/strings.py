"""Primitive functions for the Hydra strings DSL."""

from collections.abc import Callable
from typing import override

import hydra.core
import hydra.dsl.terms as terms
import hydra.dsl.types as types
from hydra.compute import Flow, FlowState, Trace
from hydra.core import Name, Term, TypeScheme
from hydra.dsl.python import frozenlist
from hydra.graph import Graph
from hydra.tools import PrimitiveFunction


class Length(PrimitiveFunction):
    """A primitive function that returns the length of a string."""

    @override
    def name(self) -> Name:
        return Name("hydra.lib.strings.length")

    @override
    def type(self) -> TypeScheme:
        return types.scheme(
            variables=["s"],
            body=types.function(types.string(), types.int32()),
        )

    @override
    def implementation(self) -> Callable[[frozenlist[Term]], Flow[Graph, Term]]:
        def func(args: frozenlist[Term]) -> Flow[Graph, Term]:
            def run(graph: Graph, trace: Trace) -> FlowState[Graph, Term]:
                if len(args) == 0:
                    return FlowState(
                        value=None,
                        state=graph,
                        trace=Trace(
                            stack=trace.stack,
                            messages=trace.messages
                            + tuple(["Expected string argument"]),
                            other=trace.other,
                        ),
                    )

                term = args[0]

                match term:
                    case hydra.core.TermLiteral(value):
                        match value:
                            case hydra.core.LiteralString(value):
                                return FlowState(
                                    value=terms.int32(len(value.value)),
                                    state=graph,
                                    trace=trace,
                                )
                            case _:
                                return FlowState(
                                    value=None,
                                    state=graph,
                                    trace=Trace(
                                        stack=trace.stack,
                                        messages=trace.messages
                                        + tuple(["Literal is not a string."]),
                                        other=trace.other,
                                    ),
                                )
                    case _:
                        return FlowState(
                            value=None,
                            state=graph,
                            trace=Trace(
                                stack=trace.stack,
                                messages=trace.messages
                                + tuple(["Term is not a literal."]),
                                other=trace.other,
                            ),
                        )

            return Flow(run)

        return func
