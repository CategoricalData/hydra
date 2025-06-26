"""Python implementations of hydra.lib.flows primitives. These are simply wrappers around hydra.flows functions."""

from __future__ import annotations

from collections.abc import Callable
from typing import TypeVar

from hydra.compute import Flow, FlowState, Trace
from hydra.core import Unit
from hydra.dsl.python import FrozenDict

A = TypeVar("A")
B = TypeVar("B")
S = TypeVar("S")


EMPTY_TRACE = Trace(
    tuple(),
    tuple(),
    FrozenDict(),
)

MAX_MAPM_SIZE = 1000

UNIT = Unit()


def pure[S, A](value: A, meaningless: S | None = None) -> Flow[S, A]:
    """Lift a value into the Flow monad."""

    def run(state: S, trace: Trace) -> FlowState[S, A]:
        return FlowState[S, A](value=value, state=state, trace=trace)

    return Flow[S, A](run)


def bind[S, A, B](flow: Flow[S, A], f: Callable[[A], Flow[S, B]]) -> Flow[S, B]:
    """Monadic bind function for flows."""

    def run(state: S, trace: Trace) -> FlowState[S, B]:
        flow_state = flow.value(state, trace)

        return (
            FlowState[S, B](
                value=None,
                state=state,
                trace=trace,
            )
            if flow_state.value is None
            else f(flow_state.value).value(flow_state.state, flow_state.trace)
        )

    return Flow(run)


def apply[S, A, B](flow: Flow[S, Callable[[A], B]], x: Flow[S, A]) -> Flow[S, B]:
    """Apply a function flow to a domain value flow."""

    def run(state: S, trace: Trace) -> FlowState[S, B]:
        flow_state = flow.value(state, trace)

        return (
            FlowState[S, B](
                value=None,
                state=state,
                trace=trace,
            )
            if flow_state.value is None
            else map_(flow_state.value, x).value(flow_state.state, flow_state.trace)
        )

    return Flow(run)


def map_[S, A, B](f: Callable[[A], B], x: Flow[S, A]) -> Flow[S, B]:
    """Map a function over a flow."""

    def run(state: S, trace: Trace) -> FlowState[S, B]:
        flow_state = x.value(state, trace)
        return FlowState[S, B](
            value=f(flow_state.value) if flow_state.value is not None else None,
            state=flow_state.state,
            trace=flow_state.trace,
        )

    return Flow[S, B](run)


def compose[S, A, B, C](
    f: Callable[[A], Flow[S, B]], g: Callable[[B], Flow[S, C]]
) -> Callable[[A], Flow[S, C]]:
    """Compose two monadic functions, feeding the output of the first into the second."""

    def run(a: A) -> Flow[S, C]:
        return bind(f(a), g)

    return run


def consume[S, A](flow: Flow[S, A], consumer: Callable[[A], None]) -> Flow[S, Unit]:
    """Evaluate a flow and consume the result."""

    def f(a: A) -> Unit:
        consumer(a)
        return Unit()

    return map_(f, flow)


def fail[S, A](msg: str, meaningless: S | A | None = None) -> Flow[S, A]:
    """Produce a failure flow with the provided message."""

    def run(state: S, trace: Trace) -> FlowState[S, A]:
        return FlowState[S, A](
            value=None,
            state=state,
            trace=Trace(
                stack=trace.stack,
                messages=trace.messages + tuple([msg]),
                other=trace.other,
            ),
        )

    return Flow[S, A](run)


def fail_with_exception[S, A](
    msg: str, cause: Exception, meaningless: S | A | None = None
) -> Flow[S, A]:
    """Produce a failure flow with the provided message and additional information from a Throwable."""
    return fail(msg + ": " + str(cause))


def check[A](x: A, *predicates: Callable[[A], str | None]) -> Flow[Unit, A]:
    """Check whether a given value satisfies a list of predicates.

    Returns the value itself if all checks are successful, or a
    failure flow for the first predicate that fails.
    """
    for predicate in predicates:
        msg = predicate(x)
        if msg is not None:
            return fail(msg)

    return pure(x)


def from_flow[S, A](state: S, flow: Flow[S, A]) -> A:
    """Extract the value from a flow, throwing an exception if the flow failed."""
    flow_state = flow.value(state, EMPTY_TRACE)
    if flow_state.value is None:
        raise Exception(flow_state.trace)
    return flow_state.value


def get_state[S]() -> Flow[S, S]:
    """Get the current state."""

    def run(state: S, trace: Trace) -> FlowState[S, S]:
        return FlowState[S, S](value=state, state=state, trace=trace)

    return Flow[S, S](run)


# Alias
map = map_
