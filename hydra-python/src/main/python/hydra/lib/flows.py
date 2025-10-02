"""Python implementations of hydra.lib.flows primitives."""

from collections.abc import Callable, Mapping, Sequence
from typing import Any, TypeVar

from hydra.compute import Flow, FlowState, Trace
from hydra.dsl.python import FrozenDict, frozenlist

S = TypeVar('S')
S1 = TypeVar('S1')
S2 = TypeVar('S2')
X = TypeVar('X')
Y = TypeVar('Y')
A = TypeVar('A')
B = TypeVar('B')
K = TypeVar('K')
K1 = TypeVar('K1')
K2 = TypeVar('K2')
V1 = TypeVar('V1')
V2 = TypeVar('V2')


def apply[S, X, Y](f: Flow[S, Callable[[X], Y]], x: Flow[S, X]) -> Flow[S, Y]:
    """Apply a function in a Flow to a value in a Flow (applicative)."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        f_state = f.value(state, trace)
        if f_state.value is None:
            return FlowState(value=None, state=f_state.state, trace=f_state.trace)

        x_state = x.value(f_state.state, f_state.trace)
        if x_state.value is None:
            return FlowState(value=None, state=x_state.state, trace=x_state.trace)

        result = f_state.value(x_state.value)
        return FlowState(value=result, state=x_state.state, trace=x_state.trace)

    return Flow(run)


def bind[S, X, Y](mx: Flow[S, X], f: Callable[[X], Flow[S, Y]]) -> Flow[S, Y]:
    """Monadic bind for Flow."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        x_state = mx.value(state, trace)
        if x_state.value is None:
            return FlowState(value=None, state=x_state.state, trace=x_state.trace)

        my = f(x_state.value)
        return my.value(x_state.state, x_state.trace)

    return Flow(run)


def fail(message: str) -> Flow[Any, Any]:
    """Create a failed Flow with an error message."""
    def run(state: Any, trace: Trace) -> FlowState[Any, Any]:
        new_messages = tuple(list(trace.messages) + [message])
        new_trace = Trace(
            stack=trace.stack,
            messages=new_messages,
            other=trace.other
        )
        return FlowState(value=None, state=state, trace=new_trace)

    return Flow(run)


def foldl[S, A, B](f: Callable[[A, B], Flow[S, A]], initial: A, values: Sequence[B]) -> Flow[S, A]:
    """Fold over a list with a monadic function."""
    def run(state: S, trace: Trace) -> FlowState[S, A]:
        current_state = state
        current_trace = trace
        current_value = initial

        for b in values:
            flow_result = f(current_value, b)
            result_state = flow_result.value(current_state, current_trace)

            if result_state.value is None:
                return FlowState(value=None, state=result_state.state, trace=result_state.trace)

            current_value = result_state.value
            current_state = result_state.state
            current_trace = result_state.trace

        return FlowState(value=current_value, state=current_state, trace=current_trace)

    return Flow(run)


def map[S, X, Y](f: Callable[[X], Y], mx: Flow[S, X]) -> Flow[S, Y]:
    """Map a function over a Flow (functor)."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        x_state = mx.value(state, trace)
        if x_state.value is None:
            return FlowState(value=None, state=x_state.state, trace=x_state.trace)

        result = f(x_state.value)
        return FlowState(value=result, state=x_state.state, trace=x_state.trace)

    return Flow(run)


def map_elems[S, K, V1, V2](f: Callable[[V1], Flow[S, V2]], m: Mapping[K, V1]) -> Flow[S, FrozenDict[K, V2]]:
    """Map a monadic function over the values of a map."""
    def run(state: S, trace: Trace) -> FlowState[S, FrozenDict[K, V2]]:
        current_state = state
        current_trace = trace
        result_dict: dict[K, V2] = {}

        for k, v1 in m.items():
            flow_result = f(v1)
            v2_state = flow_result.value(current_state, current_trace)

            if v2_state.value is None:
                return FlowState(value=None, state=v2_state.state, trace=v2_state.trace)

            result_dict[k] = v2_state.value
            current_state = v2_state.state
            current_trace = v2_state.trace

        return FlowState(value=FrozenDict(result_dict), state=current_state, trace=current_trace)

    return Flow(run)


def map_keys[S, K1, K2, V](f: Callable[[K1], Flow[S, K2]], m: Mapping[K1, V]) -> Flow[S, FrozenDict[K2, V]]:
    """Map a monadic function over the keys of a map."""
    def run(state: S, trace: Trace) -> FlowState[S, FrozenDict[K2, V]]:
        current_state = state
        current_trace = trace
        result_dict: dict[K2, V] = {}

        for k1, v in m.items():
            flow_result = f(k1)
            k2_state = flow_result.value(current_state, current_trace)

            if k2_state.value is None:
                return FlowState(value=None, state=k2_state.state, trace=k2_state.trace)

            result_dict[k2_state.value] = v
            current_state = k2_state.state
            current_trace = k2_state.trace

        return FlowState(value=FrozenDict(result_dict), state=current_state, trace=current_trace)

    return Flow(run)


def map_list[S, X, Y](f: Callable[[X], Flow[S, Y]], xs: Sequence[X]) -> Flow[S, frozenlist[Y]]:
    """Map a monadic function over a list."""
    def run(state: S, trace: Trace) -> FlowState[S, frozenlist[Y]]:
        current_state = state
        current_trace = trace
        results: list[Y] = []

        for x in xs:
            flow_result = f(x)
            y_state = flow_result.value(current_state, current_trace)

            if y_state.value is None:
                return FlowState(value=None, state=y_state.state, trace=y_state.trace)

            results.append(y_state.value)
            current_state = y_state.state
            current_trace = y_state.trace

        return FlowState(value=tuple(results), state=current_state, trace=current_trace)

    return Flow(run)


# TODO: map_optional highlights an incompatibility between X|None in Python, and Hydra's optional<x>.
# def map_optional[S, X, Y](f: Callable[[X], Flow[S, Y]], mx: X | None) -> Flow[S, Y | None]:
#     """Map a monadic function over an optional value."""
#     def run(state: S, trace: Trace) -> FlowState[S, Y | None]:
#         if mx is None:
#             # Input is Nothing, succeed with Nothing as result
#             # Since Python can't distinguish Maybe (Maybe Y), we return the Flow's
#             # value field as None, which represents "Just Nothing" in this context
#             # This is semantically different from a failed Flow
#             return FlowState(value=None, state=state, trace=trace)
#
#         # Input is Just x, apply the function
#         flow_result = f(mx)
#         return flow_result.value(state, trace)
#
#     return Flow(run)


def map_set[S, X, Y](f: Callable[[X], Flow[S, Y]], xs: frozenset[X]) -> Flow[S, frozenset[Y]]:
    """Map a monadic function over a set."""
    def run(state: S, trace: Trace) -> FlowState[S, frozenset[Y]]:
        current_state = state
        current_trace = trace
        results: list[Y] = []

        for x in xs:
            flow_result = f(x)
            y_state = flow_result.value(current_state, current_trace)

            if y_state.value is None:
                return FlowState(value=None, state=y_state.state, trace=y_state.trace)

            results.append(y_state.value)
            current_state = y_state.state
            current_trace = y_state.trace

        return FlowState(value=frozenset(results), state=current_state, trace=current_trace)

    return Flow(run)


def pure[X](x: X) -> Flow[Any, X]:
    """Lift a value into a Flow."""
    def run(state: Any, trace: Trace) -> FlowState[Any, X]:
        return FlowState(value=x, state=state, trace=trace)

    return Flow(run)


def sequence[S, X](flows: Sequence[Flow[S, X]]) -> Flow[S, frozenlist[X]]:
    """Sequence a list of Flows into a Flow of a list."""
    def run(state: S, trace: Trace) -> FlowState[S, frozenlist[X]]:
        current_state = state
        current_trace = trace
        results: list[X] = []

        for flow in flows:
            x_state = flow.value(current_state, current_trace)

            if x_state.value is None:
                return FlowState(value=None, state=x_state.state, trace=x_state.trace)

            results.append(x_state.value)
            current_state = x_state.state
            current_trace = x_state.trace

        return FlowState(value=tuple(results), state=current_state, trace=current_trace)

    return Flow(run)
