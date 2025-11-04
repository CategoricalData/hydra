"""Python implementations of hydra.lib.flows primitives."""

from collections.abc import Callable, Mapping, Sequence
from typing import Any

from hydra.compute import Flow, FlowState, Trace
from hydra.dsl.python import FrozenDict, frozenlist, Maybe, Just, Nothing, NOTHING


def apply[S, X, Y](f: Flow[S, Callable[[X], Y]], x: Flow[S, X]) -> Flow[S, Y]:
    """Apply a function in a Flow to a value in a Flow (applicative)."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        f_state = f.value(state, trace)

        match f_state.value:
            case Nothing():
                return FlowState(value=NOTHING, state=f_state.state, trace=f_state.trace)
            case Just(func):
                x_state = x.value(f_state.state, f_state.trace)

                match x_state.value:
                    case Nothing():
                        return FlowState(value=NOTHING, state=x_state.state, trace=x_state.trace)
                    case Just(val):
                        result = func(val)
                        return FlowState(value=Just(result), state=x_state.state, trace=x_state.trace)

    return Flow(run)


def bind[S, X, Y](mx: Flow[S, X], f: Callable[[X], Flow[S, Y]]) -> Flow[S, Y]:
    """Monadic bind for Flow."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        x_state = mx.value(state, trace)

        match x_state.value:
            case Nothing():
                return FlowState(value=NOTHING, state=x_state.state, trace=x_state.trace)
            case Just(x):
                my = f(x)
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
        return FlowState(value=NOTHING, state=state, trace=new_trace)

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

            match result_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=result_state.state, trace=result_state.trace)
                case Just(val):
                    current_value = val
                    current_state = result_state.state
                    current_trace = result_state.trace

        return FlowState(value=Just(current_value), state=current_state, trace=current_trace)

    return Flow(run)


def map[S, X, Y](f: Callable[[X], Y], mx: Flow[S, X]) -> Flow[S, Y]:
    """Map a function over a Flow (functor)."""
    def run(state: S, trace: Trace) -> FlowState[S, Y]:
        x_state = mx.value(state, trace)

        match x_state.value:
            case Nothing():
                return FlowState(value=NOTHING, state=x_state.state, trace=x_state.trace)
            case Just(x):
                result = f(x)
                return FlowState(value=Just(result), state=x_state.state, trace=x_state.trace)

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

            match v2_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=v2_state.state, trace=v2_state.trace)
                case Just(v2):
                    result_dict[k] = v2
                    current_state = v2_state.state
                    current_trace = v2_state.trace

        return FlowState(value=Just(FrozenDict(result_dict)), state=current_state, trace=current_trace)

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

            match k2_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=k2_state.state, trace=k2_state.trace)
                case Just(k2):
                    result_dict[k2] = v
                    current_state = k2_state.state
                    current_trace = k2_state.trace

        return FlowState(value=Just(FrozenDict(result_dict)), state=current_state, trace=current_trace)

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

            match y_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=y_state.state, trace=y_state.trace)
                case Just(y):
                    results.append(y)
                    current_state = y_state.state
                    current_trace = y_state.trace

        return FlowState(value=Just(tuple(results)), state=current_state, trace=current_trace)

    return Flow(run)


def map_maybe[S, X, Y](f: Callable[[X], Flow[S, Y]], mx: Maybe[X]) -> Flow[S, Maybe[Y]]:
    """Map a monadic function over an optional value."""
    def run(state: S, trace: Trace) -> FlowState[S, Maybe[Y]]:
        match mx:
            case Nothing():
                # Input is Nothing, succeed with Nothing as result
                # This properly represents "Just Nothing" - a successful computation with no value
                return FlowState(value=Just(NOTHING), state=state, trace=trace)
            case Just(x):
                # Input is Just x, apply the function
                flow_result = f(x)
                y_state = flow_result.value(state, trace)

                match y_state.value:
                    case Nothing():
                        # Flow failed - return Nothing (not Just(Nothing))
                        return FlowState(value=NOTHING, state=y_state.state, trace=y_state.trace)
                    case Just(y):
                        # Flow succeeded with value - wrap in Just(Just(...))
                        return FlowState(value=Just(Just(y)), state=y_state.state, trace=y_state.trace)

    return Flow(run)


def map_set[S, X, Y](f: Callable[[X], Flow[S, Y]], xs: frozenset[X]) -> Flow[S, frozenset[Y]]:
    """Map a monadic function over a set."""
    def run(state: S, trace: Trace) -> FlowState[S, frozenset[Y]]:
        current_state = state
        current_trace = trace
        results: list[Y] = []

        for x in xs:
            flow_result = f(x)
            y_state = flow_result.value(current_state, current_trace)

            match y_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=y_state.state, trace=y_state.trace)
                case Just(y):
                    results.append(y)
                    current_state = y_state.state
                    current_trace = y_state.trace

        return FlowState(value=Just(frozenset(results)), state=current_state, trace=current_trace)

    return Flow(run)


def pure[X](x: X) -> Flow[Any, X]:
    """Lift a value into a Flow."""
    def run(state: Any, trace: Trace) -> FlowState[Any, X]:
        return FlowState(value=Just(x), state=state, trace=trace)

    return Flow(run)


def sequence[S, X](flows: Sequence[Flow[S, X]]) -> Flow[S, frozenlist[X]]:
    """Sequence a list of Flows into a Flow of a list."""
    def run(state: S, trace: Trace) -> FlowState[S, frozenlist[X]]:
        current_state = state
        current_trace = trace
        results: list[X] = []

        for flow in flows:
            x_state = flow.value(current_state, current_trace)

            match x_state.value:
                case Nothing():
                    return FlowState(value=NOTHING, state=x_state.state, trace=x_state.trace)
                case Just(x):
                    results.append(x)
                    current_state = x_state.state
                    current_trace = x_state.trace

        return FlowState(value=Just(tuple(results)), state=current_state, trace=current_trace)

    return Flow(run)
