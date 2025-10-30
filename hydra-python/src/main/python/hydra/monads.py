# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with Hydra's 'flow' and other monads."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
import hydra.compute
import hydra.constants
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.strings
import hydra.mantle
import hydra.show.core

def bind[T0, T1, T2](l: hydra.compute.Flow[T0, T1], r: Callable[[T1], hydra.compute.Flow[T0, T2]]) -> hydra.compute.Flow[T0, T2]:
    def q(s0: T0, t0: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T2]:
        fs1 = l.value(s0, t0)
        return hydra.lib.optionals.maybe(cast(hydra.compute.FlowState[T0, T2], hydra.compute.FlowState(cast(Maybe[T2], Nothing()), fs1.state, fs1.trace)), (lambda v: r(v).value(fs1.state, fs1.trace)), fs1.value)
    return cast(hydra.compute.Flow[T0, T2], hydra.compute.Flow(q))

empty_trace = hydra.compute.Trace(cast(frozenlist[str], ()), cast(frozenlist[str], ()), cast(FrozenDict[hydra.core.Name, hydra.core.Term], hydra.lib.maps.empty()))

def exec[T0, T1](f: hydra.compute.Flow[T0, T1], s0: T0) -> T0:
    return f.value(s0, empty_trace).state

def push_error(msg: str, t: hydra.compute.Trace) -> hydra.compute.Trace:
    r"""Push an error message."""
    
    def condense_repeats(ys: frozenlist[str]) -> frozenlist[str]:
        def condense_group(xs: frozenlist[str]) -> str:
            x = hydra.lib.lists.head(xs)
            n = hydra.lib.lists.length(xs)
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(n, 1), x, hydra.lib.strings.cat((x, " (x", hydra.lib.literals.show_int32(n), ")")))
        return hydra.lib.lists.map(condense_group, hydra.lib.lists.group(ys))
    error_msg = hydra.lib.strings.cat(("Error: ", msg, " (", hydra.lib.strings.intercalate(" > ", condense_repeats(hydra.lib.lists.reverse(t.stack))), ")"))
    return hydra.compute.Trace(t.stack, hydra.lib.lists.cons(error_msg, t.messages), t.other)

def fail[T0, T1](msg: str) -> hydra.compute.Flow[T0, T1]:
    return cast(hydra.compute.Flow[T0, T1], hydra.compute.Flow((lambda s, t: cast(hydra.compute.FlowState[T0, T1], hydra.compute.FlowState(cast(Maybe[T1], Nothing()), s, push_error(msg, t))))))

def flow_succeeds[T0, T1](s: T0, f: hydra.compute.Flow[T0, T1]) -> bool:
    return hydra.lib.optionals.is_just(f.value(s, empty_trace).value)

def from_flow[T0, T1](def_: T0, cx: T1, f: hydra.compute.Flow[T1, T0]) -> T0:
    return hydra.lib.optionals.maybe(def_, (lambda xmo: xmo), f.value(cx, empty_trace).value)

def pure[T0, T1](xp: T0) -> hydra.compute.Flow[T1, T0]:
    return cast(hydra.compute.Flow[T1, T0], hydra.compute.Flow((lambda s, t: cast(hydra.compute.FlowState[T1, T0], hydra.compute.FlowState(cast(Maybe[T0], Just(xp)), s, t)))))

def get_state[T0]() -> hydra.compute.Flow[T0, T0]:
    return cast(hydra.compute.Flow[T0, T0], hydra.compute.Flow((lambda s0, t0: (fs1 := pure(None).value(s0, t0), v := fs1.value, s := fs1.state, t := fs1.trace, hydra.lib.optionals.maybe(cast(hydra.compute.FlowState[T0, T0], hydra.compute.FlowState(cast(Maybe[T0], Nothing()), s, t)), (lambda _: cast(hydra.compute.FlowState[T0, T0], hydra.compute.FlowState(cast(Maybe[T0], Just(s)), s, t))), v))[4])))

def map[T0, T1, T2](f: Callable[[T0], T1], f1: hydra.compute.Flow[T2, T0]) -> hydra.compute.Flow[T2, T1]:
    return cast(hydra.compute.Flow[T2, T1], hydra.compute.Flow((lambda s0, t0: (f2 := f1.value(s0, t0), cast(hydra.compute.FlowState[T2, T1], hydra.compute.FlowState(hydra.lib.optionals.map(f, f2.value), f2.state, f2.trace)))[1])))

def put_state[T0](cx: T0) -> hydra.compute.Flow[T0, None]:
    return cast(hydra.compute.Flow[T0, None], hydra.compute.Flow((lambda s0, t0: (f1 := pure(None).value(s0, t0), cast(hydra.compute.FlowState[T0, None], hydra.compute.FlowState(f1.value, cx, f1.trace)))[1])))

def modify[T0](f: Callable[[T0], T0]) -> hydra.compute.Flow[T0, None]:
    return bind(cast(hydra.compute.Flow[T0, T0], get_state), (lambda s: put_state(f(s))))

def mutate_trace[T0, T1](mutate: Callable[[hydra.compute.Trace], hydra.mantle.Either[str, hydra.compute.Trace]], restore: Callable[[hydra.compute.Trace, hydra.compute.Trace], hydra.compute.Trace], f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def choose[T2, T3, T4](for_left: Callable[[T2], T3], for_right: Callable[[T4], T3], e: hydra.mantle.Either[T2, T4]) -> T3:
        match e:
            case hydra.mantle.EitherLeft(value=e2):
                return for_left(e2)
            
            case hydra.mantle.EitherRight(value=e22):
                return for_right(e22)
    def flow_fun(s0: T0, t0: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T1]:
        def for_left[T2](msg: str) -> hydra.compute.FlowState[T0, T2]:
            return cast(hydra.compute.FlowState[T0, T2], hydra.compute.FlowState(cast(Maybe[T2], Nothing()), s0, push_error(msg, t0)))
        def for_right(t1: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T1]:
            f2 = f.value(s0, t1)
            return cast(hydra.compute.FlowState[T0, T1], hydra.compute.FlowState(f2.value, f2.state, restore(t0, f2.trace)))
        return choose(cast(Callable[[str], hydra.compute.FlowState[T0, T1]], for_left), for_right, mutate(t0))
    return cast(hydra.compute.Flow[T0, T1], hydra.compute.Flow(flow_fun))

def optional_to_list[T0](mx: Maybe[T0]) -> frozenlist[T0]:
    return hydra.lib.optionals.maybe(cast(frozenlist[T0], ()), cast(Callable[[T0], frozenlist[T0]], hydra.lib.lists.pure), mx)

def trace_summary(t: hydra.compute.Trace) -> str:
    r"""Summarize a trace as a string."""
    
    message_lines = hydra.lib.lists.nub(t.messages)
    def to_line(pair: Tuple[hydra.core.Name, hydra.core.Term]) -> str:
        return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("\t", pair[0].value)), ": ")), hydra.show.core.term(pair[1])))
    keyval_lines = hydra.lib.logic.if_else(hydra.lib.maps.null(t.other), cast(frozenlist[str], ()), hydra.lib.lists.cons("key/value pairs: ", hydra.lib.lists.map(to_line, hydra.lib.maps.to_list(t.other))))
    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat2(message_lines, keyval_lines))

def unexpected[T0, T1](expected: str, actual: str) -> hydra.compute.Flow[T0, T1]:
    return fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("expected ", expected)), " but found: ")), actual)))

def warn[T0, T1](msg: str, b: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    return cast(hydra.compute.Flow[T0, T1], hydra.compute.Flow((lambda s0, t0: (f1 := b.value(s0, t0), add_message := (lambda t: hydra.compute.Trace(t.stack, hydra.lib.lists.cons(hydra.lib.strings.cat(("Warning: ", msg)), t.messages), t.other)), cast(hydra.compute.FlowState[T0, T1], hydra.compute.FlowState(f1.value, f1.state, add_message(f1.trace))))[2])))

def with_flag[T0, T1](flag: hydra.core.Name, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def mutate(t: hydra.compute.Trace) -> hydra.mantle.Either[str, hydra.compute.Trace]:
        return hydra.lib.logic.if_else(False, cast(hydra.mantle.Either[str, hydra.compute.Trace], cast(hydra.mantle.Either, hydra.mantle.EitherLeft("never happens"))), cast(hydra.mantle.Either[str, hydra.compute.Trace], cast(hydra.mantle.Either, hydra.mantle.EitherRight(hydra.compute.Trace(t.stack, t.messages, hydra.lib.maps.insert(flag, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))), t.other))))))
    def restore[T2](ignored: T2, t1: hydra.compute.Trace) -> hydra.compute.Trace:
        return hydra.compute.Trace(t1.stack, t1.messages, hydra.lib.maps.remove(flag, t1.other))
    return mutate_trace(mutate, cast(Callable[[hydra.compute.Trace, hydra.compute.Trace], hydra.compute.Trace], restore), f)

def with_state[T0, T1, T2](cx0: T0, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T2, T1]:
    return cast(hydra.compute.Flow[T2, T1], hydra.compute.Flow((lambda cx1, t1: (f1 := f.value(cx0, t1), cast(hydra.compute.FlowState[T2, T1], hydra.compute.FlowState(f1.value, cx1, f1.trace)))[1])))

def with_trace[T0, T1](msg: str, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def mutate(t: hydra.compute.Trace) -> hydra.mantle.Either[str, hydra.compute.Trace]:
        return hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(t.stack), hydra.constants.max_trace_depth), cast(hydra.mantle.Either[str, hydra.compute.Trace], cast(hydra.mantle.Either, hydra.mantle.EitherLeft("maximum trace depth exceeded. This may indicate an infinite loop"))), cast(hydra.mantle.Either[str, hydra.compute.Trace], cast(hydra.mantle.Either, hydra.mantle.EitherRight(hydra.compute.Trace(hydra.lib.lists.cons(msg, t.stack), t.messages, t.other)))))
    def restore(t0: hydra.compute.Trace, t1: hydra.compute.Trace) -> hydra.compute.Trace:
        return hydra.compute.Trace(t0.stack, t1.messages, t1.other)
    return mutate_trace(mutate, restore, f)
