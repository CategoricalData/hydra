# Note: this is an automatically generated file. Do not edit.

r"""Functions for working with Hydra's 'flow' and other monads."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.constants
import hydra.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")

def bind(l: hydra.compute.Flow[T0, T1], r: Callable[[T1], hydra.compute.Flow[T0, T2]]) -> hydra.compute.Flow[T0, T2]:
    def q(s0: T0, t0: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T2]:
        @lru_cache(1)
        def fs1() -> hydra.compute.FlowState[T0, T1]:
            return l.value(s0, t0)
        return hydra.lib.maybes.maybe(hydra.compute.FlowState(Nothing(), fs1().state, fs1().trace), (lambda v: r(v).value(fs1().state, fs1().trace)), fs1().value)
    return hydra.compute.Flow(q)

def push_error(msg: str, t: hydra.compute.Trace) -> hydra.core.Type:
    r"""Push an error message."""
    
    def condense_repeats(ys: frozenlist[str]) -> frozenlist[str]:
        def condense_group(xs: frozenlist[str]) -> str:
            @lru_cache(1)
            def x() -> str:
                return hydra.lib.lists.head(xs)
            @lru_cache(1)
            def n() -> int:
                return hydra.lib.lists.length(xs)
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 1), (lambda : x()), (lambda : hydra.lib.strings.cat((x(), " (x", hydra.lib.literals.show_int32(n()), ")"))))
        return hydra.lib.lists.map(condense_group, hydra.lib.lists.group(ys))
    @lru_cache(1)
    def error_msg() -> str:
        return hydra.lib.strings.cat(("Error: ", msg, " (", hydra.lib.strings.intercalate(" > ", condense_repeats(hydra.lib.lists.reverse(t.stack))), ")"))
    return hydra.compute.Trace(t.stack, hydra.lib.lists.cons(error_msg(), t.messages), t.other)

def fail(msg: str) -> hydra.compute.Flow[T0, T1]:
    return hydra.compute.Flow((lambda s, t: hydra.compute.FlowState(Nothing(), s, push_error(msg, t))))

def pure(xp: T0) -> hydra.compute.Flow[T1, T0]:
    return hydra.compute.Flow((lambda s, t: hydra.compute.FlowState(Just(xp), s, t)))

def either_to_flow(format_error: Callable[[T0], str], e: Either[T0, T1]) -> hydra.compute.Flow[T2, T1]:
    return hydra.lib.eithers.either((lambda l: fail(format_error(l))), (lambda r: pure(r)), e)

@lru_cache(1)
def empty_trace() -> hydra.core.Type:
    r"""An empty trace with no stack, messages, or other attributes."""
    
    return hydra.compute.Trace((), (), hydra.lib.maps.empty())

def exec(f: hydra.compute.Flow[T0, T1], s0: T0) -> T0:
    return f.value(s0, empty_trace()).state

def flow_succeeds(s: T0, f: hydra.compute.Flow[T0, T1]) -> bool:
    return hydra.lib.maybes.is_just(f.value(s, empty_trace()).value)

def from_flow(def_: T0, cx: T1, f: hydra.compute.Flow[T1, T0]) -> T0:
    return hydra.lib.maybes.maybe(def_, (lambda xmo: xmo), f.value(cx, empty_trace()).value)

@lru_cache(1)
def get_state() -> hydra.compute.Flow[T0, T0]:
    return hydra.compute.Flow((lambda s0, t0: (fs1 := pure(None).value(s0, t0), v := fs1.value, s := fs1.state, t := fs1.trace, hydra.lib.maybes.maybe(hydra.compute.FlowState(Nothing(), s, t), (lambda _: hydra.compute.FlowState(Just(s), s, t)), v))[4]))

def map(f: Callable[[T0], T1], f1: hydra.compute.Flow[T2, T0]) -> hydra.compute.Flow[T2, T1]:
    return hydra.compute.Flow((lambda s0, t0: (f2 := f1.value(s0, t0), hydra.compute.FlowState(hydra.lib.maybes.map(f, f2.value), f2.state, f2.trace))[1]))

def maybe_to_list(mx: Maybe[T0]) -> frozenlist[T0]:
    return hydra.lib.maybes.maybe((), (lambda x1: hydra.lib.lists.pure(x1)), mx)

def put_state(cx: T0) -> hydra.compute.Flow[T0, None]:
    return hydra.compute.Flow((lambda s0, t0: (f1 := pure(None).value(s0, t0), hydra.compute.FlowState(f1.value, cx, f1.trace))[1]))

def modify(f: Callable[[T0], T0]) -> hydra.compute.Flow[T0, None]:
    return bind(get_state(), (lambda s: put_state(f(s))))

def mutate_trace(mutate: Callable[[hydra.compute.Trace], Either[str, hydra.compute.Trace]], restore: Callable[[hydra.compute.Trace, hydra.compute.Trace], hydra.compute.Trace], f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def choose(for_left: Callable[[T2], T3], for_right: Callable[[T4], T3], e: Either[T2, T4]) -> T3:
        return hydra.lib.eithers.either((lambda l: for_left(l)), (lambda r: for_right(r)), e)
    def flow_fun(s0: T0, t0: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T1]:
        def for_left(msg: str) -> hydra.compute.FlowState[T0, T2]:
            return hydra.compute.FlowState(Nothing(), s0, push_error(msg, t0))
        def for_right(t1: hydra.compute.Trace) -> hydra.compute.FlowState[T0, T1]:
            @lru_cache(1)
            def f2() -> hydra.compute.FlowState[T0, T1]:
                return f.value(s0, t1)
            return hydra.compute.FlowState(f2().value, f2().state, restore(t0, f2().trace))
        return choose((lambda x1: for_left(x1)), for_right, mutate(t0))
    return hydra.compute.Flow(flow_fun)

def trace_summary(t: hydra.compute.Trace) -> str:
    r"""Summarize a trace as a string."""
    
    @lru_cache(1)
    def message_lines() -> frozenlist[str]:
        return hydra.lib.lists.nub(t.messages)
    def to_line(pair: tuple[hydra.core.Name, hydra.core.Term]) -> str:
        return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("\t", hydra.lib.pairs.first(pair).value), ": "), hydra.show.core.term(hydra.lib.pairs.second(pair)))
    @lru_cache(1)
    def keyval_lines() -> frozenlist[str]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(t.other), (lambda : ()), (lambda : hydra.lib.lists.cons("key/value pairs: ", hydra.lib.lists.map(to_line, hydra.lib.maps.to_list(t.other)))))
    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.concat2(message_lines(), keyval_lines()))

def unexpected(expected: str, actual: str) -> hydra.compute.Flow[T0, T1]:
    return fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", expected), " but found "), actual))

def warn(msg: str, b: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    return hydra.compute.Flow((lambda s0, t0: (f1 := b.value(s0, t0), add_message := (lambda t: hydra.compute.Trace(t.stack, hydra.lib.lists.cons(hydra.lib.strings.cat2("Warning: ", msg), t.messages), t.other)), hydra.compute.FlowState(f1.value, f1.state, add_message(f1.trace)))[2]))

def with_flag(flag: hydra.core.Name, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def mutate(t: hydra.compute.Trace) -> Either[str, hydra.compute.Trace]:
        return hydra.lib.logic.if_else(False, (lambda : Left("never happens")), (lambda : Right(hydra.compute.Trace(t.stack, t.messages, hydra.lib.maps.insert(flag, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))), t.other)))))
    def restore(ignored: T2, t1: hydra.compute.Trace) -> hydra.core.Type:
        return hydra.compute.Trace(t1.stack, t1.messages, hydra.lib.maps.delete(flag, t1.other))
    return mutate_trace(mutate, (lambda x1, x2: restore(x1, x2)), f)

def with_state(cx0: T0, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T2, T1]:
    return hydra.compute.Flow((lambda cx1, t1: (f1 := f.value(cx0, t1), hydra.compute.FlowState(f1.value, cx1, f1.trace))[1]))

def with_trace(msg: str, f: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    def mutate(t: hydra.compute.Trace) -> Either[str, hydra.compute.Trace]:
        return hydra.lib.logic.if_else(hydra.lib.equality.gte(hydra.lib.lists.length(t.stack), hydra.constants.max_trace_depth), (lambda : Left("maximum trace depth exceeded. This may indicate an infinite loop")), (lambda : Right(hydra.compute.Trace(hydra.lib.lists.cons(msg, t.stack), t.messages, t.other))))
    def restore(t0: hydra.compute.Trace, t1: hydra.compute.Trace) -> hydra.core.Type:
        return hydra.compute.Trace(t0.stack, t1.messages, t1.other)
    return mutate_trace(mutate, restore, f)
