# Note: this is an automatically generated file. Do not edit.

r"""General-purpose parser combinators."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def alt(p1: hydra.parsing.Parser[T0], p2: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T0]:
    def parse(input: str) -> hydra.parsing.ParseResult[T0]:
        return (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported"))(p1.value(input))
    return hydra.parsing.Parser(parse)

def satisfy(pred: Callable[[int], bool]) -> hydra.parsing.Parser[int]:
    r"""Parse a character (codepoint) that satisfies the given predicate."""
    
    def parse(input: str) -> hydra.parsing.ParseResult[int]:
        @lru_cache(1)
        def codes() -> frozenlist[int]:
            return hydra.lib.strings.to_list(input)
        @lru_cache(1)
        def c() -> int:
            return hydra.lib.lists.head(codes())
        @lru_cache(1)
        def rest() -> str:
            return hydra.lib.strings.from_list(hydra.lib.lists.tail(codes()))
        return hydra.lib.logic.if_else(hydra.lib.strings.null(input), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError("unexpected end of input", input)))), (lambda : hydra.lib.logic.if_else(pred(c()), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(hydra.parsing.ParseSuccess(c(), rest())))), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError("character did not satisfy predicate", input)))))))
    return hydra.parsing.Parser(parse)

@lru_cache(1)
def any_char() -> hydra.parsing.Parser[int]:
    r"""Parse any single character (codepoint)."""
    
    return satisfy((lambda _: True))

def apply(pf: hydra.parsing.Parser[Callable[[T0], T1]], pa: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T1]:
    def parse(input: str) -> hydra.parsing.ParseResult[T1]:
        def _hoist_parse_1(sf: hydra.parsing.ParseSuccess[Callable[[T2], T3]], v1: hydra.parsing.ParseResult[T2]) -> hydra.parsing.ParseResult[T3]:
            match v1:
                case hydra.parsing.ParseResultSuccess(value=sa):
                    return cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(hydra.parsing.ParseSuccess(sf.value(sa.value), sa.remainder)))
                
                case hydra.parsing.ParseResultFailure(value=e):
                    return cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(e))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported"))(pf.value(input))
    return hydra.parsing.Parser(parse)

def bind(pa: hydra.parsing.Parser[T0], f: Callable[[T0], hydra.parsing.Parser[T1]]) -> hydra.parsing.Parser[T1]:
    def parse(input: str) -> hydra.parsing.ParseResult[T1]:
        return (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported"))(pa.value(input))
    return hydra.parsing.Parser(parse)

def pure(a: T0) -> hydra.parsing.Parser[T0]:
    return hydra.parsing.Parser((lambda input: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(hydra.parsing.ParseSuccess(a, input)))))

def between(open: hydra.parsing.Parser[T0], close: hydra.parsing.Parser[T1], p: hydra.parsing.Parser[T2]) -> hydra.parsing.Parser[T2]:
    return bind(open, (lambda _: bind(p, (lambda x: bind(close, (lambda _2: pure(x)))))))

def char(c: int) -> hydra.parsing.Parser[int]:
    r"""Parse a specific character (codepoint)."""
    
    return satisfy((lambda x: hydra.lib.equality.equal(x, c)))

def fail(msg: str) -> hydra.parsing.Parser[T0]:
    return hydra.parsing.Parser((lambda input: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError(msg, input)))))

def choice(ps: frozenlist[hydra.parsing.Parser[T0]]) -> hydra.parsing.Parser[T0]:
    return hydra.lib.lists.foldl((lambda x1, x2: alt(x1, x2)), fail("no choice matched"), ps)

@lru_cache(1)
def eof() -> hydra.parsing.Parser[None]:
    r"""A parser that succeeds only at the end of input."""
    
    return hydra.parsing.Parser((lambda input: hydra.lib.logic.if_else(hydra.lib.equality.equal(input, ""), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(hydra.parsing.ParseSuccess(None, "")))), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError("expected end of input", input)))))))

def many(p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return alt(some(p), pure(()))

def some(p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return bind(p, (lambda x: bind(many(p), (lambda xs: pure(hydra.lib.lists.cons(x, xs))))))

def map(f: Callable[[T0], T1], pa: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T1]:
    def parse(input: str) -> hydra.parsing.ParseResult[T1]:
        return (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported"))(pa.value(input))
    return hydra.parsing.Parser(parse)

def optional(p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[Maybe[T0]]:
    return alt(map((lambda x1: hydra.lib.maybes.pure(x1)), p), pure(Nothing()))

def run_parser(p: hydra.parsing.Parser[T0], input: str) -> hydra.parsing.ParseResult[T0]:
    return p.value(input)

def sep_by1(p: hydra.parsing.Parser[T0], sep: hydra.parsing.Parser[T1]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return bind(p, (lambda x: bind(many(bind(sep, (lambda _: p))), (lambda xs: pure(hydra.lib.lists.cons(x, xs))))))

def sep_by(p: hydra.parsing.Parser[T0], sep: hydra.parsing.Parser[T1]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return alt(sep_by1(p, sep), pure(()))

def string(str: str) -> hydra.parsing.Parser[str]:
    r"""Parse a specific string."""
    
    return hydra.parsing.Parser((lambda input: (str_codes := hydra.lib.strings.to_list(str), input_codes := hydra.lib.strings.to_list(input), str_len := hydra.lib.lists.length(str_codes), input_prefix := hydra.lib.lists.take(str_len, input_codes), hydra.lib.logic.if_else(hydra.lib.equality.equal(str_codes, input_prefix), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(hydra.parsing.ParseSuccess(str, hydra.lib.strings.from_list(hydra.lib.lists.drop(str_len, input_codes)))))), (lambda : cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError(hydra.lib.strings.cat2("expected: ", str), input))))))[4]))
