# Note: this is an automatically generated file. Do not edit.

r"""General-purpose parser combinators."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsing

def alt[T0](p1: hydra.parsing.Parser[T0], p2: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T0]:
    return cast(hydra.parsing.Parser[T0], hydra.parsing.Parser((lambda input: "inline match expressions are unsupported")))

def satisfy(pred: Callable[[int], bool]) -> hydra.parsing.Parser[int]:
    r"""Parse a character (codepoint) that satisfies the given predicate."""
    
    return cast(hydra.parsing.Parser[int], hydra.parsing.Parser((lambda input: hydra.lib.logic.if_else(hydra.lib.strings.null(input), (lambda : cast(hydra.parsing.ParseResult[int], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError("unexpected end of input", input))))), (lambda : "let terms are not supported here")))))

# Parse any single character (codepoint).
any_char = satisfy((lambda _: True))

def apply[T0, T1](pf: hydra.parsing.Parser[Callable[[T0], T1]], pa: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T1]:
    return cast(hydra.parsing.Parser[T1], hydra.parsing.Parser((lambda input: "inline match expressions are unsupported")))

def bind[T0, T1](pa: hydra.parsing.Parser[T0], f: Callable[[T0], hydra.parsing.Parser[T1]]) -> hydra.parsing.Parser[T1]:
    return cast(hydra.parsing.Parser[T1], hydra.parsing.Parser((lambda input: "inline match expressions are unsupported")))

def pure[T0](a: T0) -> hydra.parsing.Parser[T0]:
    return cast(hydra.parsing.Parser[T0], hydra.parsing.Parser((lambda input: cast(hydra.parsing.ParseResult[T0], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(cast(hydra.parsing.ParseSuccess[T0], hydra.parsing.ParseSuccess(a, input))))))))

def between[T0, T1, T2](open: hydra.parsing.Parser[T0], close: hydra.parsing.Parser[T1], p: hydra.parsing.Parser[T2]) -> hydra.parsing.Parser[T2]:
    return bind(open, (lambda _: bind(p, (lambda x: bind(close, (lambda _2: pure(x)))))))

def char(c: int) -> hydra.parsing.Parser[int]:
    r"""Parse a specific character (codepoint)."""
    
    return satisfy((lambda x: hydra.lib.equality.equal(x, c)))

def fail[T0](msg: str) -> hydra.parsing.Parser[T0]:
    return cast(hydra.parsing.Parser[T0], hydra.parsing.Parser((lambda input: cast(hydra.parsing.ParseResult[T0], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError(msg, input)))))))

def choice[T0](ps: frozenlist[hydra.parsing.Parser[T0]]) -> hydra.parsing.Parser[T0]:
    return hydra.lib.lists.foldl(cast(Callable[[hydra.parsing.Parser[T0], hydra.parsing.Parser[T0]], hydra.parsing.Parser[T0]], alt), fail("no choice matched"), ps)

# A parser that succeeds only at the end of input.
eof = cast(hydra.parsing.Parser[None], hydra.parsing.Parser((lambda input: hydra.lib.logic.if_else(hydra.lib.equality.equal(input, ""), (lambda : cast(hydra.parsing.ParseResult[None], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(cast(hydra.parsing.ParseSuccess[None], hydra.parsing.ParseSuccess(None, "")))))), (lambda : cast(hydra.parsing.ParseResult[None], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError("expected end of input", input)))))))))

def many[T0](p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return alt(some(p), pure(cast(frozenlist[T0], ())))

def some[T0](p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return bind(p, (lambda x: bind(many(p), (lambda xs: pure(hydra.lib.lists.cons(x, xs))))))

def map[T0, T1](f: Callable[[T0], T1], pa: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T1]:
    return cast(hydra.parsing.Parser[T1], hydra.parsing.Parser((lambda input: "inline match expressions are unsupported")))

def optional[T0](p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[Maybe[T0]]:
    return alt(map(cast(Callable[[T0], Maybe[T0]], hydra.lib.maybes.pure), p), pure(cast(Maybe[T0], Nothing())))

def run_parser[T0](p: hydra.parsing.Parser[T0], input: str) -> hydra.parsing.ParseResult[T0]:
    return p.value(input)

def sep_by1[T0, T1](p: hydra.parsing.Parser[T0], sep: hydra.parsing.Parser[T1]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return bind(p, (lambda x: bind(many(bind(sep, (lambda _: p))), (lambda xs: pure(hydra.lib.lists.cons(x, xs))))))

def sep_by[T0, T1](p: hydra.parsing.Parser[T0], sep: hydra.parsing.Parser[T1]) -> hydra.parsing.Parser[frozenlist[T0]]:
    return alt(sep_by1(p, sep), pure(cast(frozenlist[T0], ())))

def string(str: str) -> hydra.parsing.Parser[str]:
    r"""Parse a specific string."""
    
    return cast(hydra.parsing.Parser[str], hydra.parsing.Parser((lambda input: (str_codes := hydra.lib.strings.to_list(str), input_codes := hydra.lib.strings.to_list(input), str_len := hydra.lib.lists.length(str_codes), input_prefix := hydra.lib.lists.take(str_len, input_codes), hydra.lib.logic.if_else(hydra.lib.equality.equal(str_codes, input_prefix), (lambda : cast(hydra.parsing.ParseResult[str], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(cast(hydra.parsing.ParseSuccess[str], hydra.parsing.ParseSuccess(str, hydra.lib.strings.from_list(hydra.lib.lists.drop(str_len, input_codes)))))))), (lambda : cast(hydra.parsing.ParseResult[str], cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(hydra.parsing.ParseError(hydra.lib.strings.cat2("expected: ", str), input)))))))[4])))
