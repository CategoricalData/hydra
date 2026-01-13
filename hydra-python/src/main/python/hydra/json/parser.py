# Note: this is an automatically generated file. Do not edit.

r"""JSON parser using Hydra parser combinators."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.json.model
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsers
import hydra.parsing

T0 = TypeVar("T0")

@lru_cache(1)
def digit() -> hydra.parsing.Parser[int]:
    r"""Parse a single digit (0-9)."""
    
    return hydra.parsers.satisfy((lambda c: hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))))

@lru_cache(1)
def digits() -> hydra.parsing.Parser[str]:
    r"""Parse one or more digits as a string."""
    
    return hydra.parsers.map(hydra.lib.strings.from_list, hydra.parsers.some(digit()))

@lru_cache(1)
def whitespace() -> hydra.parsing.Parser[None]:
    r"""Parse zero or more JSON whitespace characters (space, tab, newline, carriage return)."""
    
    return hydra.parsers.map((lambda _: None), hydra.parsers.many(hydra.parsers.satisfy((lambda c: hydra.lib.lists.foldl(hydra.lib.logic.or_, False, (hydra.lib.equality.equal(c, 32), hydra.lib.equality.equal(c, 9), hydra.lib.equality.equal(c, 10), hydra.lib.equality.equal(c, 13)))))))

def token(p: hydra.parsing.Parser[T0]) -> hydra.parsing.Parser[T0]:
    return hydra.parsers.bind(p, (lambda x: hydra.parsers.bind(whitespace(), (lambda _: hydra.parsers.pure(x)))))

@lru_cache(1)
def json_bool() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse JSON boolean (true or false)."""
    
    return hydra.parsers.alt(hydra.parsers.map((lambda _: cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(True))), token(hydra.parsers.string("true"))), hydra.parsers.map((lambda _: cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(False))), token(hydra.parsers.string("false"))))

@lru_cache(1)
def json_null() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse JSON null value."""
    
    return hydra.parsers.map((lambda _: cast(hydra.json.model.Value, hydra.json.model.ValueNull())), token(hydra.parsers.string("null")))

@lru_cache(1)
def json_exponent_part() -> hydra.parsing.Parser[Maybe[str]]:
    r"""Parse the optional exponent part of a JSON number."""
    
    return hydra.parsers.optional(hydra.parsers.bind(hydra.parsers.satisfy((lambda c: hydra.lib.logic.or_(hydra.lib.equality.equal(c, 101), hydra.lib.equality.equal(c, 69)))), (lambda _: hydra.parsers.bind(hydra.parsers.optional(hydra.parsers.satisfy((lambda c: hydra.lib.logic.or_(hydra.lib.equality.equal(c, 43), hydra.lib.equality.equal(c, 45))))), (lambda sign: hydra.parsers.map((lambda digits: hydra.lib.strings.cat2(hydra.lib.strings.cat2("e", hydra.lib.maybes.maybe("", (lambda arg_: hydra.lib.strings.from_list(hydra.lib.lists.pure(arg_))), sign)), digits)), digits()))))))

@lru_cache(1)
def json_fraction_part() -> hydra.parsing.Parser[Maybe[str]]:
    r"""Parse the optional fractional part of a JSON number."""
    
    return hydra.parsers.optional(hydra.parsers.bind(hydra.parsers.char(46), (lambda _: hydra.parsers.map((lambda d: hydra.lib.strings.cat2(".", d)), digits()))))

@lru_cache(1)
def json_integer_part() -> hydra.parsing.Parser[str]:
    r"""Parse the integer part of a JSON number (optional minus, then digits)."""
    
    return hydra.parsers.bind(hydra.parsers.optional(hydra.parsers.char(45)), (lambda sign: hydra.parsers.bind(digits(), (lambda digits: hydra.parsers.pure(hydra.lib.maybes.maybe(digits, (lambda _: hydra.lib.strings.cat2("-", digits)), sign))))))

@lru_cache(1)
def json_number() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse a JSON number (integer, decimal, or scientific notation)."""
    
    return token(hydra.parsers.bind(json_integer_part(), (lambda int_part: hydra.parsers.bind(json_fraction_part(), (lambda frac_part: hydra.parsers.bind(json_exponent_part(), (lambda exp_part: (num_str := hydra.lib.strings.cat2(hydra.lib.strings.cat2(int_part, hydra.lib.maybes.maybe("", (lambda x1: hydra.lib.equality.identity(x1)), frac_part)), hydra.lib.maybes.maybe("", (lambda x1: hydra.lib.equality.identity(x1)), exp_part)), hydra.parsers.pure(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.maybes.maybe(Decimal('0.0'), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.literals.read_bigfloat(num_str))))))[1])))))))

@lru_cache(1)
def json_escape_char() -> hydra.parsing.Parser[int]:
    r"""Parse a JSON escape sequence after the backslash."""
    
    return hydra.parsers.choice((hydra.parsers.map((lambda _: 34), hydra.parsers.char(34)), hydra.parsers.map((lambda _: 92), hydra.parsers.char(92)), hydra.parsers.map((lambda _: 47), hydra.parsers.char(47)), hydra.parsers.map((lambda _: 8), hydra.parsers.char(98)), hydra.parsers.map((lambda _: 12), hydra.parsers.char(102)), hydra.parsers.map((lambda _: 10), hydra.parsers.char(110)), hydra.parsers.map((lambda _: 13), hydra.parsers.char(114)), hydra.parsers.map((lambda _: 9), hydra.parsers.char(116))))

@lru_cache(1)
def json_string_char() -> hydra.parsing.Parser[int]:
    r"""Parse a single character in a JSON string (handling escapes)."""
    
    return hydra.parsers.alt(hydra.parsers.bind(hydra.parsers.char(92), (lambda _: json_escape_char())), hydra.parsers.satisfy((lambda c: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(c, 34)), hydra.lib.logic.not_(hydra.lib.equality.equal(c, 92))))))

@lru_cache(1)
def json_string() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse a JSON string value."""
    
    return token(hydra.parsers.bind(hydra.parsers.char(34), (lambda _: hydra.parsers.bind(hydra.parsers.many(json_string_char()), (lambda chars: hydra.parsers.bind(hydra.parsers.char(34), (lambda _2: hydra.parsers.pure(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.strings.from_list(chars)))))))))))

@lru_cache(1)
def json_array() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse a JSON array."""
    
    return hydra.parsers.map((lambda x: cast(hydra.json.model.Value, hydra.json.model.ValueArray(x))), hydra.parsers.between(token(hydra.parsers.char(91)), token(hydra.parsers.char(93)), hydra.parsers.sep_by(json_value(), token(hydra.parsers.char(44)))))

@lru_cache(1)
def json_key_value() -> hydra.parsing.Parser[tuple[str, hydra.json.model.Value]]:
    r"""Parse a JSON object key-value pair."""
    
    return hydra.parsers.bind(token(hydra.parsers.bind(hydra.parsers.char(34), (lambda _: hydra.parsers.bind(hydra.parsers.many(json_string_char()), (lambda chars: hydra.parsers.bind(hydra.parsers.char(34), (lambda _2: hydra.parsers.pure(hydra.lib.strings.from_list(chars))))))))), (lambda key: hydra.parsers.bind(token(hydra.parsers.char(58)), (lambda _: hydra.parsers.map((lambda v: (key, v)), json_value())))))

@lru_cache(1)
def json_object() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse a JSON object."""
    
    return hydra.parsers.map((lambda arg_: (lambda x: cast(hydra.json.model.Value, hydra.json.model.ValueObject(x)))(hydra.lib.maps.from_list(arg_))), hydra.parsers.between(token(hydra.parsers.char(123)), token(hydra.parsers.char(125)), hydra.parsers.sep_by(json_key_value(), token(hydra.parsers.char(44)))))

@lru_cache(1)
def json_value() -> hydra.parsing.Parser[hydra.json.model.Value]:
    r"""Parse any JSON value."""
    
    return hydra.parsers.choice((json_null(), json_bool(), json_number(), json_string(), json_array(), json_object()))

def parse_json(input: str) -> hydra.parsing.ParseResult[hydra.json.model.Value]:
    r"""Parse a JSON document from a string."""
    
    return hydra.parsers.bind(whitespace(), (lambda _: hydra.parsers.bind(json_value(), (lambda v: hydra.parsers.bind(whitespace(), (lambda _2: hydra.parsers.bind(hydra.parsers.eof(), (lambda _3: hydra.parsers.pure(v))))))))).value(input)
