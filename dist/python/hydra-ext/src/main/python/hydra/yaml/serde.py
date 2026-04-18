# Note: this is an automatically generated file. Do not edit.

r"""Native YAML serialization: YAML Node to String."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.lib.chars
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.yaml.model

def escape_single_quotes(s: str) -> str:
    r"""Escape single quotes by doubling them."""

    squote = 39
    return hydra.lib.strings.from_list(hydra.lib.lists.bind(hydra.lib.strings.to_list(s), (lambda c: hydra.lib.logic.if_else(hydra.lib.equality.equal(c, squote), (lambda : (squote, squote)), (lambda : (c,))))))

def has_leading_trailing_space(s: str) -> bool:
    r"""Check if a string has leading or trailing whitespace."""

    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(s)
    return hydra.lib.logic.or_(hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda c: hydra.lib.chars.is_space(c)), hydra.lib.lists.maybe_head(chars()))), hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda c: hydra.lib.chars.is_space(c)), hydra.lib.lists.maybe_last(chars()))))

def indent_string(s: str) -> str:
    r"""Indent all lines of a string by 2 spaces."""

    return hydra.lib.strings.cat(hydra.lib.lists.map((lambda line: hydra.lib.logic.if_else(hydra.lib.strings.null(line), (lambda : ""), (lambda : hydra.lib.strings.cat(("  ", line, "\n"))))), hydra.lib.strings.lines(s)))

def is_decimal_string(chars: frozenlist[int]) -> bool:
    r"""Check if character codes represent a decimal number."""

    dot_code = 46
    @lru_cache(1)
    def parts() -> tuple[frozenlist[int], frozenlist[int]]:
        return hydra.lib.lists.span((lambda c: hydra.lib.logic.not_(hydra.lib.equality.equal(c, dot_code))), chars)
    @lru_cache(1)
    def before() -> frozenlist[int]:
        return hydra.lib.pairs.first(parts())
    @lru_cache(1)
    def after_with_dot() -> frozenlist[int]:
        return hydra.lib.pairs.second(parts())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(before()), (lambda : False), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(after_with_dot()), (lambda : False), (lambda : (after := hydra.lib.lists.drop(1, after_with_dot()), hydra.lib.logic.if_else(hydra.lib.lists.null(after), (lambda : False), (lambda : (is_digit_fn := (lambda c: hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))), hydra.lib.logic.and_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(is_digit_fn(c))), before())), hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(is_digit_fn(c))), after))))[1])))[1]))))

def looks_like_number(s: str) -> bool:
    r"""Check if a string looks like a number."""

    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(s)
    return hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda p: (first_ch := hydra.lib.pairs.first(p), tail_ch := hydra.lib.pairs.second(p), rest := hydra.lib.logic.if_else(hydra.lib.equality.equal(first_ch, 45), (lambda : tail_ch), (lambda : chars())), is_digit_fn := (lambda c: hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))), all_digits := hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.null(rest)), hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.logic.not_(is_digit_fn(c))), rest))), hydra.lib.logic.if_else(all_digits, (lambda : True), (lambda : is_decimal_string(rest))))[5]), hydra.lib.lists.uncons(chars())))

# YAML reserved words that need quoting.
yaml_reserved_words = ("true", "false", "null", "~", "yes", "no", "on", "off", "True", "False", "Null", "Yes", "No", "On", "Off", "TRUE", "FALSE", "NULL", "YES", "NO", "ON", "OFF")

# YAML special characters that trigger quoting.
yaml_special_chars = ": {}[]#,&*!|>'\"%@`"

def needs_quoting(s: str) -> bool:
    r"""Check if a string needs quoting in YAML."""

    return hydra.lib.logic.if_else(hydra.lib.strings.null(s), (lambda : True), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.elem(s, yaml_reserved_words), (lambda : True), (lambda : hydra.lib.logic.if_else(looks_like_number(s), (lambda : True), (lambda : (chars := hydra.lib.strings.to_list(s), (specials := hydra.lib.strings.to_list(yaml_special_chars), (has_special := hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda c: hydra.lib.lists.elem(c, specials)), chars))), hydra.lib.logic.if_else(has_special, (lambda : True), (lambda : has_leading_trailing_space(s))))[1])[1])[1]))))))

def write_string(s: str) -> str:
    r"""Write a string value, quoting if necessary."""

    return hydra.lib.logic.if_else(needs_quoting(s), (lambda : hydra.lib.strings.cat(("'", escape_single_quotes(s), "'"))), (lambda : s))

def write_scalar(s: hydra.yaml.model.Scalar) -> str:
    r"""Write a scalar value."""

    match s:
        case hydra.yaml.model.ScalarBool(value=b):
            return hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false"))

        case hydra.yaml.model.ScalarDecimal(value=d):
            return hydra.lib.literals.show_decimal(d)

        case hydra.yaml.model.ScalarFloat(value=f):
            return hydra.lib.literals.show_bigfloat(f)

        case hydra.yaml.model.ScalarInt(value=i):
            return hydra.lib.literals.show_bigint(i)

        case hydra.yaml.model.ScalarNull():
            return "null"

        case hydra.yaml.model.ScalarStr(value=str):
            return write_string(str)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_node_inline(node: hydra.yaml.model.Node_) -> str:
    r"""Write a node inline (for use as a mapping key)."""

    match node:
        case hydra.yaml.model.NodeScalar(value=s):
            return write_scalar(s)

        case hydra.yaml.model.NodeSequence(value=items):
            return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda item: write_node_inline(item)), items)), "]"))

        case hydra.yaml.model.NodeMapping(value=m):
            def write_flow_entry(e: tuple[hydra.yaml.model.Node_, hydra.yaml.model.Node_]) -> str:
                return hydra.lib.strings.cat((write_node_inline(hydra.lib.pairs.first(e)), ": ", write_node_inline(hydra.lib.pairs.second(e))))
            return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: write_flow_entry(x1)), hydra.lib.maps.to_list(m))), "}"))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_mapping_entry(entry: tuple[hydra.yaml.model.Node_, hydra.yaml.model.Node_]) -> str:
    r"""Write a mapping entry in block style."""

    @lru_cache(1)
    def key() -> hydra.yaml.model.Node_:
        return hydra.lib.pairs.first(entry)
    @lru_cache(1)
    def value() -> hydra.yaml.model.Node_:
        return hydra.lib.pairs.second(entry)
    match value():
        case hydra.yaml.model.NodeScalar(value=s):
            return hydra.lib.strings.cat((write_node_inline(key()), ": ", write_scalar(s), "\n"))

        case hydra.yaml.model.NodeSequence(value=items):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(items), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ": []\n"))), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ":\n", indent_string(write_node(value()))))))

        case hydra.yaml.model.NodeMapping(value=m):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maps.size(m), 0), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ": {}\n"))), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ":\n", indent_string(write_node(value()))))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_mapping_entry_inline(entry: tuple[hydra.yaml.model.Node_, hydra.yaml.model.Node_]) -> str:
    r"""Write a mapping entry for the first item of a sequence element."""

    @lru_cache(1)
    def key() -> hydra.yaml.model.Node_:
        return hydra.lib.pairs.first(entry)
    @lru_cache(1)
    def value() -> hydra.yaml.model.Node_:
        return hydra.lib.pairs.second(entry)
    match value():
        case hydra.yaml.model.NodeScalar(value=s):
            return hydra.lib.strings.cat((write_node_inline(key()), ": ", write_scalar(s), "\n"))

        case hydra.yaml.model.NodeSequence(value=items):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(items), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ": []\n"))), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ":\n", indent_string(write_node(value()))))))

        case hydra.yaml.model.NodeMapping(value=m):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maps.size(m), 0), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ": {}\n"))), (lambda : hydra.lib.strings.cat((write_node_inline(key()), ":\n", indent_string(write_node(value()))))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_node(node: hydra.yaml.model.Node_) -> str:
    r"""Write a YAML node as a top-level value in block style."""

    match node:
        case hydra.yaml.model.NodeScalar(value=s):
            return hydra.lib.strings.cat2(write_scalar(s), "\n")

        case hydra.yaml.model.NodeSequence(value=items):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(items), (lambda : "[]\n"), (lambda : hydra.lib.strings.cat(hydra.lib.lists.map((lambda item: write_sequence_item(item)), items))))

        case hydra.yaml.model.NodeMapping(value=m):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maps.size(m), 0), (lambda : "{}\n"), (lambda : hydra.lib.strings.cat(hydra.lib.lists.map((lambda e: write_mapping_entry(e)), hydra.lib.maps.to_list(m)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_sequence_item(node: hydra.yaml.model.Node_) -> str:
    r"""Write a sequence item in block style."""

    match node:
        case hydra.yaml.model.NodeScalar(value=s):
            return hydra.lib.strings.cat(("- ", write_scalar(s), "\n"))

        case hydra.yaml.model.NodeSequence(value=items):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(items), (lambda : "- []\n"), (lambda : hydra.lib.strings.cat2("-\n", indent_string(write_node(node)))))

        case hydra.yaml.model.NodeMapping(value=m):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maps.size(m), 0), (lambda : "- {}\n"), (lambda : (entries := hydra.lib.maps.to_list(m), hydra.lib.maybes.from_maybe((lambda : ""), hydra.lib.maybes.map((lambda p: (first_entry := hydra.lib.pairs.first(p), rest_entries := hydra.lib.pairs.second(p), first_str := write_mapping_entry_inline(first_entry), rest_str := hydra.lib.strings.cat(hydra.lib.lists.map((lambda e: write_mapping_entry(e)), rest_entries)), hydra.lib.strings.cat(("- ", first_str, indent_string(rest_str))))[4]), hydra.lib.lists.uncons(entries))))[1]))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def hydra_yaml_to_string(node: hydra.yaml.model.Node_) -> str:
    r"""Serialize a YAML node to a string."""

    return write_node(node)
