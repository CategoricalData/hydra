# Note: this is an automatically generated file. Do not edit.

r"""JSON serialization functions using the Hydra AST."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.ast
import hydra.core
import hydra.json.model
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.strings
import hydra.serialization

# The colon operator used to separate keys and values in JSON objects.
colon_op = hydra.ast.Op(hydra.ast.Symbol(":"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)

def json_string(s: str) -> str:
    r"""Escape and quote a string for JSON output."""
    
    def escape(c: int) -> str:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 34), (lambda : "\\\""), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 92), (lambda : "\\\\"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 10), (lambda : "\\n"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 13), (lambda : "\\r"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 9), (lambda : "\\t"), (lambda : hydra.lib.strings.from_list(hydra.lib.lists.pure(c))))))))))))
    @lru_cache(1)
    def escaped() -> str:
        return hydra.lib.strings.cat(hydra.lib.lists.map(escape, hydra.lib.strings.to_list(s)))
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2("\"", escaped()), "\"")

def key_value_to_expr(pair: tuple[str, hydra.json.model.Value]) -> hydra.core.Type:
    r"""Convert a key-value pair to an AST expression."""
    
    @lru_cache(1)
    def key() -> str:
        return hydra.lib.pairs.first(pair)
    @lru_cache(1)
    def value() -> hydra.core.Type:
        return hydra.lib.pairs.second(pair)
    return hydra.serialization.ifx(colon_op, hydra.serialization.cst(json_string(key())), value_to_expr(value()))

def value_to_expr(value: hydra.json.model.Value) -> hydra.core.Type:
    r"""Convert a JSON value to an AST expression for serialization."""
    
    match value:
        case hydra.json.model.ValueArray(value=arr):
            return hydra.serialization.bracket_list_adaptive(hydra.lib.lists.map(value_to_expr, arr))
        
        case hydra.json.model.ValueBoolean(value=b):
            return hydra.serialization.cst(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))
        
        case hydra.json.model.ValueNull():
            return hydra.serialization.cst("null")
        
        case hydra.json.model.ValueNumber(value=n):
            return hydra.serialization.cst(hydra.lib.literals.show_bigfloat(n))
        
        case hydra.json.model.ValueObject(value=obj):
            return hydra.serialization.braces_list_adaptive(hydra.lib.lists.map(key_value_to_expr, hydra.lib.maps.to_list(obj)))
        
        case hydra.json.model.ValueString(value=s):
            return hydra.serialization.cst(json_string(s))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def print_json(value: hydra.json.model.Value) -> str:
    r"""Serialize a JSON value to a string."""
    
    return hydra.serialization.print_expr(value_to_expr(value))
