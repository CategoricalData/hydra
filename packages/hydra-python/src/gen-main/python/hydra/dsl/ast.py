# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.ast."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

associativity_both = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("both"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

associativity_left = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("left"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

associativity_none = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("none"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

associativity_right = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("right"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def block_style(indent: hydra.phantoms.TTerm[Maybe[str]], newline_before_content: hydra.phantoms.TTerm[bool], newline_after_content: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), indent.value), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), newline_before_content.value), hydra.core.Field(hydra.core.Name("newlineAfterContent"), newline_after_content.value))))))

def block_style_indent(x: hydra.phantoms.TTerm[hydra.ast.BlockStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("indent")))))))), x.value))))

def block_style_newline_after_content(x: hydra.phantoms.TTerm[hydra.ast.BlockStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineAfterContent")))))))), x.value))))

def block_style_newline_before_content(x: hydra.phantoms.TTerm[hydra.ast.BlockStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineBeforeContent")))))))), x.value))))

def block_style_with_indent(original: hydra.phantoms.TTerm[hydra.ast.BlockStyle], new_val: hydra.phantoms.TTerm[Maybe[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), new_val.value), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineBeforeContent")))))))), original.value)))), hydra.core.Field(hydra.core.Name("newlineAfterContent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineAfterContent")))))))), original.value)))))))))

def block_style_with_newline_after_content(original: hydra.phantoms.TTerm[hydra.ast.BlockStyle], new_val: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("indent")))))))), original.value)))), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineBeforeContent")))))))), original.value)))), hydra.core.Field(hydra.core.Name("newlineAfterContent"), new_val.value))))))

def block_style_with_newline_before_content(original: hydra.phantoms.TTerm[hydra.ast.BlockStyle], new_val: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("indent")))))))), original.value)))), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), new_val.value), hydra.core.Field(hydra.core.Name("newlineAfterContent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BlockStyle"), hydra.core.Name("newlineAfterContent")))))))), original.value)))))))))

def bracket_expr(brackets: hydra.phantoms.TTerm[hydra.ast.Brackets], enclosed: hydra.phantoms.TTerm[hydra.ast.Expr], style: hydra.phantoms.TTerm[hydra.ast.BlockStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), brackets.value), hydra.core.Field(hydra.core.Name("enclosed"), enclosed.value), hydra.core.Field(hydra.core.Name("style"), style.value))))))

def bracket_expr_brackets(x: hydra.phantoms.TTerm[hydra.ast.BracketExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("brackets")))))))), x.value))))

def bracket_expr_enclosed(x: hydra.phantoms.TTerm[hydra.ast.BracketExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("enclosed")))))))), x.value))))

def bracket_expr_style(x: hydra.phantoms.TTerm[hydra.ast.BracketExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("style")))))))), x.value))))

def bracket_expr_with_brackets(original: hydra.phantoms.TTerm[hydra.ast.BracketExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Brackets]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), new_val.value), hydra.core.Field(hydra.core.Name("enclosed"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("enclosed")))))))), original.value)))), hydra.core.Field(hydra.core.Name("style"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("style")))))))), original.value)))))))))

def bracket_expr_with_enclosed(original: hydra.phantoms.TTerm[hydra.ast.BracketExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("brackets")))))))), original.value)))), hydra.core.Field(hydra.core.Name("enclosed"), new_val.value), hydra.core.Field(hydra.core.Name("style"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("style")))))))), original.value)))))))))

def bracket_expr_with_style(original: hydra.phantoms.TTerm[hydra.ast.BracketExpr], new_val: hydra.phantoms.TTerm[hydra.ast.BlockStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("brackets")))))))), original.value)))), hydra.core.Field(hydra.core.Name("enclosed"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.BracketExpr"), hydra.core.Name("enclosed")))))))), original.value)))), hydra.core.Field(hydra.core.Name("style"), new_val.value))))))

def brackets(open: hydra.phantoms.TTerm[hydra.ast.Symbol], close: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Brackets"), (hydra.core.Field(hydra.core.Name("open"), open.value), hydra.core.Field(hydra.core.Name("close"), close.value))))))

def brackets_close(x: hydra.phantoms.TTerm[hydra.ast.Brackets]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Brackets"), hydra.core.Name("close")))))))), x.value))))

def brackets_open(x: hydra.phantoms.TTerm[hydra.ast.Brackets]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Brackets"), hydra.core.Name("open")))))))), x.value))))

def brackets_with_close(original: hydra.phantoms.TTerm[hydra.ast.Brackets], new_val: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Brackets"), (hydra.core.Field(hydra.core.Name("open"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Brackets"), hydra.core.Name("open")))))))), original.value)))), hydra.core.Field(hydra.core.Name("close"), new_val.value))))))

def brackets_with_open(original: hydra.phantoms.TTerm[hydra.ast.Brackets], new_val: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Brackets"), (hydra.core.Field(hydra.core.Name("open"), new_val.value), hydra.core.Field(hydra.core.Name("close"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Brackets"), hydra.core.Name("close")))))))), original.value)))))))))

def expr_brackets(x: hydra.phantoms.TTerm[hydra.ast.BracketExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("brackets"), x.value)))))

def expr_const(x: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("const"), x.value)))))

def expr_indent(x: hydra.phantoms.TTerm[hydra.ast.IndentedExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("indent"), x.value)))))

def expr_op(x: hydra.phantoms.TTerm[hydra.ast.OpExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("op"), x.value)))))

def expr_seq(x: hydra.phantoms.TTerm[hydra.ast.SeqExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("seq"), x.value)))))

def indent_style_all_lines(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("allLines"), x.value)))))

def indent_style_subsequent_lines(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("subsequentLines"), x.value)))))

def indented_expression(style: hydra.phantoms.TTerm[hydra.ast.IndentStyle], expr: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.IndentedExpression"), (hydra.core.Field(hydra.core.Name("style"), style.value), hydra.core.Field(hydra.core.Name("expr"), expr.value))))))

def indented_expression_expr(x: hydra.phantoms.TTerm[hydra.ast.IndentedExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.IndentedExpression"), hydra.core.Name("expr")))))))), x.value))))

def indented_expression_style(x: hydra.phantoms.TTerm[hydra.ast.IndentedExpression]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.IndentedExpression"), hydra.core.Name("style")))))))), x.value))))

def indented_expression_with_expr(original: hydra.phantoms.TTerm[hydra.ast.IndentedExpression], new_val: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.IndentedExpression"), (hydra.core.Field(hydra.core.Name("style"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.IndentedExpression"), hydra.core.Name("style")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expr"), new_val.value))))))

def indented_expression_with_style(original: hydra.phantoms.TTerm[hydra.ast.IndentedExpression], new_val: hydra.phantoms.TTerm[hydra.ast.IndentStyle]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.IndentedExpression"), (hydra.core.Field(hydra.core.Name("style"), new_val.value), hydra.core.Field(hydra.core.Name("expr"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.IndentedExpression"), hydra.core.Name("expr")))))))), original.value)))))))))

def op(symbol: hydra.phantoms.TTerm[hydra.ast.Symbol], padding: hydra.phantoms.TTerm[hydra.ast.Padding], precedence: hydra.phantoms.TTerm[hydra.ast.Precedence], associativity: hydra.phantoms.TTerm[hydra.ast.Associativity]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), symbol.value), hydra.core.Field(hydra.core.Name("padding"), padding.value), hydra.core.Field(hydra.core.Name("precedence"), precedence.value), hydra.core.Field(hydra.core.Name("associativity"), associativity.value))))))

def op_associativity(x: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("associativity")))))))), x.value))))

def op_expr(op: hydra.phantoms.TTerm[hydra.ast.Op], lhs: hydra.phantoms.TTerm[hydra.ast.Expr], rhs: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), op.value), hydra.core.Field(hydra.core.Name("lhs"), lhs.value), hydra.core.Field(hydra.core.Name("rhs"), rhs.value))))))

def op_expr_lhs(x: hydra.phantoms.TTerm[hydra.ast.OpExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("lhs")))))))), x.value))))

def op_expr_op(x: hydra.phantoms.TTerm[hydra.ast.OpExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("op")))))))), x.value))))

def op_expr_rhs(x: hydra.phantoms.TTerm[hydra.ast.OpExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("rhs")))))))), x.value))))

def op_expr_with_lhs(original: hydra.phantoms.TTerm[hydra.ast.OpExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("op")))))))), original.value)))), hydra.core.Field(hydra.core.Name("lhs"), new_val.value), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("rhs")))))))), original.value)))))))))

def op_expr_with_op(original: hydra.phantoms.TTerm[hydra.ast.OpExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), new_val.value), hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("lhs")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("rhs")))))))), original.value)))))))))

def op_expr_with_rhs(original: hydra.phantoms.TTerm[hydra.ast.OpExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Expr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("op")))))))), original.value)))), hydra.core.Field(hydra.core.Name("lhs"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.OpExpr"), hydra.core.Name("lhs")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rhs"), new_val.value))))))

def op_padding(x: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("padding")))))))), x.value))))

def op_precedence(x: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("precedence")))))))), x.value))))

def op_symbol(x: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("symbol")))))))), x.value))))

def op_with_associativity(original: hydra.phantoms.TTerm[hydra.ast.Op], new_val: hydra.phantoms.TTerm[hydra.ast.Associativity]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("symbol")))))))), original.value)))), hydra.core.Field(hydra.core.Name("padding"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("padding")))))))), original.value)))), hydra.core.Field(hydra.core.Name("precedence"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("precedence")))))))), original.value)))), hydra.core.Field(hydra.core.Name("associativity"), new_val.value))))))

def op_with_padding(original: hydra.phantoms.TTerm[hydra.ast.Op], new_val: hydra.phantoms.TTerm[hydra.ast.Padding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("symbol")))))))), original.value)))), hydra.core.Field(hydra.core.Name("padding"), new_val.value), hydra.core.Field(hydra.core.Name("precedence"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("precedence")))))))), original.value)))), hydra.core.Field(hydra.core.Name("associativity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("associativity")))))))), original.value)))))))))

def op_with_precedence(original: hydra.phantoms.TTerm[hydra.ast.Op], new_val: hydra.phantoms.TTerm[hydra.ast.Precedence]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("symbol")))))))), original.value)))), hydra.core.Field(hydra.core.Name("padding"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("padding")))))))), original.value)))), hydra.core.Field(hydra.core.Name("precedence"), new_val.value), hydra.core.Field(hydra.core.Name("associativity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("associativity")))))))), original.value)))))))))

def op_with_symbol(original: hydra.phantoms.TTerm[hydra.ast.Op], new_val: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), new_val.value), hydra.core.Field(hydra.core.Name("padding"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("padding")))))))), original.value)))), hydra.core.Field(hydra.core.Name("precedence"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("precedence")))))))), original.value)))), hydra.core.Field(hydra.core.Name("associativity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Op"), hydra.core.Name("associativity")))))))), original.value)))))))))

def padding(left: hydra.phantoms.TTerm[hydra.ast.Ws], right: hydra.phantoms.TTerm[hydra.ast.Ws]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Padding"), (hydra.core.Field(hydra.core.Name("left"), left.value), hydra.core.Field(hydra.core.Name("right"), right.value))))))

def padding_left(x: hydra.phantoms.TTerm[hydra.ast.Padding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Padding"), hydra.core.Name("left")))))))), x.value))))

def padding_right(x: hydra.phantoms.TTerm[hydra.ast.Padding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Padding"), hydra.core.Name("right")))))))), x.value))))

def padding_with_left(original: hydra.phantoms.TTerm[hydra.ast.Padding], new_val: hydra.phantoms.TTerm[hydra.ast.Ws]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Padding"), (hydra.core.Field(hydra.core.Name("left"), new_val.value), hydra.core.Field(hydra.core.Name("right"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Padding"), hydra.core.Name("right")))))))), original.value)))))))))

def padding_with_right(original: hydra.phantoms.TTerm[hydra.ast.Padding], new_val: hydra.phantoms.TTerm[hydra.ast.Ws]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Padding"), (hydra.core.Field(hydra.core.Name("left"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.Padding"), hydra.core.Name("left")))))))), original.value)))), hydra.core.Field(hydra.core.Name("right"), new_val.value))))))

def precedence(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Precedence"), x.value))))

def seq_expr(op: hydra.phantoms.TTerm[hydra.ast.Op], elements: hydra.phantoms.TTerm[frozenlist[hydra.ast.Expr]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.SeqExpr"), (hydra.core.Field(hydra.core.Name("op"), op.value), hydra.core.Field(hydra.core.Name("elements"), elements.value))))))

def seq_expr_elements(x: hydra.phantoms.TTerm[hydra.ast.SeqExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.SeqExpr"), hydra.core.Name("elements")))))))), x.value))))

def seq_expr_op(x: hydra.phantoms.TTerm[hydra.ast.SeqExpr]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.SeqExpr"), hydra.core.Name("op")))))))), x.value))))

def seq_expr_with_elements(original: hydra.phantoms.TTerm[hydra.ast.SeqExpr], new_val: hydra.phantoms.TTerm[frozenlist[hydra.ast.Expr]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.SeqExpr"), (hydra.core.Field(hydra.core.Name("op"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.SeqExpr"), hydra.core.Name("op")))))))), original.value)))), hydra.core.Field(hydra.core.Name("elements"), new_val.value))))))

def seq_expr_with_op(original: hydra.phantoms.TTerm[hydra.ast.SeqExpr], new_val: hydra.phantoms.TTerm[hydra.ast.Op]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.SeqExpr"), (hydra.core.Field(hydra.core.Name("op"), new_val.value), hydra.core.Field(hydra.core.Name("elements"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.ast.SeqExpr"), hydra.core.Name("elements")))))))), original.value)))))))))

def symbol(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Symbol"), x.value))))

def un_precedence(x: hydra.phantoms.TTerm[hydra.ast.Precedence]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.ast.Precedence"))))))), x.value))))

def un_symbol(x: hydra.phantoms.TTerm[hydra.ast.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.ast.Symbol"))))))), x.value))))

ws_break = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("break"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def ws_break_and_indent(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("breakAndIndent"), x.value)))))

ws_double_break = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("doubleBreak"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

ws_none = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("none"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

ws_space = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("space"), cast(hydra.core.Term, hydra.core.TermUnit()))))))
