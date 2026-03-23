# Note: this is an automatically generated file. Do not edit.

r"""Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

angle_braces = hydra.ast.Brackets(hydra.ast.Symbol("<"), hydra.ast.Symbol(">"))

def brackets(br: hydra.ast.Brackets, style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprBrackets(hydra.ast.BracketExpr(br, e, style)))

def sym(s: str) -> hydra.ast.Symbol:
    return hydra.ast.Symbol(s)

def cst(s: str) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprConst(sym(s)))

def ifx(op: hydra.ast.Op, lhs: hydra.ast.Expr, rhs: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op, lhs, rhs)))

def symbol_sep(symb: str, style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    @lru_cache(1)
    def break_count() -> int:
        return hydra.lib.lists.length(hydra.lib.lists.filter((lambda x_: x_), (style.newline_before_content, style.newline_after_content)))
    @lru_cache(1)
    def break_() -> hydra.ast.Ws:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count(), 0), (lambda : cast(hydra.ast.Ws, hydra.ast.WsSpace())), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count(), 1), (lambda : cast(hydra.ast.Ws, hydra.ast.WsBreak())), (lambda : cast(hydra.ast.Ws, hydra.ast.WsDoubleBreak())))))
    @lru_cache(1)
    def comma_op() -> hydra.ast.Op:
        return hydra.ast.Op(sym(symb), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), break_()), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.maybes.maybe((lambda : cst("")), (lambda h: hydra.lib.lists.foldl((lambda acc, el: ifx(comma_op(), acc, el)), h, hydra.lib.lists.drop(1, l))), hydra.lib.lists.safe_head(l))

def comma_sep(v1: hydra.ast.BlockStyle, v2: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return symbol_sep(",", v1, v2)

def angle_braces_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("<>")), (lambda : brackets(angle_braces, style, comma_sep(style, els))))

curly_braces = hydra.ast.Brackets(hydra.ast.Symbol("{"), hydra.ast.Symbol("}"))

def curly_braces_list(msymb: Maybe[str], style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("{}")), (lambda : brackets(curly_braces, style, symbol_sep(hydra.lib.maybes.from_maybe((lambda : ","), msymb), style, els))))

def expression_length(e: hydra.ast.Expr) -> int:
    r"""Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it."""

    def symbol_length(s: hydra.ast.Symbol) -> int:
        return hydra.lib.strings.length(s.value)
    def ws_length(ws: hydra.ast.Ws) -> int:
        match ws:
            case hydra.ast.WsNone():
                return 0

            case hydra.ast.WsSpace():
                return 1

            case hydra.ast.WsBreak():
                return 10000

            case hydra.ast.WsBreakAndIndent():
                return 10000

            case hydra.ast.WsDoubleBreak():
                return 10000

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def block_style_length(style: hydra.ast.BlockStyle) -> int:
        @lru_cache(1)
        def mindent_len() -> int:
            return hydra.lib.maybes.maybe((lambda : 0), hydra.lib.strings.length, style.indent)
        @lru_cache(1)
        def nl_before_len() -> int:
            return hydra.lib.logic.if_else(style.newline_before_content, (lambda : 1), (lambda : 0))
        @lru_cache(1)
        def nl_after_len() -> int:
            return hydra.lib.logic.if_else(style.newline_after_content, (lambda : 1), (lambda : 0))
        return hydra.lib.math.add(mindent_len(), hydra.lib.math.add(nl_before_len(), nl_after_len()))
    def brackets_length(brackets: hydra.ast.Brackets) -> int:
        return hydra.lib.math.add(symbol_length(brackets.open), symbol_length(brackets.close))
    def bracket_expr_length(be: hydra.ast.BracketExpr) -> int:
        return hydra.lib.math.add(brackets_length(be.brackets), hydra.lib.math.add(expression_length(be.enclosed), block_style_length(be.style)))
    def indented_expression_length(ie: hydra.ast.IndentedExpression) -> int:
        @lru_cache(1)
        def base_len() -> int:
            return expression_length(ie.expr)
        @lru_cache(1)
        def indent_len() -> int:
            match ie.style:
                case hydra.ast.IndentStyleAllLines(value=s):
                    return hydra.lib.strings.length(s)

                case hydra.ast.IndentStyleSubsequentLines(value=s2):
                    return hydra.lib.strings.length(s2)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return hydra.lib.math.add(base_len(), indent_len())
    def op_length(op: hydra.ast.Op) -> int:
        @lru_cache(1)
        def sym_len() -> int:
            return symbol_length(op.symbol)
        padding = op.padding
        @lru_cache(1)
        def left_len() -> int:
            return ws_length(padding.left)
        @lru_cache(1)
        def right_len() -> int:
            return ws_length(padding.right)
        return hydra.lib.math.add(sym_len(), hydra.lib.math.add(left_len(), right_len()))
    def op_expr_length(oe: hydra.ast.OpExpr) -> int:
        @lru_cache(1)
        def op_len() -> int:
            return op_length(oe.op)
        @lru_cache(1)
        def left_len() -> int:
            return expression_length(oe.lhs)
        @lru_cache(1)
        def right_len() -> int:
            return expression_length(oe.rhs)
        return hydra.lib.math.add(op_len(), hydra.lib.math.add(left_len(), right_len()))
    def seq_expr_length(se: hydra.ast.SeqExpr) -> int:
        @lru_cache(1)
        def sop_len() -> int:
            return op_length(se.op)
        @lru_cache(1)
        def element_lens() -> frozenlist[int]:
            return hydra.lib.lists.map((lambda x1: expression_length(x1)), se.elements)
        @lru_cache(1)
        def total_el_len() -> int:
            return hydra.lib.lists.foldl(hydra.lib.math.add, 0, element_lens())
        @lru_cache(1)
        def num_seps() -> int:
            return hydra.lib.math.sub(hydra.lib.lists.length(se.elements), 1)
        return hydra.lib.math.add(total_el_len(), hydra.lib.math.mul(sop_len(), hydra.lib.logic.if_else(hydra.lib.equality.gt(num_seps(), 0), (lambda : num_seps()), (lambda : 0))))
    match e:
        case hydra.ast.ExprConst(value=s):
            return symbol_length(s)

        case hydra.ast.ExprIndent(value=ie):
            return indented_expression_length(ie)

        case hydra.ast.ExprOp(value=oe):
            return op_expr_length(oe)

        case hydra.ast.ExprBrackets(value=be):
            return bracket_expr_length(be)

        case hydra.ast.ExprSeq(value=se):
            return seq_expr_length(se)

        case _:
            raise AssertionError("Unreachable: all variants handled")

double_space = "  "

half_block_style = hydra.ast.BlockStyle(Just(double_space), True, False)

inline_style = hydra.ast.BlockStyle(Nothing(), False, False)

def braces_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""

    @lru_cache(1)
    def inline_list() -> hydra.ast.Expr:
        return curly_braces_list(Nothing(), inline_style, els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list()), 70), (lambda : curly_braces_list(Nothing(), half_block_style, els)), (lambda : inline_list()))

square_brackets = hydra.ast.Brackets(hydra.ast.Symbol("["), hydra.ast.Symbol("]"))

def bracket_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("[]")), (lambda : brackets(square_brackets, style, comma_sep(style, els))))

def bracket_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""

    @lru_cache(1)
    def inline_list() -> hydra.ast.Expr:
        return bracket_list(inline_style, els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list()), 70), (lambda : bracket_list(half_block_style, els)), (lambda : inline_list()))

def curly_block(style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return curly_braces_list(Nothing(), style, (e,))

def custom_indent(idt: str, s: str) -> str:
    return hydra.lib.strings.cat(hydra.lib.lists.intersperse("\n", hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt, line)), hydra.lib.strings.lines(s))))

def sep(op: hydra.ast.Op, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.maybes.maybe((lambda : cst("")), (lambda h: hydra.lib.lists.foldl((lambda acc, el: ifx(op, acc, el)), h, hydra.lib.lists.drop(1, els))), hydra.lib.lists.safe_head(els))

def newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def custom_indent_block(idt: str, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    @lru_cache(1)
    def idt_op() -> hydra.ast.Op:
        return hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent(idt))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.maybes.maybe((lambda : cst("")), (lambda head: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), (lambda : head), (lambda : ifx(idt_op(), head, newline_sep(hydra.lib.lists.drop(1, els)))))), hydra.lib.lists.safe_head(els))

def dot_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def double_newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsBreak()), cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

full_block_style = hydra.ast.BlockStyle(Just(double_space), True, True)

def indent(v1: str) -> str:
    return custom_indent(double_space, v1)

def indent_block(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return custom_indent_block(double_space, v1)

def indent_subsequent_lines(idt: str, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleSubsequentLines(idt)), e)))

def space_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def infix_ws(op: str, l: hydra.ast.Expr, r: hydra.ast.Expr) -> hydra.ast.Expr:
    return space_sep((l, cst(op), r))

def infix_ws_list(op: str, opers: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    @lru_cache(1)
    def op_expr() -> hydra.ast.Expr:
        return cst(op)
    def fold_fun(e: frozenlist[hydra.ast.Expr], r: hydra.ast.Expr) -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(e), (lambda : (r,)), (lambda : hydra.lib.lists.cons(r, hydra.lib.lists.cons(op_expr(), e))))
    return space_sep(hydra.lib.lists.foldl((lambda x1, x2: fold_fun(x1, x2)), (), hydra.lib.lists.reverse(opers)))

no_padding = hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone()))

def no_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def num(i: int) -> hydra.ast.Expr:
    return cst(hydra.lib.literals.show_int32(i))

def op(s: str, p: int, assoc: hydra.ast.Associativity) -> hydra.ast.Op:
    return hydra.ast.Op(sym(s), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(p), assoc)

def or_op(newlines: bool) -> hydra.ast.Op:
    return hydra.ast.Op(sym("|"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), hydra.lib.logic.if_else(newlines, (lambda : cast(hydra.ast.Ws, hydra.ast.WsBreak())), (lambda : cast(hydra.ast.Ws, hydra.ast.WsSpace())))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)

def or_sep(style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    newlines = style.newline_before_content
    return hydra.lib.maybes.maybe((lambda : cst("")), (lambda h: hydra.lib.lists.foldl((lambda acc, el: ifx(or_op(newlines), acc, el)), h, hydra.lib.lists.drop(1, l))), hydra.lib.lists.safe_head(l))

parentheses = hydra.ast.Brackets(hydra.ast.Symbol("("), hydra.ast.Symbol(")"))

def paren_list(newlines: bool, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    @lru_cache(1)
    def style() -> hydra.ast.BlockStyle:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(newlines, hydra.lib.equality.gt(hydra.lib.lists.length(els), 1)), (lambda : half_block_style), (lambda : inline_style))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("()")), (lambda : brackets(parentheses, style(), comma_sep(style(), els))))

def parens(v1: hydra.ast.Expr) -> hydra.ast.Expr:
    return brackets(parentheses, inline_style, v1)

def parenthesize(exp: hydra.ast.Expr) -> hydra.ast.Expr:
    def assoc_left(a: hydra.ast.Associativity) -> bool:
        match a:
            case hydra.ast.Associativity.RIGHT:
                return False

            case _:
                return True
    def assoc_right(a: hydra.ast.Associativity) -> bool:
        match a:
            case hydra.ast.Associativity.LEFT:
                return False

            case _:
                return True
    match exp:
        case hydra.ast.ExprBrackets(value=bracket_expr):
            return cast(hydra.ast.Expr, hydra.ast.ExprBrackets(hydra.ast.BracketExpr(bracket_expr.brackets, parenthesize(bracket_expr.enclosed), bracket_expr.style)))

        case hydra.ast.ExprConst():
            return exp

        case hydra.ast.ExprIndent(value=indent_expr):
            return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(indent_expr.style, parenthesize(indent_expr.expr))))

        case hydra.ast.ExprSeq(value=seq_expr):
            return cast(hydra.ast.Expr, hydra.ast.ExprSeq(hydra.ast.SeqExpr(seq_expr.op, hydra.lib.lists.map((lambda x1: parenthesize(x1)), seq_expr.elements))))

        case hydra.ast.ExprOp(value=op_expr):
            op = op_expr.op
            prec = op.precedence.value
            assoc = op.associativity
            lhs = op_expr.lhs
            rhs = op_expr.rhs
            @lru_cache(1)
            def lhs_() -> hydra.ast.Expr:
                return parenthesize(lhs)
            @lru_cache(1)
            def rhs_() -> hydra.ast.Expr:
                return parenthesize(rhs)
            @lru_cache(1)
            def lhs2():
                def _hoist_lhs2_1(v1):
                    match v1:
                        case hydra.ast.ExprOp(value=lop_expr):
                            lop = lop_expr.op
                            lprec = lop.precedence.value
                            lassoc = lop.associativity
                            @lru_cache(1)
                            def comparison() -> hydra.util.Comparison:
                                return hydra.lib.equality.compare(prec, lprec)
                            def _hoist_comparison_body_1(v12):
                                match v12:
                                    case hydra.util.Comparison.LESS_THAN:
                                        return lhs_()

                                    case hydra.util.Comparison.GREATER_THAN:
                                        return parens(lhs_())

                                    case hydra.util.Comparison.EQUAL_TO:
                                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_left(assoc), assoc_left(lassoc)), (lambda : lhs_()), (lambda : parens(lhs_())))

                                    case _:
                                        raise AssertionError("Unreachable: all variants handled")
                            return _hoist_comparison_body_1(comparison())

                        case _:
                            return lhs_()
                return _hoist_lhs2_1(lhs_())
            @lru_cache(1)
            def rhs2():
                def _hoist_rhs2_1(v1):
                    match v1:
                        case hydra.ast.ExprOp(value=rop_expr):
                            rop = rop_expr.op
                            rprec = rop.precedence.value
                            rassoc = rop.associativity
                            @lru_cache(1)
                            def comparison() -> hydra.util.Comparison:
                                return hydra.lib.equality.compare(prec, rprec)
                            def _hoist_comparison_body_1(v12):
                                match v12:
                                    case hydra.util.Comparison.LESS_THAN:
                                        return rhs_()

                                    case hydra.util.Comparison.GREATER_THAN:
                                        return parens(rhs_())

                                    case hydra.util.Comparison.EQUAL_TO:
                                        return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_right(assoc), assoc_right(rassoc)), (lambda : rhs_()), (lambda : parens(rhs_())))

                                    case _:
                                        raise AssertionError("Unreachable: all variants handled")
                            return _hoist_comparison_body_1(comparison())

                        case _:
                            return rhs_()
                return _hoist_rhs2_1(rhs_())
            return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op, lhs2(), rhs2())))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def prefix(p: str, expr: hydra.ast.Expr) -> hydra.ast.Expr:
    @lru_cache(1)
    def pre_op() -> hydra.ast.Op:
        return hydra.ast.Op(sym(p), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return ifx(pre_op(), cst(""), expr)

def print_expr(e: hydra.ast.Expr) -> str:
    def pad(ws: hydra.ast.Ws) -> str:
        match ws:
            case hydra.ast.WsNone():
                return ""

            case hydra.ast.WsSpace():
                return " "

            case hydra.ast.WsBreak():
                return "\n"

            case hydra.ast.WsBreakAndIndent():
                return "\n"

            case hydra.ast.WsDoubleBreak():
                return "\n\n"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def idt(ws: hydra.ast.Ws, s: str) -> str:
        match ws:
            case hydra.ast.WsBreakAndIndent(value=indent_str):
                return custom_indent(indent_str, s)

            case _:
                return s
    match e:
        case hydra.ast.ExprConst(value=symbol):
            return symbol.value

        case hydra.ast.ExprIndent(value=indent_expr):
            style = indent_expr.style
            expr = indent_expr.expr
            @lru_cache(1)
            def lns() -> frozenlist[str]:
                return hydra.lib.strings.lines(print_expr(expr))
            @lru_cache(1)
            def ilns():
                def _hoist_ilns_1(v1):
                    match v1:
                        case hydra.ast.IndentStyleAllLines(value=idt2):
                            return hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt2, line)), lns())

                        case hydra.ast.IndentStyleSubsequentLines(value=idt2):
                            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lns()), 1), (lambda : lns()), (lambda : hydra.lib.lists.cons(hydra.lib.lists.head(lns()), hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt2, line)), hydra.lib.lists.tail(lns())))))

                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return _hoist_ilns_1(style)
            return hydra.lib.strings.intercalate("\n", ilns())

        case hydra.ast.ExprSeq(value=seq_expr):
            sop = seq_expr.op
            ssym = sop.symbol.value
            spadding = sop.padding
            spadl = spadding.left
            spadr = spadding.right
            selements = seq_expr.elements
            @lru_cache(1)
            def separator() -> str:
                return hydra.lib.strings.cat2(hydra.lib.strings.cat2(pad(spadl), ssym), pad(spadr))
            @lru_cache(1)
            def printed_elements() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda el: idt(spadr, print_expr(el))), selements)
            return hydra.lib.strings.intercalate(separator(), printed_elements())

        case hydra.ast.ExprOp(value=op_expr):
            op = op_expr.op
            sym = op.symbol.value
            padding = op.padding
            padl = padding.left
            padr = padding.right
            l = op_expr.lhs
            r = op_expr.rhs
            @lru_cache(1)
            def lhs() -> str:
                return idt(padl, print_expr(l))
            @lru_cache(1)
            def rhs() -> str:
                return idt(padr, print_expr(r))
            return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(lhs(), pad(padl)), sym), pad(padr)), rhs())

        case hydra.ast.ExprBrackets(value=bracket_expr):
            brs = bracket_expr.brackets
            l = brs.open.value
            r = brs.close.value
            e = bracket_expr.enclosed
            style = bracket_expr.style
            @lru_cache(1)
            def body() -> str:
                return print_expr(e)
            do_indent = style.indent
            nl_before = style.newline_before_content
            nl_after = style.newline_after_content
            @lru_cache(1)
            def ibody() -> str:
                return hydra.lib.maybes.maybe((lambda : body()), (lambda idt2: custom_indent(idt2, body())), do_indent)
            @lru_cache(1)
            def pre() -> str:
                return hydra.lib.logic.if_else(nl_before, (lambda : "\n"), (lambda : ""))
            @lru_cache(1)
            def suf() -> str:
                return hydra.lib.logic.if_else(nl_after, (lambda : "\n"), (lambda : ""))
            return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(l, pre()), ibody()), suf()), r)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def semicolon_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return symbol_sep(";", inline_style, v1)

def structural_sep(op: hydra.ast.Op, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Like sep, but produces a SeqExpr instead of an OpExpr chain. SeqExpr is treated as structural layout and is not subject to parenthesization."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), (lambda : hydra.lib.lists.head(els)), (lambda : cast(hydra.ast.Expr, hydra.ast.ExprSeq(hydra.ast.SeqExpr(op, els)))))))

def structural_space_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Like spaceSep, but produces a SeqExpr. Use for structural layout that should not trigger parenthesization of children."""

    return structural_sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def suffix(s: str, expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Append a suffix string to an expression."""

    @lru_cache(1)
    def suf_op() -> hydra.ast.Op:
        return hydra.ast.Op(sym(s), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return ifx(suf_op(), expr, cst(""))

def tab_indent(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleAllLines("    ")), e)))

def tab_indent_double_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return tab_indent(double_newline_sep(exprs))

def tab_indent_single_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return tab_indent(newline_sep(exprs))

def unsupported_type(label: str) -> hydra.ast.Expr:
    return cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[", label), "]"))

def unsupported_variant(label: str, obj: str) -> hydra.ast.Expr:
    return cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[unsupported ", label), ": "), hydra.lib.literals.show_string(obj)), "]"))

def with_comma(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return no_sep((e, cst(",")))

def with_semi(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return no_sep((e, cst(";")))
