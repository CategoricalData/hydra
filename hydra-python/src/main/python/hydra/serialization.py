# Note: this is an automatically generated file. Do not edit.

"""Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation."""

from __future__ import annotations
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.optionals
import hydra.lib.strings
import hydra.mantle

def sym(s: str) -> hydra.ast.Symbol:
    return hydra.ast.Symbol(s)

angle_braces = hydra.ast.Brackets(sym("<"), sym(">"))

def brackets(br: hydra.ast.Brackets, style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprBrackets(hydra.ast.BracketExpr(br, e, style)))

def cst(s: str) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprConst(sym(s)))

def ifx(op: hydra.ast.Op, lhs: hydra.ast.Expr, rhs: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op, lhs, rhs)))

def symbol_sep(symb: str, style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    h = hydra.lib.lists.head(l)
    r = hydra.lib.lists.tail(l)
    break_count = hydra.lib.lists.length(hydra.lib.lists.filter((lambda x_: x_), (style.newline_before_content, style.newline_after_content)))
    break_ = hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count, 0), cast(hydra.ast.Ws, hydra.ast.WsSpace(None)), hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count, 1), cast(hydra.ast.Ws, hydra.ast.WsBreak(None)), cast(hydra.ast.Ws, hydra.ast.WsDoubleBreak(None))))
    comma_op = hydra.ast.Op(sym(symb), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), break_), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(l), cst(""), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(l), 1), hydra.lib.lists.head(l), ifx(comma_op, h, symbol_sep(symb, style, r))))

def comma_sep(v1: hydra.ast.BlockStyle, v2: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return symbol_sep(",", v1, v2)

def angle_braces_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst("<>"), brackets(angle_braces, style, comma_sep(style, els)))

curly_braces = hydra.ast.Brackets(sym("{"), sym("}"))

def curly_braces_list(msymb: Maybe[str], style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst("{}"), brackets(curly_braces, style, symbol_sep(hydra.lib.optionals.from_maybe(",", msymb), style, els)))

def expression_length(e: hydra.ast.Expr) -> int:
    """Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it."""
    
    def symbol_length(s: hydra.ast.Symbol) -> int:
        return hydra.lib.strings.length(s.value)
    def ws_length(ws: hydra.ast.Ws) -> int:
        match ws:
            case hydra.ast.WsNone():
                return 0
            
            case hydra.ast.WsSpace():
                return 1
            
            case hydra.ast.WsBreak():
                return 1
            
            case hydra.ast.WsBreakAndIndent(value=s):
                return hydra.lib.math.add(1, hydra.lib.strings.length(s))
            
            case hydra.ast.WsDoubleBreak():
                return 2
    def block_style_length(style: hydra.ast.BlockStyle) -> int:
        mindent_len = hydra.lib.optionals.maybe(0, hydra.lib.strings.length, style.indent)
        nl_before_len = hydra.lib.logic.if_else(style.newline_before_content, 1, 0)
        nl_after_len = hydra.lib.logic.if_else(style.newline_after_content, 1, 0)
        return hydra.lib.math.add(mindent_len, hydra.lib.math.add(nl_before_len, nl_after_len))
    def brackets_length(brackets: hydra.ast.Brackets) -> int:
        return hydra.lib.math.add(symbol_length(brackets.open), symbol_length(brackets.close))
    def bracket_expr_length(be: hydra.ast.BracketExpr) -> int:
        return hydra.lib.math.add(brackets_length(be.brackets), hydra.lib.math.add(expression_length(be.enclosed), block_style_length(be.style)))
    def indented_expression_length(ie: hydra.ast.IndentedExpression) -> int:
        base_len = expression_length(ie.expr)
        def indent_len() -> int:
            match ie.style:
                case hydra.ast.IndentStyleAllLines(value=s):
                    return hydra.lib.strings.length(s)
                
                case hydra.ast.IndentStyleSubsequentLines(value=s2):
                    return hydra.lib.strings.length(s2)
        return hydra.lib.math.add(base_len, indent_len())
    def op_length(op: hydra.ast.Op) -> int:
        sym_len = symbol_length(op.symbol)
        padding = op.padding
        left_len = ws_length(padding.left)
        right_len = ws_length(padding.right)
        return hydra.lib.math.add(sym_len, hydra.lib.math.add(left_len, right_len))
    def op_expr_length(oe: hydra.ast.OpExpr) -> int:
        op_len = op_length(oe.op)
        left_len = expression_length(oe.lhs)
        right_len = expression_length(oe.rhs)
        return hydra.lib.math.add(op_len, hydra.lib.math.add(left_len, right_len))
    match e:
        case hydra.ast.ExprConst(value=s):
            return symbol_length(s)
        
        case hydra.ast.ExprIndent(value=ie):
            return indented_expression_length(ie)
        
        case hydra.ast.ExprOp(value=oe):
            return op_expr_length(oe)
        
        case hydra.ast.ExprBrackets(value=be):
            return bracket_expr_length(be)

double_space = "  "

half_block_style = hydra.ast.BlockStyle(Just(double_space), True, False)

inline_style = hydra.ast.BlockStyle(Nothing(), False, False)

def braces_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    """Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""
    
    inline_list = curly_braces_list(Nothing(), inline_style, els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list), 70), curly_braces_list(Nothing(), half_block_style, els), inline_list)

square_brackets = hydra.ast.Brackets(sym("["), sym("]"))

def bracket_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst("[]"), brackets(square_brackets, style, comma_sep(style, els)))

def bracket_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    """Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""
    
    inline_list = bracket_list(inline_style, els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list), 70), bracket_list(half_block_style, els), inline_list)

def curly_block(style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return curly_braces_list(Nothing(), style, (e,))

def custom_indent(idt: str, s: str) -> str:
    return hydra.lib.strings.cat(hydra.lib.lists.intersperse("\n", hydra.lib.lists.map((lambda line: hydra.lib.strings.cat((idt, line))), hydra.lib.strings.lines(s))))

def sep(op: hydra.ast.Op, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    h = hydra.lib.lists.head(els)
    r = hydra.lib.lists.tail(els)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst(""), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), hydra.lib.lists.head(els), ifx(op, h, sep(op, r))))

def newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), cast(hydra.ast.Ws, hydra.ast.WsBreak(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def custom_indent_block(idt: str, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    head = hydra.lib.lists.head(els)
    rest = hydra.lib.lists.tail(els)
    idt_op = hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace(None)), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent(idt))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst(""), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), hydra.lib.lists.head(els), ifx(idt_op, head, newline_sep(rest))))

def dot_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), cast(hydra.ast.Ws, hydra.ast.WsNone(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def double_newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsBreak(None)), cast(hydra.ast.Ws, hydra.ast.WsBreak(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

full_block_style = hydra.ast.BlockStyle(Just(double_space), True, True)

def indent(v1: str) -> str:
    return custom_indent(double_space, v1)

def indent_block(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return custom_indent_block(double_space, v1)

def indent_subsequent_lines(idt: str, e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleSubsequentLines(idt)), e)))

def space_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace(None)), cast(hydra.ast.Ws, hydra.ast.WsNone(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def infix_ws(op: str, l: hydra.ast.Expr, r: hydra.ast.Expr) -> hydra.ast.Expr:
    return space_sep((l, cst(op), r))

def infix_ws_list(op: str, opers: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    op_expr = cst(op)
    def fold_fun(e: frozenlist[hydra.ast.Expr], r: hydra.ast.Expr) -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(e), (r,), hydra.lib.lists.cons(r, hydra.lib.lists.cons(op_expr, e)))
    return space_sep(hydra.lib.lists.foldl(fold_fun, (), hydra.lib.lists.reverse(opers)))

no_padding = hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), cast(hydra.ast.Ws, hydra.ast.WsNone(None)))

def no_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), cast(hydra.ast.Ws, hydra.ast.WsNone(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def num(i: int) -> hydra.ast.Expr:
    return cst(hydra.lib.literals.show_int32(i))

def op(s: str, p: int, assoc: hydra.ast.Associativity) -> hydra.ast.Op:
    return hydra.ast.Op(sym(s), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace(None)), cast(hydra.ast.Ws, hydra.ast.WsSpace(None))), hydra.ast.Precedence(p), assoc)

def or_op(newlines: bool) -> hydra.ast.Op:
    return hydra.ast.Op(sym("|"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace(None)), hydra.lib.logic.if_else(newlines, cast(hydra.ast.Ws, hydra.ast.WsBreak(None)), cast(hydra.ast.Ws, hydra.ast.WsSpace(None)))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)

def or_sep(style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    h = hydra.lib.lists.head(l)
    r = hydra.lib.lists.tail(l)
    newlines = style.newline_before_content
    return hydra.lib.logic.if_else(hydra.lib.lists.null(l), cst(""), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(l), 1), hydra.lib.lists.head(l), ifx(or_op(newlines), h, or_sep(style, r))))

parentheses = hydra.ast.Brackets(sym("("), sym(")"))

def paren_list(newlines: bool, els: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    style = hydra.lib.logic.if_else(hydra.lib.logic.and_(newlines, hydra.lib.equality.gt(hydra.lib.lists.length(els), 1)), half_block_style, inline_style)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), cst("()"), brackets(parentheses, style, comma_sep(style, els)))

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
        
        case hydra.ast.ExprOp(value=op_expr):
            op = op_expr.op
            prec = op.precedence.value
            assoc = op.associativity
            lhs = op_expr.lhs
            rhs = op_expr.rhs
            lhs_ = parenthesize(lhs)
            rhs_ = parenthesize(rhs)
            def lhs2() -> hydra.ast.Expr:
                match lhs_:
                    case hydra.ast.ExprOp(value=lop_expr):
                        lop = lop_expr.op
                        lprec = lop.precedence.value
                        lassoc = lop.associativity
                        comparison = hydra.lib.equality.compare(prec, lprec)
                        match comparison:
                            case hydra.mantle.Comparison.LESS_THAN:
                                return lhs_
                            
                            case hydra.mantle.Comparison.GREATER_THAN:
                                return parens(lhs_)
                            
                            case hydra.mantle.Comparison.EQUAL_TO:
                                return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_left(assoc), assoc_left(lassoc)), lhs_, parens(lhs_))
                    
                    case _:
                        return lhs_
            def rhs2() -> hydra.ast.Expr:
                match rhs_:
                    case hydra.ast.ExprOp(value=rop_expr):
                        rop = rop_expr.op
                        rprec = rop.precedence.value
                        rassoc = rop.associativity
                        comparison = hydra.lib.equality.compare(prec, rprec)
                        match comparison:
                            case hydra.mantle.Comparison.LESS_THAN:
                                return rhs_
                            
                            case hydra.mantle.Comparison.GREATER_THAN:
                                return parens(rhs_)
                            
                            case hydra.mantle.Comparison.EQUAL_TO:
                                return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_right(assoc), assoc_right(rassoc)), rhs_, parens(rhs_))
                    
                    case _:
                        return rhs_
            return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op, lhs2(), rhs2())))

def prefix(p: str, expr: hydra.ast.Expr) -> hydra.ast.Expr:
    pre_op = hydra.ast.Op(sym(p), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone(None)), cast(hydra.ast.Ws, hydra.ast.WsNone(None))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return ifx(pre_op, cst(""), expr)

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
            lns = hydra.lib.strings.lines(print_expr(expr))
            def ilns() -> frozenlist[str]:
                match style:
                    case hydra.ast.IndentStyleAllLines(value=idt):
                        return hydra.lib.lists.map((lambda line: hydra.lib.strings.cat((idt, line))), lns)
                    
                    case hydra.ast.IndentStyleSubsequentLines(value=idt2):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lns), 1), lns, hydra.lib.lists.cons(hydra.lib.lists.head(lns), hydra.lib.lists.map((lambda line: hydra.lib.strings.cat((idt2, line))), hydra.lib.lists.tail(lns))))
            return hydra.lib.strings.intercalate("\n", ilns())
        
        case hydra.ast.ExprOp(value=op_expr):
            op = op_expr.op
            sym = op.symbol.value
            padding = op.padding
            padl = padding.left
            padr = padding.right
            l = op_expr.lhs
            r = op_expr.rhs
            lhs = idt(padl, print_expr(l))
            rhs = idt(padr, print_expr(r))
            return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((lhs, pad(padl))), sym)), pad(padr))), rhs))
        
        case hydra.ast.ExprBrackets(value=bracket_expr):
            brackets = bracket_expr.brackets
            l = brackets.open.value
            r = brackets.close.value
            e = bracket_expr.enclosed
            style = bracket_expr.style
            body = print_expr(e)
            do_indent = style.indent
            nl_before = style.newline_before_content
            nl_after = style.newline_after_content
            ibody = hydra.lib.optionals.maybe(body, (lambda idt: custom_indent(idt, body)), do_indent)
            pre = hydra.lib.logic.if_else(nl_before, "\n", "")
            suf = hydra.lib.logic.if_else(nl_after, "\n", "")
            return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((l, pre)), ibody)), suf)), r))

def semicolon_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return symbol_sep(";", inline_style, v1)

def tab_indent(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleAllLines("    ")), e)))

def tab_indent_double_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return tab_indent(double_newline_sep(exprs))

def tab_indent_single_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    return tab_indent(newline_sep(exprs))

def unsupported_type(label: str) -> hydra.ast.Expr:
    return cst(hydra.lib.strings.cat((hydra.lib.strings.cat(("[", label)), "]")))

def unsupported_variant(label: str, obj: str) -> hydra.ast.Expr:
    return cst(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("[unsupported ", label)), ": ")), hydra.lib.literals.show_string(obj))), "]")))

def with_comma(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return no_sep((e, cst(",")))

def with_semi(e: hydra.ast.Expr) -> hydra.ast.Expr:
    return no_sep((e, cst(";")))
