# Note: this is an automatically generated file. Do not edit.

r"""Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation."""

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
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def sym(s: str) -> hydra.core.Type:
    return hydra.ast.Symbol(s)

def angle_braces() -> hydra.core.Type:
    return hydra.ast.Brackets(sym("<"), sym(">"))

def brackets(br: hydra.ast.Brackets, style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.core.Type:
    return cast(hydra.ast.Expr, hydra.ast.ExprBrackets(hydra.ast.BracketExpr(br, e, style)))

def cst(s: str) -> hydra.core.Type:
    return cast(hydra.ast.Expr, hydra.ast.ExprConst(sym(s)))

def ifx(op: hydra.ast.Op, lhs: hydra.ast.Expr, rhs: hydra.ast.Expr) -> hydra.core.Type:
    return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op, lhs, rhs)))

def symbol_sep(symb: str, style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def h() -> hydra.core.Type:
        return hydra.lib.lists.head(l)
    def r() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.tail(l)
    def break_count() -> int:
        return hydra.lib.lists.length(hydra.lib.lists.filter((lambda x_: x_), (style.newline_before_content, style.newline_after_content)))
    def break_() -> hydra.core.Type:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count(), 0), (lambda : cast(hydra.ast.Ws, hydra.ast.WsSpace())), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(break_count(), 1), (lambda : cast(hydra.ast.Ws, hydra.ast.WsBreak())), (lambda : cast(hydra.ast.Ws, hydra.ast.WsDoubleBreak())))))
    def comma_op() -> hydra.core.Type:
        return hydra.ast.Op(sym(symb), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), break_()), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(l), 1), (lambda : hydra.lib.lists.head(l)), (lambda : ifx(comma_op(), h(), symbol_sep(symb, style, r()))))))

def comma_sep(v1: hydra.ast.BlockStyle, v2: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return symbol_sep(",", v1, v2)

def angle_braces_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("<>")), (lambda : brackets(angle_braces(), style, comma_sep(style, els))))

def curly_braces() -> hydra.core.Type:
    return hydra.ast.Brackets(sym("{"), sym("}"))

def curly_braces_list(msymb: Maybe[str], style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("{}")), (lambda : brackets(curly_braces(), style, symbol_sep(hydra.lib.maybes.from_maybe(",", msymb), style, els))))

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
                return 1
            
            case hydra.ast.WsBreakAndIndent(value=s):
                return hydra.lib.math.add(1, hydra.lib.strings.length(s))
            
            case hydra.ast.WsDoubleBreak():
                return 2
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def block_style_length(style: hydra.ast.BlockStyle) -> int:
        def mindent_len() -> int:
            return hydra.lib.maybes.maybe(0, hydra.lib.strings.length, style.indent)
        def nl_before_len() -> int:
            return hydra.lib.logic.if_else(style.newline_before_content, (lambda : 1), (lambda : 0))
        def nl_after_len() -> int:
            return hydra.lib.logic.if_else(style.newline_after_content, (lambda : 1), (lambda : 0))
        return hydra.lib.math.add(mindent_len(), hydra.lib.math.add(nl_before_len(), nl_after_len()))
    def brackets_length(brackets: hydra.ast.Brackets) -> int:
        return hydra.lib.math.add(symbol_length(brackets.open), symbol_length(brackets.close))
    def bracket_expr_length(be: hydra.ast.BracketExpr) -> int:
        return hydra.lib.math.add(brackets_length(be.brackets), hydra.lib.math.add(expression_length(be.enclosed), block_style_length(be.style)))
    def indented_expression_length(ie: hydra.ast.IndentedExpression) -> int:
        def base_len() -> int:
            return expression_length(ie.expr)
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
        def sym_len() -> int:
            return symbol_length(op.symbol)
        def padding() -> hydra.core.Type:
            return op.padding
        def left_len() -> int:
            return ws_length(padding().left)
        def right_len() -> int:
            return ws_length(padding().right)
        return hydra.lib.math.add(sym_len(), hydra.lib.math.add(left_len(), right_len()))
    def op_expr_length(oe: hydra.ast.OpExpr) -> int:
        def op_len() -> int:
            return op_length(oe.op)
        def left_len() -> int:
            return expression_length(oe.lhs)
        def right_len() -> int:
            return expression_length(oe.rhs)
        return hydra.lib.math.add(op_len(), hydra.lib.math.add(left_len(), right_len()))
    match e:
        case hydra.ast.ExprConst(value=s):
            return symbol_length(s)
        
        case hydra.ast.ExprIndent(value=ie):
            return indented_expression_length(ie)
        
        case hydra.ast.ExprOp(value=oe):
            return op_expr_length(oe)
        
        case hydra.ast.ExprBrackets(value=be):
            return bracket_expr_length(be)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

double_space = "  "

half_block_style = hydra.ast.BlockStyle(Just(double_space), True, False)

def inline_style() -> hydra.core.Type:
    return hydra.ast.BlockStyle(Nothing(), False, False)

def braces_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    r"""Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""
    
    def inline_list() -> hydra.core.Type:
        return curly_braces_list(Nothing(), inline_style(), els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list()), 70), (lambda : curly_braces_list(Nothing(), half_block_style, els)), (lambda : inline_list()))

def square_brackets() -> hydra.core.Type:
    return hydra.ast.Brackets(sym("["), sym("]"))

def bracket_list(style: hydra.ast.BlockStyle, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("[]")), (lambda : brackets(square_brackets(), style, comma_sep(style, els))))

def bracket_list_adaptive(els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    r"""Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression."""
    
    def inline_list() -> hydra.core.Type:
        return bracket_list(inline_style(), els)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(expression_length(inline_list()), 70), (lambda : bracket_list(half_block_style, els)), (lambda : inline_list()))

def curly_block(style: hydra.ast.BlockStyle, e: hydra.ast.Expr) -> hydra.core.Type:
    return curly_braces_list(Nothing(), style, (e,))

def custom_indent(idt: str, s: str) -> str:
    return hydra.lib.strings.cat(hydra.lib.lists.intersperse("\n", hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt, line)), hydra.lib.strings.lines(s))))

def sep(op: hydra.ast.Op, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def h() -> hydra.core.Type:
        return hydra.lib.lists.head(els)
    def r() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.tail(els)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), (lambda : hydra.lib.lists.head(els)), (lambda : ifx(op, h(), sep(op, r()))))))

def newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def custom_indent_block(idt: str, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def head() -> hydra.core.Type:
        return hydra.lib.lists.head(els)
    def rest() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.tail(els)
    def idt_op() -> hydra.core.Type:
        return hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent(idt))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(els), 1), (lambda : hydra.lib.lists.head(els)), (lambda : ifx(idt_op(), head(), newline_sep(rest()))))))

def dot_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return sep(hydra.ast.Op(sym("."), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def double_newline_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsBreak()), cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

full_block_style = hydra.ast.BlockStyle(Just(double_space), True, True)

def indent(v1: str) -> str:
    return custom_indent(double_space, v1)

def indent_block(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return custom_indent_block(double_space, v1)

def indent_subsequent_lines(idt: str, e: hydra.ast.Expr) -> hydra.core.Type:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleSubsequentLines(idt)), e)))

def space_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def infix_ws(op: str, l: hydra.ast.Expr, r: hydra.ast.Expr) -> hydra.core.Type:
    return space_sep((l, cst(op), r))

def infix_ws_list(op: str, opers: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def op_expr() -> hydra.core.Type:
        return cst(op)
    def fold_fun(e: frozenlist[hydra.ast.Expr], r: hydra.ast.Expr) -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(e), (lambda : (r,)), (lambda : hydra.lib.lists.cons(r, hydra.lib.lists.cons(op_expr(), e))))
    return space_sep(hydra.lib.lists.foldl(fold_fun, (), hydra.lib.lists.reverse(opers)))

no_padding = hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone()))

def no_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return sep(hydra.ast.Op(sym(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE), v1)

def num(i: int) -> hydra.core.Type:
    return cst(hydra.lib.literals.show_int32(i))

def op(s: str, p: int, assoc: hydra.ast.Associativity) -> hydra.core.Type:
    return hydra.ast.Op(sym(s), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.ast.Precedence(p), assoc)

def or_op(newlines: bool) -> hydra.core.Type:
    return hydra.ast.Op(sym("|"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), hydra.lib.logic.if_else(newlines, (lambda : cast(hydra.ast.Ws, hydra.ast.WsBreak())), (lambda : cast(hydra.ast.Ws, hydra.ast.WsSpace())))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)

def or_sep(style: hydra.ast.BlockStyle, l: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def h() -> hydra.core.Type:
        return hydra.lib.lists.head(l)
    def r() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.tail(l)
    def newlines() -> bool:
        return style.newline_before_content
    return hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : cst("")), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(l), 1), (lambda : hydra.lib.lists.head(l)), (lambda : ifx(or_op(newlines()), h(), or_sep(style, r()))))))

def parentheses() -> hydra.core.Type:
    return hydra.ast.Brackets(sym("("), sym(")"))

def paren_list(newlines: bool, els: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    def style() -> hydra.core.Type:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(newlines, hydra.lib.equality.gt(hydra.lib.lists.length(els), 1)), (lambda : half_block_style), (lambda : inline_style()))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : cst("()")), (lambda : brackets(parentheses(), style(), comma_sep(style(), els))))

def parens(v1: hydra.ast.Expr) -> hydra.core.Type:
    return brackets(parentheses(), inline_style(), v1)

def parenthesize(exp: hydra.ast.Expr) -> hydra.core.Type:
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
            def op() -> hydra.core.Type:
                return op_expr.op
            def prec() -> int:
                return op().precedence.value
            def assoc() -> hydra.core.Type:
                return op().associativity
            def lhs() -> hydra.core.Type:
                return op_expr.lhs
            def rhs() -> hydra.core.Type:
                return op_expr.rhs
            def lhs_() -> hydra.core.Type:
                return parenthesize(lhs())
            def rhs_() -> hydra.core.Type:
                return parenthesize(rhs())
            def lhs2() -> hydra.core.Type:
                match lhs_():
                    case hydra.ast.ExprOp(value=lop_expr):
                        def lop() -> hydra.core.Type:
                            return lop_expr.op
                        def lprec() -> int:
                            return lop().precedence.value
                        def lassoc() -> hydra.core.Type:
                            return lop().associativity
                        def comparison() -> hydra.core.Type:
                            return hydra.lib.equality.compare(prec(), lprec())
                        match comparison():
                            case hydra.util.Comparison.LESS_THAN:
                                return lhs_()
                            
                            case hydra.util.Comparison.GREATER_THAN:
                                return parens(lhs_())
                            
                            case hydra.util.Comparison.EQUAL_TO:
                                return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_left(assoc()), assoc_left(lassoc())), (lambda : lhs_()), (lambda : parens(lhs_())))
                            
                            case _:
                                raise AssertionError("Unreachable: all variants handled")
                    
                    case _:
                        return lhs_()
            def rhs2() -> hydra.core.Type:
                match rhs_():
                    case hydra.ast.ExprOp(value=rop_expr):
                        def rop() -> hydra.core.Type:
                            return rop_expr.op
                        def rprec() -> int:
                            return rop().precedence.value
                        def rassoc() -> hydra.core.Type:
                            return rop().associativity
                        def comparison() -> hydra.core.Type:
                            return hydra.lib.equality.compare(prec(), rprec())
                        match comparison():
                            case hydra.util.Comparison.LESS_THAN:
                                return rhs_()
                            
                            case hydra.util.Comparison.GREATER_THAN:
                                return parens(rhs_())
                            
                            case hydra.util.Comparison.EQUAL_TO:
                                return hydra.lib.logic.if_else(hydra.lib.logic.and_(assoc_right(assoc()), assoc_right(rassoc())), (lambda : rhs_()), (lambda : parens(rhs_())))
                            
                            case _:
                                raise AssertionError("Unreachable: all variants handled")
                    
                    case _:
                        return rhs_()
            return cast(hydra.ast.Expr, hydra.ast.ExprOp(hydra.ast.OpExpr(op(), lhs2(), rhs2())))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def prefix(p: str, expr: hydra.ast.Expr) -> hydra.core.Type:
    def pre_op() -> hydra.core.Type:
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
            def style() -> hydra.core.Type:
                return indent_expr.style
            def expr() -> hydra.core.Type:
                return indent_expr.expr
            def lns() -> frozenlist[str]:
                return hydra.lib.strings.lines(print_expr(expr()))
            def ilns() -> frozenlist[str]:
                match style():
                    case hydra.ast.IndentStyleAllLines(value=idt):
                        return hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt, line)), lns())
                    
                    case hydra.ast.IndentStyleSubsequentLines(value=idt2):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lns()), 1), (lambda : lns()), (lambda : hydra.lib.lists.cons(hydra.lib.lists.head(lns()), hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2(idt2, line)), hydra.lib.lists.tail(lns())))))
                    
                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return hydra.lib.strings.intercalate("\n", ilns())
        
        case hydra.ast.ExprOp(value=op_expr):
            def op() -> hydra.core.Type:
                return op_expr.op
            def sym() -> str:
                return op().symbol.value
            def padding() -> hydra.core.Type:
                return op().padding
            def padl() -> hydra.core.Type:
                return padding().left
            def padr() -> hydra.core.Type:
                return padding().right
            def l() -> hydra.core.Type:
                return op_expr.lhs
            def r() -> hydra.core.Type:
                return op_expr.rhs
            def lhs() -> str:
                return idt(padl(), print_expr(l()))
            def rhs() -> str:
                return idt(padr(), print_expr(r()))
            return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(lhs(), pad(padl())), sym()), pad(padr())), rhs())
        
        case hydra.ast.ExprBrackets(value=bracket_expr):
            def brackets() -> hydra.core.Type:
                return bracket_expr.brackets
            def l() -> str:
                return brackets().open.value
            def r() -> str:
                return brackets().close.value
            def e() -> hydra.core.Type:
                return bracket_expr.enclosed
            def style() -> hydra.core.Type:
                return bracket_expr.style
            def body() -> str:
                return print_expr(e())
            def do_indent() -> Maybe[str]:
                return style().indent
            def nl_before() -> bool:
                return style().newline_before_content
            def nl_after() -> bool:
                return style().newline_after_content
            def ibody() -> str:
                return hydra.lib.maybes.maybe(body(), (lambda idt: custom_indent(idt, body())), do_indent())
            def pre() -> str:
                return hydra.lib.logic.if_else(nl_before(), (lambda : "\n"), (lambda : ""))
            def suf() -> str:
                return hydra.lib.logic.if_else(nl_after(), (lambda : "\n"), (lambda : ""))
            return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(l(), pre()), ibody()), suf()), r())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def semicolon_sep(v1: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return symbol_sep(";", inline_style(), v1)

def tab_indent(e: hydra.ast.Expr) -> hydra.core.Type:
    return cast(hydra.ast.Expr, hydra.ast.ExprIndent(hydra.ast.IndentedExpression(cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleAllLines("    ")), e)))

def tab_indent_double_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return tab_indent(double_newline_sep(exprs))

def tab_indent_single_space(exprs: frozenlist[hydra.ast.Expr]) -> hydra.core.Type:
    return tab_indent(newline_sep(exprs))

def unsupported_type(label: str) -> hydra.core.Type:
    return cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[", label), "]"))

def unsupported_variant(label: str, obj: str) -> hydra.core.Type:
    return cst(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("[unsupported ", label), ": "), hydra.lib.literals.show_string(obj)), "]"))

def with_comma(e: hydra.ast.Expr) -> hydra.core.Type:
    return no_sep((e, cst(",")))

def with_semi(e: hydra.ast.Expr) -> hydra.core.Type:
    return no_sep((e, cst(";")))
