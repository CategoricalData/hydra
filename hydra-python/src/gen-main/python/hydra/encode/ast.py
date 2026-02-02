# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.ast."""

from __future__ import annotations
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.maybes

def associativity(v1: hydra.ast.Associativity) -> hydra.core.Type:
    match v1:
        case hydra.ast.Associativity.NONE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("none"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.Associativity.LEFT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("left"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.Associativity.RIGHT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("right"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.Associativity.BOTH:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("both"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def block_style(x: hydra.ast.BlockStyle) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.indent)))), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.newline_before_content))))), hydra.core.Field(hydra.core.Name("newlineAfterContent"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.newline_after_content)))))))))

def symbol(x: hydra.ast.Symbol) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Symbol"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def brackets(x: hydra.ast.Brackets) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Brackets"), (hydra.core.Field(hydra.core.Name("open"), symbol(x.open)), hydra.core.Field(hydra.core.Name("close"), symbol(x.close))))))

def indent_style(v1: hydra.ast.IndentStyle) -> hydra.core.Type:
    match v1:
        case hydra.ast.IndentStyleAllLines(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("allLines"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y))))))))
        
        case hydra.ast.IndentStyleSubsequentLines(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("subsequentLines"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y2))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def ws(v1: hydra.ast.Ws) -> hydra.core.Type:
    match v1:
        case hydra.ast.WsNone():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("none"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.WsSpace():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("space"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.WsBreak():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("break"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.ast.WsBreakAndIndent(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("breakAndIndent"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y4))))))))
        
        case hydra.ast.WsDoubleBreak():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("doubleBreak"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def padding(x: hydra.ast.Padding) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Padding"), (hydra.core.Field(hydra.core.Name("left"), ws(x.left)), hydra.core.Field(hydra.core.Name("right"), ws(x.right))))))

def precedence(x: hydra.ast.Precedence) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Precedence"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x.value)))))))))

def op(x: hydra.ast.Op) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), symbol(x.symbol)), hydra.core.Field(hydra.core.Name("padding"), padding(x.padding)), hydra.core.Field(hydra.core.Name("precedence"), precedence(x.precedence)), hydra.core.Field(hydra.core.Name("associativity"), associativity(x.associativity))))))

def bracket_expr(x: hydra.ast.BracketExpr) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), brackets(x.brackets)), hydra.core.Field(hydra.core.Name("enclosed"), expr(x.enclosed)), hydra.core.Field(hydra.core.Name("style"), block_style(x.style))))))

def expr(v1: hydra.ast.Expr) -> hydra.core.Type:
    match v1:
        case hydra.ast.ExprConst(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("const"), symbol(y)))))
        
        case hydra.ast.ExprIndent(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("indent"), indented_expression(y2)))))
        
        case hydra.ast.ExprOp(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("op"), op_expr(y3)))))
        
        case hydra.ast.ExprBrackets(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("brackets"), bracket_expr(y4)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def indented_expression(x: hydra.ast.IndentedExpression) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.IndentedExpression"), (hydra.core.Field(hydra.core.Name("style"), indent_style(x.style)), hydra.core.Field(hydra.core.Name("expr"), expr(x.expr))))))

def op_expr(x: hydra.ast.OpExpr) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), op(x.op)), hydra.core.Field(hydra.core.Name("lhs"), expr(x.lhs)), hydra.core.Field(hydra.core.Name("rhs"), expr(x.rhs))))))
