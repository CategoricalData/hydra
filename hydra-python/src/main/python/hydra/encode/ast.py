# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.ast."""

from __future__ import annotations
from hydra.dsl.python import Maybe
from typing import cast
import hydra.ast
import hydra.core
import hydra.lib.maybes

def associativity(v1: hydra.ast.Associativity) -> hydra.core.Type:
    match v1:
        case hydra.ast.Associativity.NONE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("none"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.Associativity.LEFT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("left"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.Associativity.RIGHT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("right"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.Associativity.BOTH:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Associativity"), hydra.core.Field(hydra.core.Name("both"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def block_style(x: hydra.ast.BlockStyle) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BlockStyle"), (hydra.core.Field(hydra.core.Name("indent"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), opt))))(x.indent)), hydra.core.Field(hydra.core.Name("newlineBeforeContent"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x2)))))(x.newline_before_content)), hydra.core.Field(hydra.core.Name("newlineAfterContent"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x2)))))(x.newline_after_content))))))

def symbol(x: hydra.ast.Symbol) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Symbol"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def brackets(x: hydra.ast.Brackets) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Brackets"), (hydra.core.Field(hydra.core.Name("open"), symbol(x.open)), hydra.core.Field(hydra.core.Name("close"), symbol(x.close))))))

def indent_style(v1: hydra.ast.IndentStyle) -> hydra.core.Type:
    match v1:
        case hydra.ast.IndentStyleAllLines(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("allLines"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v)))))
        
        case hydra.ast.IndentStyleSubsequentLines(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.IndentStyle"), hydra.core.Field(hydra.core.Name("subsequentLines"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def ws(v1: hydra.ast.Ws) -> hydra.core.Type:
    match v1:
        case hydra.ast.WsNone():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("none"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.WsSpace():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("space"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.WsBreak():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("break"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.ast.WsBreakAndIndent(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("breakAndIndent"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v4)))))
        
        case hydra.ast.WsDoubleBreak():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Ws"), hydra.core.Field(hydra.core.Name("doubleBreak"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def padding(x: hydra.ast.Padding) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Padding"), (hydra.core.Field(hydra.core.Name("left"), ws(x.left)), hydra.core.Field(hydra.core.Name("right"), ws(x.right))))))

def precedence(x: hydra.ast.Precedence) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.ast.Precedence"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2)))))))(x.value))))

def op(x: hydra.ast.Op) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.Op"), (hydra.core.Field(hydra.core.Name("symbol"), symbol(x.symbol)), hydra.core.Field(hydra.core.Name("padding"), padding(x.padding)), hydra.core.Field(hydra.core.Name("precedence"), precedence(x.precedence)), hydra.core.Field(hydra.core.Name("associativity"), associativity(x.associativity))))))

def bracket_expr(x: hydra.ast.BracketExpr) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.BracketExpr"), (hydra.core.Field(hydra.core.Name("brackets"), brackets(x.brackets)), hydra.core.Field(hydra.core.Name("enclosed"), expr(x.enclosed)), hydra.core.Field(hydra.core.Name("style"), block_style(x.style))))))

def expr(v1: hydra.ast.Expr) -> hydra.core.Type:
    match v1:
        case hydra.ast.ExprConst(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("const"), symbol(v)))))
        
        case hydra.ast.ExprIndent(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("indent"), indented_expression(v2)))))
        
        case hydra.ast.ExprOp(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("op"), op_expr(v3)))))
        
        case hydra.ast.ExprBrackets(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.ast.Expr"), hydra.core.Field(hydra.core.Name("brackets"), bracket_expr(v4)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def indented_expression(x: hydra.ast.IndentedExpression) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.IndentedExpression"), (hydra.core.Field(hydra.core.Name("style"), indent_style(x.style)), hydra.core.Field(hydra.core.Name("expr"), expr(x.expr))))))

def op_expr(x: hydra.ast.OpExpr) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.ast.OpExpr"), (hydra.core.Field(hydra.core.Name("op"), op(x.op)), hydra.core.Field(hydra.core.Name("lhs"), expr(x.lhs)), hydra.core.Field(hydra.core.Name("rhs"), expr(x.rhs))))))
