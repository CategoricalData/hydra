# Note: this is an automatically generated file. Do not edit.

r"""Haskell operator precendence and associativity are drawn from:
https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
Other operators were investigated using GHCi, e.g. ":info (->)"
Operator names are drawn (loosely) from:
https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Nothing, frozenlist
from typing import cast
import hydra.ast
import hydra.constants
import hydra.core
import hydra.ext.haskell.ast
import hydra.ext.haskell.operators
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

def write_qualified_name(qname: hydra.ext.haskell.ast.QualifiedName) -> str:
    r"""Write a qualified name as a string."""
    
    @lru_cache(1)
    def qualifiers() -> frozenlist[hydra.ext.haskell.ast.NamePart]:
        return qname.qualifiers
    @lru_cache(1)
    def unqual() -> hydra.ext.haskell.ast.NamePart:
        return qname.unqualified
    def h(name_part: hydra.ext.haskell.ast.NamePart) -> str:
        return name_part.value
    @lru_cache(1)
    def all_parts() -> frozenlist[str]:
        return hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x1: h(x1)), qualifiers()), (h(unqual()),))
    return hydra.lib.strings.intercalate(".", all_parts())

def name_to_expr(name: hydra.ext.haskell.ast.Name) -> hydra.ast.Expr:
    def _hoist_hydra_ext_haskell_serde_name_to_expr_1(v1: hydra.ext.haskell.ast.Name) -> str:
        match v1:
            case hydra.ext.haskell.ast.NameImplicit(value=qn):
                return hydra.lib.strings.cat2("?", write_qualified_name(qn))
            
            case hydra.ext.haskell.ast.NameNormal(value=qn2):
                return write_qualified_name(qn2)
            
            case hydra.ext.haskell.ast.NameParens(value=qn3):
                return hydra.lib.strings.cat(("(", write_qualified_name(qn3), ")"))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_ext_haskell_serde_name_to_expr_1(name))

def literal_to_expr(lit: hydra.ext.haskell.ast.Literal) -> hydra.ast.Expr:
    r"""Convert a literal value to an AST expression."""
    
    def parens_if_neg(b: bool, e: str) -> str:
        return hydra.lib.logic.if_else(b, (lambda : hydra.lib.strings.cat(("(", e, ")"))), (lambda : e))
    def _hoist_body_1(v1: hydra.ext.haskell.ast.Literal) -> str:
        match v1:
            case hydra.ext.haskell.ast.LiteralChar(value=c):
                return hydra.lib.literals.show_string(hydra.lib.literals.show_uint16(c))
            
            case hydra.ext.haskell.ast.LiteralDouble(value=d):
                return parens_if_neg(hydra.lib.equality.lt(d, 0.0), hydra.lib.literals.show_float64(d))
            
            case hydra.ext.haskell.ast.LiteralFloat(value=f):
                return parens_if_neg(hydra.lib.equality.lt(f, 0.0), hydra.lib.literals.show_float32(f))
            
            case hydra.ext.haskell.ast.LiteralInt(value=i):
                return parens_if_neg(hydra.lib.equality.lt(i, 0), hydra.lib.literals.show_int32(i))
            
            case hydra.ext.haskell.ast.LiteralInteger(value=i2):
                return parens_if_neg(hydra.lib.equality.lt(i2, 0), hydra.lib.literals.show_bigint(i2))
            
            case hydra.ext.haskell.ast.LiteralString(value=s):
                return hydra.lib.literals.show_string(s)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_body_1(lit))

def application_pattern_to_expr(app_pat: hydra.ext.haskell.ast.ApplicationPattern) -> hydra.ast.Expr:
    r"""Convert an application pattern to an AST expression."""
    
    @lru_cache(1)
    def name() -> hydra.ext.haskell.ast.Name:
        return app_pat.name
    @lru_cache(1)
    def pats() -> frozenlist[hydra.ext.haskell.ast.Pattern]:
        return app_pat.args
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats())))

def pattern_to_expr(pat: hydra.ext.haskell.ast.Pattern) -> hydra.ast.Expr:
    r"""Convert a pattern to an AST expression."""
    
    match pat:
        case hydra.ext.haskell.ast.PatternApplication(value=app):
            return application_pattern_to_expr(app)
        
        case hydra.ext.haskell.ast.PatternList(value=pats):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats))
        
        case hydra.ext.haskell.ast.PatternLiteral(value=lit):
            return literal_to_expr(lit)
        
        case hydra.ext.haskell.ast.PatternName(value=name):
            return name_to_expr(name)
        
        case hydra.ext.haskell.ast.PatternParens(value=pat_):
            return hydra.serialization.parenthesize(pattern_to_expr(pat_))
        
        case hydra.ext.haskell.ast.PatternTuple(value=pats2):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats2))
        
        case hydra.ext.haskell.ast.PatternWildcard():
            return hydra.serialization.cst("_")
        
        case _:
            raise TypeError("Unsupported Pattern")

def assertion_to_expr(sert: hydra.ext.haskell.ast.Assertion) -> hydra.ast.Expr:
    r"""Convert a type class assertion to an AST expression."""
    
    match sert:
        case hydra.ext.haskell.ast.AssertionClass(value=cls):
            return class_assertion_to_expr(cls)
        
        case hydra.ext.haskell.ast.AssertionTuple(value=serts):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: assertion_to_expr(x1)), serts))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def class_assertion_to_expr(cls_asrt: hydra.ext.haskell.ast.ClassAssertion) -> hydra.ast.Expr:
    r"""Convert a class assertion to an AST expression."""
    
    @lru_cache(1)
    def name() -> hydra.ext.haskell.ast.Name:
        return cls_asrt.name
    @lru_cache(1)
    def types() -> frozenlist[hydra.ext.haskell.ast.Type]:
        return cls_asrt.types
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), (hydra.serialization.comma_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types())),)))

def type_to_expr(htype: hydra.ext.haskell.ast.Type) -> hydra.ast.Expr:
    r"""Convert a Haskell type to an AST expression."""
    
    match htype:
        case hydra.ext.haskell.ast.TypeApplication(value=app_type):
            @lru_cache(1)
            def lhs() -> hydra.ext.haskell.ast.Type:
                return app_type.context
            @lru_cache(1)
            def rhs() -> hydra.ext.haskell.ast.Type:
                return app_type.argument
            return hydra.serialization.ifx(hydra.ext.haskell.operators.app_op, type_to_expr(lhs()), type_to_expr(rhs()))
        
        case hydra.ext.haskell.ast.TypeCtx(value=ctx_type):
            @lru_cache(1)
            def ctx() -> hydra.ext.haskell.ast.Assertion:
                return ctx_type.ctx
            @lru_cache(1)
            def typ() -> hydra.ext.haskell.ast.Type:
                return ctx_type.type
            return hydra.serialization.ifx(hydra.ext.haskell.operators.assert_op, assertion_to_expr(ctx()), type_to_expr(typ()))
        
        case hydra.ext.haskell.ast.TypeFunction(value=fun_type):
            @lru_cache(1)
            def dom() -> hydra.ext.haskell.ast.Type:
                return fun_type.domain
            @lru_cache(1)
            def cod() -> hydra.ext.haskell.ast.Type:
                return fun_type.codomain
            return hydra.serialization.ifx(hydra.ext.haskell.operators.arrow_op, type_to_expr(dom()), type_to_expr(cod()))
        
        case hydra.ext.haskell.ast.TypeList(value=htype_):
            return hydra.serialization.bracket_list(hydra.serialization.inline_style(), (type_to_expr(htype_),))
        
        case hydra.ext.haskell.ast.TypeTuple(value=types):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types))
        
        case hydra.ext.haskell.ast.TypeVariable(value=name):
            return name_to_expr(name)
        
        case _:
            raise TypeError("Unsupported Type")

def type_signature_to_expr(type_sig: hydra.ext.haskell.ast.TypeSignature) -> hydra.ast.Expr:
    r"""Convert a type signature to an AST expression."""
    
    @lru_cache(1)
    def name() -> hydra.ext.haskell.ast.Name:
        return type_sig.name
    @lru_cache(1)
    def typ() -> hydra.ext.haskell.ast.Type:
        return type_sig.type
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), hydra.lib.lists.cons(hydra.serialization.cst("::"), (type_to_expr(typ()),))))

def alternative_to_expr(alt: hydra.ext.haskell.ast.Alternative) -> hydra.ast.Expr:
    r"""Convert a pattern-matching alternative to an AST expression."""
    
    return hydra.serialization.ifx(hydra.ext.haskell.operators.case_op, pattern_to_expr(alt.pattern), case_rhs_to_expr(alt.rhs))

def application_expression_to_expr(app: hydra.ext.haskell.ast.ApplicationExpression) -> hydra.ast.Expr:
    r"""Convert a function application expression to an AST expression."""
    
    return hydra.serialization.ifx(hydra.ext.haskell.operators.app_op, expression_to_expr(app.function), expression_to_expr(app.argument))

def case_expression_to_expr(case_expr: hydra.ext.haskell.ast.CaseExpression) -> hydra.ast.Expr:
    r"""Convert a case expression to an AST expression."""
    
    @lru_cache(1)
    def cs() -> hydra.ext.haskell.ast.Expression:
        return case_expr.case
    @lru_cache(1)
    def alts() -> frozenlist[hydra.ext.haskell.ast.Alternative]:
        return case_expr.alternatives
    of_op = hydra.ast.Op(hydra.ast.Symbol("of"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent("  "))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    @lru_cache(1)
    def lhs() -> hydra.ast.Expr:
        return hydra.serialization.space_sep((hydra.serialization.cst("case"), expression_to_expr(cs())))
    @lru_cache(1)
    def rhs() -> hydra.ast.Expr:
        return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: alternative_to_expr(x1)), alts()))
    return hydra.serialization.ifx(of_op, lhs(), rhs())

def case_rhs_to_expr(rhs: hydra.ext.haskell.ast.CaseRhs) -> hydra.ast.Expr:
    r"""Convert a case right-hand side to an AST expression."""
    
    return expression_to_expr(rhs.value)

def construct_record_expression_to_expr(construct_record: hydra.ext.haskell.ast.ConstructRecordExpression) -> hydra.ast.Expr:
    r"""Convert a record construction expression to an AST expression."""
    
    @lru_cache(1)
    def name() -> hydra.ext.haskell.ast.Name:
        return construct_record.name
    @lru_cache(1)
    def updates() -> frozenlist[hydra.ext.haskell.ast.FieldUpdate]:
        return construct_record.fields
    def from_update(update: hydra.ext.haskell.ast.FieldUpdate) -> hydra.ast.Expr:
        @lru_cache(1)
        def fn() -> hydra.ext.haskell.ast.Name:
            return update.name
        @lru_cache(1)
        def val() -> hydra.ext.haskell.ast.Expression:
            return update.value
        return hydra.serialization.ifx(hydra.ext.haskell.operators.define_op, name_to_expr(fn()), expression_to_expr(val()))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.comma_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: from_update(x1)), updates()))
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), (hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.half_block_style, body()),)))

def expression_to_expr(expr: hydra.ext.haskell.ast.Expression) -> hydra.ast.Expr:
    r"""Convert a Haskell expression to an AST expression."""
    
    match expr:
        case hydra.ext.haskell.ast.ExpressionApplication(value=app):
            return application_expression_to_expr(app)
        
        case hydra.ext.haskell.ast.ExpressionCase(value=cases):
            return case_expression_to_expr(cases)
        
        case hydra.ext.haskell.ast.ExpressionConstructRecord(value=r):
            return construct_record_expression_to_expr(r)
        
        case hydra.ext.haskell.ast.ExpressionDo(value=statements):
            return hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst("do"), hydra.lib.lists.map((lambda x1: statement_to_expr(x1)), statements)))
        
        case hydra.ext.haskell.ast.ExpressionIf(value=ifte):
            return if_expression_to_expr(ifte)
        
        case hydra.ext.haskell.ast.ExpressionLiteral(value=lit):
            return literal_to_expr(lit)
        
        case hydra.ext.haskell.ast.ExpressionLambda(value=lam):
            return hydra.serialization.parenthesize(lambda_expression_to_expr(lam))
        
        case hydra.ext.haskell.ast.ExpressionLet(value=let_expr):
            @lru_cache(1)
            def bindings() -> frozenlist[hydra.ext.haskell.ast.LocalBinding]:
                return let_expr.bindings
            @lru_cache(1)
            def inner() -> hydra.ext.haskell.ast.Expression:
                return let_expr.inner
            def encode_binding(binding: hydra.ext.haskell.ast.LocalBinding) -> hydra.ast.Expr:
                return hydra.serialization.indent_subsequent_lines("      ", local_binding_to_expr(binding))
            return hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst(""), hydra.lib.lists.cons(hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("let"), (hydra.serialization.custom_indent_block("    ", hydra.lib.lists.map((lambda x1: encode_binding(x1)), bindings())),))), (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("in"), (expression_to_expr(inner()),))),))))
        
        case hydra.ext.haskell.ast.ExpressionList(value=exprs):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), exprs))
        
        case hydra.ext.haskell.ast.ExpressionParens(value=expr_):
            return hydra.serialization.parenthesize(expression_to_expr(expr_))
        
        case hydra.ext.haskell.ast.ExpressionTuple(value=exprs2):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), exprs2))
        
        case hydra.ext.haskell.ast.ExpressionVariable(value=name):
            return name_to_expr(name)
        
        case _:
            raise TypeError("Unsupported Expression")

def if_expression_to_expr(if_expr: hydra.ext.haskell.ast.IfExpression) -> hydra.ast.Expr:
    r"""Convert an if-then-else expression to an AST expression."""
    
    @lru_cache(1)
    def eif() -> hydra.ext.haskell.ast.Expression:
        return if_expr.condition
    @lru_cache(1)
    def ethen() -> hydra.ext.haskell.ast.Expression:
        return if_expr.then
    @lru_cache(1)
    def eelse() -> hydra.ext.haskell.ast.Expression:
        return if_expr.else_
    if_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent("  "))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("then"), (expression_to_expr(ethen()),))), (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("else"), (expression_to_expr(eelse()),))),)))
    return hydra.serialization.ifx(if_op, hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("if"), (expression_to_expr(eif()),))), body())

def lambda_expression_to_expr(lambda_expr: hydra.ext.haskell.ast.LambdaExpression) -> hydra.ast.Expr:
    r"""Convert a lambda expression to an AST expression."""
    
    @lru_cache(1)
    def bindings() -> frozenlist[hydra.ext.haskell.ast.Pattern]:
        return lambda_expr.bindings
    @lru_cache(1)
    def inner() -> hydra.ext.haskell.ast.Expression:
        return lambda_expr.inner
    @lru_cache(1)
    def head() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), bindings()))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return expression_to_expr(inner())
    return hydra.serialization.ifx(hydra.ext.haskell.operators.lambda_op, hydra.serialization.prefix("\\", head()), body())

def local_binding_to_expr(binding: hydra.ext.haskell.ast.LocalBinding) -> hydra.ast.Expr:
    r"""Convert a local binding to an AST expression."""
    
    match binding:
        case hydra.ext.haskell.ast.LocalBindingSignature(value=ts):
            return type_signature_to_expr(ts)
        
        case hydra.ext.haskell.ast.LocalBindingValue(value=vb):
            return value_binding_to_expr(vb)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def right_hand_side_to_expr(rhs: hydra.ext.haskell.ast.RightHandSide) -> hydra.ast.Expr:
    r"""Convert a right-hand side to an AST expression."""
    
    return expression_to_expr(rhs.value)

def statement_to_expr(stmt: hydra.ext.haskell.ast.Statement) -> hydra.ast.Expr:
    r"""Convert a statement to an AST expression."""
    
    return expression_to_expr(stmt.value)

def value_binding_to_expr(vb: hydra.ext.haskell.ast.ValueBinding) -> hydra.ast.Expr:
    r"""Convert a value binding to an AST expression."""
    
    match vb:
        case hydra.ext.haskell.ast.ValueBindingSimple(value=simple_v_b):
            @lru_cache(1)
            def pat() -> hydra.ext.haskell.ast.Pattern:
                return simple_v_b.pattern
            @lru_cache(1)
            def rhs() -> hydra.ext.haskell.ast.RightHandSide:
                return simple_v_b.rhs
            @lru_cache(1)
            def local() -> Maybe[hydra.ext.haskell.ast.LocalBindings]:
                return simple_v_b.local_bindings
            @lru_cache(1)
            def body() -> hydra.ast.Expr:
                return hydra.serialization.ifx(hydra.ext.haskell.operators.define_op, pattern_to_expr(pat()), right_hand_side_to_expr(rhs()))
            return hydra.lib.maybes.maybe(body(), (lambda local_bindings: (bindings := local_bindings.value, hydra.serialization.indent_block(hydra.lib.lists.cons(body(), (hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst("where"), hydra.lib.lists.map((lambda x1: local_binding_to_expr(x1)), bindings))),))))[1]), local())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def field_to_expr(field: hydra.ext.haskell.ast.Field) -> hydra.ast.Expr:
    r"""Convert a field declaration to an AST expression."""
    
    @lru_cache(1)
    def name() -> hydra.ext.haskell.ast.Name:
        return field.name
    @lru_cache(1)
    def typ() -> hydra.ext.haskell.ast.Type:
        return field.type
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), hydra.lib.lists.cons(hydra.serialization.cst("::"), (type_to_expr(typ()),))))

def to_haskell_comments(c: str) -> str:
    r"""Convert a string to Haddock documentation comments."""
    
    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("-- | ", s)), hydra.lib.strings.lines(c)))

def field_with_comments_to_expr(field_with_comments: hydra.ext.haskell.ast.FieldWithComments) -> hydra.ast.Expr:
    r"""Convert a field with comments to an AST expression."""
    
    @lru_cache(1)
    def field() -> hydra.ext.haskell.ast.Field:
        return field_with_comments.field
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return field_with_comments.comments
    return hydra.lib.maybes.maybe(field_to_expr(field()), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (field_to_expr(field()),)))), mc())

def constructor_to_expr(cons: hydra.ext.haskell.ast.Constructor) -> hydra.ast.Expr:
    r"""Convert a data constructor to an AST expression."""
    
    match cons:
        case hydra.ext.haskell.ast.ConstructorOrdinary(value=ord):
            @lru_cache(1)
            def name() -> hydra.ext.haskell.ast.Name:
                return ord.name
            @lru_cache(1)
            def types() -> frozenlist[hydra.ext.haskell.ast.Type]:
                return ord.fields
            return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), (hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types())),)))
        
        case hydra.ext.haskell.ast.ConstructorRecord(value=rec):
            @lru_cache(1)
            def name() -> hydra.ext.haskell.ast.Name:
                return rec.name
            @lru_cache(1)
            def fields() -> frozenlist[hydra.ext.haskell.ast.FieldWithComments]:
                return rec.fields
            return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name()), (hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: field_with_comments_to_expr(x1)), fields())),)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def constructor_with_comments_to_expr(cons_with_comments: hydra.ext.haskell.ast.ConstructorWithComments) -> hydra.ast.Expr:
    r"""Convert a data constructor with comments to an AST expression."""
    
    @lru_cache(1)
    def body() -> hydra.ext.haskell.ast.Constructor:
        return cons_with_comments.body
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return cons_with_comments.comments
    return hydra.lib.maybes.maybe(constructor_to_expr(body()), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (constructor_to_expr(body()),)))), mc())

def data_or_newtype_to_expr(kw: hydra.ext.haskell.ast.DataOrNewtype) -> hydra.ast.Expr:
    r"""Convert a data/newtype keyword to an AST expression."""
    
    match kw:
        case hydra.ext.haskell.ast.DataOrNewtype.DATA:
            return hydra.serialization.cst("data")
        
        case hydra.ext.haskell.ast.DataOrNewtype.NEWTYPE:
            return hydra.serialization.cst("newtype")
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def variable_to_expr(variable: hydra.ext.haskell.ast.Variable) -> hydra.ast.Expr:
    r"""Convert a type variable to an AST expression."""
    
    return name_to_expr(variable.value)

def declaration_head_to_expr(hd: hydra.ext.haskell.ast.DeclarationHead) -> hydra.ast.Expr:
    r"""Convert a declaration head to an AST expression."""
    
    match hd:
        case hydra.ext.haskell.ast.DeclarationHeadApplication(value=app_head):
            @lru_cache(1)
            def fun() -> hydra.ext.haskell.ast.DeclarationHead:
                return app_head.function
            @lru_cache(1)
            def op() -> hydra.ext.haskell.ast.Variable:
                return app_head.operand
            return hydra.serialization.space_sep(hydra.lib.lists.cons(declaration_head_to_expr(fun()), (variable_to_expr(op()),)))
        
        case hydra.ext.haskell.ast.DeclarationHeadSimple(value=name):
            return name_to_expr(name)
        
        case _:
            raise TypeError("Unsupported DeclarationHead")

def declaration_to_expr(decl: hydra.ext.haskell.ast.Declaration) -> hydra.ast.Expr:
    r"""Convert a declaration to an AST expression."""
    
    match decl:
        case hydra.ext.haskell.ast.DeclarationData(value=data_decl):
            @lru_cache(1)
            def kw() -> hydra.ext.haskell.ast.DataOrNewtype:
                return data_decl.keyword
            @lru_cache(1)
            def hd() -> hydra.ext.haskell.ast.DeclarationHead:
                return data_decl.head
            @lru_cache(1)
            def cons() -> frozenlist[hydra.ext.haskell.ast.ConstructorWithComments]:
                return data_decl.constructors
            @lru_cache(1)
            def deriv() -> frozenlist[hydra.ext.haskell.ast.Deriving]:
                return data_decl.deriving
            @lru_cache(1)
            def deriv_cat() -> frozenlist[hydra.ext.haskell.ast.Name]:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda v1: v1.value), deriv()))
            @lru_cache(1)
            def constructors() -> hydra.ast.Expr:
                return hydra.serialization.or_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: constructor_with_comments_to_expr(x1)), cons()))
            @lru_cache(1)
            def deriving_clause() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(deriv_cat()), (lambda : ()), (lambda : (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("deriving"), (hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: name_to_expr(x1)), deriv_cat())),))),)))
            @lru_cache(1)
            def main_parts() -> frozenlist[hydra.ast.Expr]:
                return (hydra.serialization.space_sep(hydra.lib.lists.cons(data_or_newtype_to_expr(kw()), hydra.lib.lists.cons(declaration_head_to_expr(hd()), (hydra.serialization.cst("="),)))), constructors())
            return hydra.serialization.indent_block(hydra.lib.lists.concat2(main_parts(), deriving_clause()))
        
        case hydra.ext.haskell.ast.DeclarationType(value=type_decl):
            @lru_cache(1)
            def hd() -> hydra.ext.haskell.ast.DeclarationHead:
                return type_decl.name
            @lru_cache(1)
            def typ() -> hydra.ext.haskell.ast.Type:
                return type_decl.type
            return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("type"), hydra.lib.lists.cons(declaration_head_to_expr(hd()), hydra.lib.lists.cons(hydra.serialization.cst("="), (type_to_expr(typ()),)))))
        
        case hydra.ext.haskell.ast.DeclarationValueBinding(value=vb):
            return value_binding_to_expr(vb)
        
        case hydra.ext.haskell.ast.DeclarationTypedBinding(value=typed_binding):
            @lru_cache(1)
            def type_sig() -> hydra.ext.haskell.ast.TypeSignature:
                return typed_binding.type_signature
            @lru_cache(1)
            def vb() -> hydra.ext.haskell.ast.ValueBinding:
                return typed_binding.value_binding
            @lru_cache(1)
            def name() -> hydra.ext.haskell.ast.Name:
                return type_sig().name
            @lru_cache(1)
            def htype() -> hydra.ext.haskell.ast.Type:
                return type_sig().type
            return hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.ifx(hydra.ext.haskell.operators.type_op, name_to_expr(name()), type_to_expr(htype())), (value_binding_to_expr(vb()),)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def declaration_with_comments_to_expr(decl_with_comments: hydra.ext.haskell.ast.DeclarationWithComments) -> hydra.ast.Expr:
    r"""Convert a declaration with comments to an AST expression."""
    
    @lru_cache(1)
    def body() -> hydra.ext.haskell.ast.Declaration:
        return decl_with_comments.body
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return decl_with_comments.comments
    return hydra.lib.maybes.maybe(declaration_to_expr(body()), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (declaration_to_expr(body()),)))), mc())

def import_export_spec_to_expr(spec: hydra.ext.haskell.ast.ImportExportSpec) -> hydra.ast.Expr:
    r"""Convert an import/export specification to an AST expression."""
    
    return name_to_expr(spec.name)

def import_to_expr(import_: hydra.ext.haskell.ast.Import) -> hydra.ast.Expr:
    r"""Convert an import statement to an AST expression."""
    
    @lru_cache(1)
    def qual() -> bool:
        return import_.qualified
    @lru_cache(1)
    def mod_name() -> hydra.ext.haskell.ast.ModuleName:
        return import_.module
    @lru_cache(1)
    def mod() -> Maybe[hydra.ext.haskell.ast.ModuleName]:
        return import_.as_
    @lru_cache(1)
    def mspec() -> Maybe[hydra.ext.haskell.ast.SpecImport]:
        return import_.spec
    @lru_cache(1)
    def name() -> str:
        return mod_name().value
    def hiding_sec(spec: hydra.ext.haskell.ast.SpecImport) -> hydra.ast.Expr:
        match spec:
            case hydra.ext.haskell.ast.SpecImportHiding(value=names):
                return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("hiding "), (hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style(), hydra.lib.lists.map((lambda x1: import_export_spec_to_expr(x1)), names))),)))
            
            case _:
                raise TypeError("Unsupported SpecImport")
    @lru_cache(1)
    def parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.cat((Just(hydra.serialization.cst("import")), hydra.lib.logic.if_else(qual(), (lambda : Just(hydra.serialization.cst("qualified"))), (lambda : Nothing())), Just(hydra.serialization.cst(name())), hydra.lib.maybes.map((lambda m: hydra.serialization.cst(hydra.lib.strings.cat2("as ", m.value))), mod()), hydra.lib.maybes.map((lambda x1: hiding_sec(x1)), mspec())))
    return hydra.serialization.space_sep(parts())

def module_head_to_expr(module_head: hydra.ext.haskell.ast.ModuleHead) -> hydra.ast.Expr:
    r"""Convert a module head to an AST expression."""
    
    @lru_cache(1)
    def mc() -> Maybe[str]:
        return module_head.comments
    @lru_cache(1)
    def mod_name() -> hydra.ext.haskell.ast.ModuleName:
        return module_head.name
    @lru_cache(1)
    def mname() -> str:
        return mod_name().value
    @lru_cache(1)
    def head() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("module"), hydra.lib.lists.cons(hydra.serialization.cst(mname()), (hydra.serialization.cst("where"),))))
    return hydra.lib.maybes.maybe(head(), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), hydra.lib.lists.cons(hydra.serialization.cst(""), (head(),))))), mc())

def to_simple_comments(c: str) -> str:
    r"""Convert a string to simple line comments."""
    
    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("-- ", s)), hydra.lib.strings.lines(c)))

def module_to_expr(module: hydra.ext.haskell.ast.Module) -> hydra.ast.Expr:
    r"""Convert a Haskell module to an AST expression."""
    
    @lru_cache(1)
    def mh() -> Maybe[hydra.ext.haskell.ast.ModuleHead]:
        return module.head
    @lru_cache(1)
    def imports() -> frozenlist[hydra.ext.haskell.ast.Import]:
        return module.imports
    @lru_cache(1)
    def decls() -> frozenlist[hydra.ext.haskell.ast.DeclarationWithComments]:
        return module.declarations
    @lru_cache(1)
    def warning() -> frozenlist[hydra.ast.Expr]:
        return (hydra.serialization.cst(to_simple_comments(hydra.constants.warning_auto_generated_file)),)
    @lru_cache(1)
    def header_line() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((), (lambda h: (module_head_to_expr(h),)), mh())
    @lru_cache(1)
    def decl_lines() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: declaration_with_comments_to_expr(x1)), decls())
    @lru_cache(1)
    def import_lines() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(imports()), (lambda : ()), (lambda : (hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: import_to_expr(x1)), imports())),)))
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat((warning(), header_line(), import_lines(), decl_lines())))
