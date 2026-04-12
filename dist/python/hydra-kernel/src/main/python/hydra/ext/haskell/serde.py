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
from typing import TypeVar, cast
import hydra.ast
import hydra.constants
import hydra.core
import hydra.ext.haskell.operators
import hydra.ext.haskell.syntax
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.serialization

T0 = TypeVar("T0")

def write_qualified_name(qname: hydra.ext.haskell.syntax.QualifiedName) -> str:
    r"""Write a qualified name as a string."""

    qualifiers = qname.qualifiers
    unqual = qname.unqualified
    def h(name_part: hydra.ext.haskell.syntax.NamePart) -> str:
        return name_part.value
    @lru_cache(1)
    def all_parts() -> frozenlist[str]:
        return hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x1: h(x1)), qualifiers), (h(unqual),))
    return hydra.lib.strings.intercalate(".", all_parts())

def name_to_expr(name: hydra.ext.haskell.syntax.Name):
    def _hoist_hydra_ext_haskell_serde_name_to_expr_1(v1):
        match v1:
            case hydra.ext.haskell.syntax.NameImplicit(value=qn):
                return hydra.lib.strings.cat2("?", write_qualified_name(qn))

            case hydra.ext.haskell.syntax.NameNormal(value=qn):
                return write_qualified_name(qn)

            case hydra.ext.haskell.syntax.NameParens(value=qn):
                return hydra.lib.strings.cat(("(", write_qualified_name(qn), ")"))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_ext_haskell_serde_name_to_expr_1(name))

def literal_to_expr(lit: hydra.ext.haskell.syntax.Literal):
    r"""Convert a literal value to an AST expression."""

    def parens_if_neg(b: bool, e: str) -> str:
        return hydra.lib.logic.if_else(b, (lambda : hydra.lib.strings.cat(("(", e, ")"))), (lambda : e))
    def show_float(show_fn: Callable[[T0], str], v: T0) -> str:
        @lru_cache(1)
        def raw() -> str:
            return show_fn(v)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(raw(), "NaN"), (lambda : "(0/0)"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(raw(), "Infinity"), (lambda : "(1/0)"), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(raw(), "-Infinity"), (lambda : "(-(1/0))"), (lambda : parens_if_neg(hydra.lib.equality.equal(hydra.lib.strings.char_at(0, raw()), 45), raw())))))))
    def _hoist_show_float_body_1(v1):
        match v1:
            case hydra.ext.haskell.syntax.LiteralChar(value=c):
                return hydra.lib.literals.show_string(hydra.lib.literals.show_uint16(c))

            case hydra.ext.haskell.syntax.LiteralDouble(value=d):
                return show_float((lambda v: hydra.lib.literals.show_float64(v)), d)

            case hydra.ext.haskell.syntax.LiteralFloat(value=f):
                return show_float((lambda v: hydra.lib.literals.show_float32(v)), f)

            case hydra.ext.haskell.syntax.LiteralInt(value=i):
                return parens_if_neg(hydra.lib.equality.lt(i, 0), hydra.lib.literals.show_int32(i))

            case hydra.ext.haskell.syntax.LiteralInteger(value=i):
                return parens_if_neg(hydra.lib.equality.lt(i, 0), hydra.lib.literals.show_bigint(i))

            case hydra.ext.haskell.syntax.LiteralString(value=s):
                return hydra.lib.literals.show_string(s)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_show_float_body_1(lit))

def application_pattern_to_expr(app_pat: hydra.ext.haskell.syntax.ApplicationPattern) -> hydra.ast.Expr:
    r"""Convert an application pattern to an AST expression."""

    name = app_pat.name
    pats = app_pat.args
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats)))

def pattern_to_expr(pat: hydra.ext.haskell.syntax.Pattern) -> hydra.ast.Expr:
    r"""Convert a pattern to an AST expression."""

    match pat:
        case hydra.ext.haskell.syntax.PatternApplication(value=app):
            return application_pattern_to_expr(app)

        case hydra.ext.haskell.syntax.PatternList(value=pats):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats))

        case hydra.ext.haskell.syntax.PatternLiteral(value=lit):
            return literal_to_expr(lit)

        case hydra.ext.haskell.syntax.PatternName(value=name):
            return name_to_expr(name)

        case hydra.ext.haskell.syntax.PatternParens(value=pat_):
            return hydra.serialization.parenthesize(pattern_to_expr(pat_))

        case hydra.ext.haskell.syntax.PatternTuple(value=pats2):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), pats2))

        case hydra.ext.haskell.syntax.PatternWildcard():
            return hydra.serialization.cst("_")

        case _:
            raise TypeError("Unsupported Pattern")

def assertion_to_expr(sert: hydra.ext.haskell.syntax.Assertion) -> hydra.ast.Expr:
    r"""Convert a type class assertion to an AST expression."""

    match sert:
        case hydra.ext.haskell.syntax.AssertionClass(value=cls):
            return class_assertion_to_expr(cls)

        case hydra.ext.haskell.syntax.AssertionTuple(value=serts):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: assertion_to_expr(x1)), serts))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def class_assertion_to_expr(cls_asrt: hydra.ext.haskell.syntax.ClassAssertion) -> hydra.ast.Expr:
    r"""Convert a class assertion to an AST expression."""

    name = cls_asrt.name
    types = cls_asrt.types
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), (hydra.serialization.comma_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types)),)))

def type_to_expr(htype: hydra.ext.haskell.syntax.Type) -> hydra.ast.Expr:
    r"""Convert a Haskell type to an AST expression."""

    match htype:
        case hydra.ext.haskell.syntax.TypeApplication(value=app_type):
            lhs = app_type.context
            rhs = app_type.argument
            return hydra.serialization.ifx(hydra.ext.haskell.operators.app_op, type_to_expr(lhs), type_to_expr(rhs))

        case hydra.ext.haskell.syntax.TypeCtx(value=ctx_type):
            ctx = ctx_type.ctx
            typ = ctx_type.type
            return hydra.serialization.ifx(hydra.ext.haskell.operators.assert_op(), assertion_to_expr(ctx), type_to_expr(typ))

        case hydra.ext.haskell.syntax.TypeFunction(value=fun_type):
            dom = fun_type.domain
            cod = fun_type.codomain
            return hydra.serialization.ifx(hydra.ext.haskell.operators.arrow_op(), type_to_expr(dom), type_to_expr(cod))

        case hydra.ext.haskell.syntax.TypeList(value=htype_):
            return hydra.serialization.bracket_list(hydra.serialization.inline_style, (type_to_expr(htype_),))

        case hydra.ext.haskell.syntax.TypeTuple(value=types):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types))

        case hydra.ext.haskell.syntax.TypeVariable(value=name):
            return name_to_expr(name)

        case _:
            raise TypeError("Unsupported Type")

def type_signature_to_expr(type_sig: hydra.ext.haskell.syntax.TypeSignature) -> hydra.ast.Expr:
    r"""Convert a type signature to an AST expression."""

    name = type_sig.name
    typ = type_sig.type
    @lru_cache(1)
    def name_expr() -> hydra.ast.Expr:
        return name_to_expr(name)
    @lru_cache(1)
    def type_expr() -> hydra.ast.Expr:
        return type_to_expr(typ)
    @lru_cache(1)
    def inline_sig() -> hydra.ast.Expr:
        return hydra.serialization.structural_space_sep((name_expr(), hydra.serialization.cst("::"), type_expr()))
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.serialization.expression_length(inline_sig()), 120), (lambda : hydra.serialization.newline_sep((hydra.serialization.space_sep((name_expr(), hydra.serialization.cst("::"))), hydra.serialization.tab_indent(type_expr())))), (lambda : inline_sig()))

def alternative_to_expr(alt: hydra.ext.haskell.syntax.Alternative) -> hydra.ast.Expr:
    r"""Convert a pattern-matching alternative to an AST expression."""

    return hydra.serialization.structural_space_sep((pattern_to_expr(alt.pattern), hydra.serialization.cst("->"), case_rhs_to_expr(alt.rhs)))

def application_expression_to_expr(app: hydra.ext.haskell.syntax.ApplicationExpression) -> hydra.ast.Expr:
    r"""Convert a function application expression to an AST expression."""

    return hydra.serialization.ifx(hydra.ext.haskell.operators.app_op, expression_to_expr(app.function), expression_to_expr(app.argument))

def case_expression_to_expr(case_expr: hydra.ext.haskell.syntax.CaseExpression) -> hydra.ast.Expr:
    r"""Convert a case expression to an AST expression."""

    cs = case_expr.case
    alts = case_expr.alternatives
    of_op = hydra.ast.Op(hydra.ast.Symbol("of"), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsSpace()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent("  "))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    @lru_cache(1)
    def lhs() -> hydra.ast.Expr:
        return hydra.serialization.space_sep((hydra.serialization.cst("case"), expression_to_expr(cs)))
    @lru_cache(1)
    def rhs() -> hydra.ast.Expr:
        return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: alternative_to_expr(x1)), alts))
    return hydra.serialization.ifx(of_op, lhs(), rhs())

def case_rhs_to_expr(rhs: hydra.ext.haskell.syntax.CaseRhs) -> hydra.ast.Expr:
    r"""Convert a case right-hand side to an AST expression."""

    return expression_to_expr(rhs.value)

def construct_record_expression_to_expr(construct_record: hydra.ext.haskell.syntax.ConstructRecordExpression) -> hydra.ast.Expr:
    r"""Convert a record construction expression to an AST expression."""

    name = construct_record.name
    updates = construct_record.fields
    def from_update(update: hydra.ext.haskell.syntax.FieldUpdate) -> hydra.ast.Expr:
        fn = update.name
        val = update.value
        return hydra.serialization.ifx(hydra.ext.haskell.operators.define_op(), name_to_expr(fn), expression_to_expr(val))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.comma_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: from_update(x1)), updates))
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), (hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.half_block_style, body()),)))

def expression_to_expr(expr: hydra.ext.haskell.syntax.Expression) -> hydra.ast.Expr:
    r"""Convert a Haskell expression to an AST expression."""

    match expr:
        case hydra.ext.haskell.syntax.ExpressionApplication(value=app):
            return application_expression_to_expr(app)

        case hydra.ext.haskell.syntax.ExpressionCase(value=cases):
            return case_expression_to_expr(cases)

        case hydra.ext.haskell.syntax.ExpressionConstructRecord(value=r):
            return construct_record_expression_to_expr(r)

        case hydra.ext.haskell.syntax.ExpressionDo(value=statements):
            return hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst("do"), hydra.lib.lists.map((lambda x1: statement_to_expr(x1)), statements)))

        case hydra.ext.haskell.syntax.ExpressionIf(value=ifte):
            return if_expression_to_expr(ifte)

        case hydra.ext.haskell.syntax.ExpressionLiteral(value=lit):
            return literal_to_expr(lit)

        case hydra.ext.haskell.syntax.ExpressionLambda(value=lam):
            return hydra.serialization.parenthesize(lambda_expression_to_expr(lam))

        case hydra.ext.haskell.syntax.ExpressionLet(value=let_expr):
            bindings = let_expr.bindings
            inner = let_expr.inner
            def encode_binding(binding: hydra.ext.haskell.syntax.LocalBinding) -> hydra.ast.Expr:
                return hydra.serialization.indent_subsequent_lines("    ", local_binding_to_expr(binding))
            return hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst(""), hydra.lib.lists.cons(hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("let"), (hydra.serialization.custom_indent_block("    ", hydra.lib.lists.map((lambda x1: encode_binding(x1)), bindings)),))), (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("in"), (expression_to_expr(inner),))),))))

        case hydra.ext.haskell.syntax.ExpressionList(value=exprs):
            return hydra.serialization.bracket_list(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), exprs))

        case hydra.ext.haskell.syntax.ExpressionParens(value=expr_):
            return hydra.serialization.parenthesize(expression_to_expr(expr_))

        case hydra.ext.haskell.syntax.ExpressionTuple(value=exprs2):
            return hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: expression_to_expr(x1)), exprs2))

        case hydra.ext.haskell.syntax.ExpressionVariable(value=name):
            return name_to_expr(name)

        case _:
            raise TypeError("Unsupported Expression")

def if_expression_to_expr(if_expr: hydra.ext.haskell.syntax.IfExpression) -> hydra.ast.Expr:
    r"""Convert an if-then-else expression to an AST expression."""

    eif = if_expr.condition
    ethen = if_expr.then
    eelse = if_expr.else_
    if_op = hydra.ast.Op(hydra.ast.Symbol(""), hydra.ast.Padding(cast(hydra.ast.Ws, hydra.ast.WsNone()), cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent("  "))), hydra.ast.Precedence(0), hydra.ast.Associativity.NONE)
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("then"), (expression_to_expr(ethen),))), (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("else"), (expression_to_expr(eelse),))),)))
    return hydra.serialization.ifx(if_op, hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("if"), (expression_to_expr(eif),))), body())

def lambda_expression_to_expr(lambda_expr: hydra.ext.haskell.syntax.LambdaExpression) -> hydra.ast.Expr:
    r"""Convert a lambda expression to an AST expression."""

    bindings = lambda_expr.bindings
    inner = lambda_expr.inner
    @lru_cache(1)
    def head() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: pattern_to_expr(x1)), bindings))
    @lru_cache(1)
    def body() -> hydra.ast.Expr:
        return expression_to_expr(inner)
    return hydra.serialization.ifx(hydra.ext.haskell.operators.lambda_op(), hydra.serialization.prefix("\\", head()), body())

def local_binding_to_expr(binding: hydra.ext.haskell.syntax.LocalBinding) -> hydra.ast.Expr:
    r"""Convert a local binding to an AST expression."""

    match binding:
        case hydra.ext.haskell.syntax.LocalBindingSignature(value=ts):
            return type_signature_to_expr(ts)

        case hydra.ext.haskell.syntax.LocalBindingValue(value=vb):
            return value_binding_to_expr(vb)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def right_hand_side_to_expr(rhs: hydra.ext.haskell.syntax.RightHandSide) -> hydra.ast.Expr:
    r"""Convert a right-hand side to an AST expression."""

    return expression_to_expr(rhs.value)

def statement_to_expr(stmt: hydra.ext.haskell.syntax.Statement) -> hydra.ast.Expr:
    r"""Convert a statement to an AST expression."""

    return expression_to_expr(stmt.value)

def value_binding_to_expr(vb: hydra.ext.haskell.syntax.ValueBinding) -> hydra.ast.Expr:
    r"""Convert a value binding to an AST expression."""

    match vb:
        case hydra.ext.haskell.syntax.ValueBindingSimple(value=simple_v_b):
            pat = simple_v_b.pattern
            rhs = simple_v_b.rhs
            local = simple_v_b.local_bindings
            @lru_cache(1)
            def lhs_expr() -> hydra.ast.Expr:
                return pattern_to_expr(pat)
            @lru_cache(1)
            def rhs_expr() -> hydra.ast.Expr:
                return right_hand_side_to_expr(rhs)
            @lru_cache(1)
            def inline_body() -> hydra.ast.Expr:
                return hydra.serialization.structural_space_sep((lhs_expr(), hydra.serialization.cst("="), rhs_expr()))
            @lru_cache(1)
            def body() -> hydra.ast.Expr:
                return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.serialization.expression_length(inline_body()), 120), (lambda : hydra.serialization.newline_sep((hydra.serialization.space_sep((lhs_expr(), hydra.serialization.cst("="))), hydra.serialization.tab_indent(rhs_expr())))), (lambda : inline_body()))
            return hydra.lib.maybes.maybe((lambda : body()), (lambda local_bindings: (bindings := local_bindings.value, hydra.serialization.indent_block(hydra.lib.lists.cons(body(), (hydra.serialization.indent_block(hydra.lib.lists.cons(hydra.serialization.cst("where"), hydra.lib.lists.map((lambda x1: local_binding_to_expr(x1)), bindings))),))))[1]), local)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def field_to_expr(field: hydra.ext.haskell.syntax.Field) -> hydra.ast.Expr:
    r"""Convert a field declaration to an AST expression."""

    name = field.name
    typ = field.type
    return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), hydra.lib.lists.cons(hydra.serialization.cst("::"), (type_to_expr(typ),))))

def to_haskell_comments(c: str) -> str:
    r"""Convert a string to Haddock documentation comments."""

    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("-- | ", s)), hydra.lib.strings.lines(c)))

def field_with_comments_to_expr(field_with_comments: hydra.ext.haskell.syntax.FieldWithComments) -> hydra.ast.Expr:
    r"""Convert a field with comments to an AST expression."""

    field = field_with_comments.field
    mc = field_with_comments.comments
    return hydra.lib.maybes.maybe((lambda : field_to_expr(field)), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (field_to_expr(field),)))), mc)

def constructor_to_expr(cons: hydra.ext.haskell.syntax.Constructor) -> hydra.ast.Expr:
    r"""Convert a data constructor to an AST expression."""

    match cons:
        case hydra.ext.haskell.syntax.ConstructorOrdinary(value=ord):
            name = ord.name
            types = ord.fields
            return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), (hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: type_to_expr(x1)), types)),)))

        case hydra.ext.haskell.syntax.ConstructorRecord(value=rec):
            name = rec.name
            fields = rec.fields
            return hydra.serialization.space_sep(hydra.lib.lists.cons(name_to_expr(name), (hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: field_with_comments_to_expr(x1)), fields)),)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def constructor_with_comments_to_expr(cons_with_comments: hydra.ext.haskell.syntax.ConstructorWithComments) -> hydra.ast.Expr:
    r"""Convert a data constructor with comments to an AST expression."""

    body = cons_with_comments.body
    mc = cons_with_comments.comments
    return hydra.lib.maybes.maybe((lambda : constructor_to_expr(body)), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (constructor_to_expr(body),)))), mc)

def data_or_newtype_to_expr(kw: hydra.ext.haskell.syntax.DataOrNewtype) -> hydra.ast.Expr:
    r"""Convert a data/newtype keyword to an AST expression."""

    match kw:
        case hydra.ext.haskell.syntax.DataOrNewtype.DATA:
            return hydra.serialization.cst("data")

        case hydra.ext.haskell.syntax.DataOrNewtype.NEWTYPE:
            return hydra.serialization.cst("newtype")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def variable_to_expr(variable: hydra.ext.haskell.syntax.Variable) -> hydra.ast.Expr:
    r"""Convert a type variable to an AST expression."""

    return name_to_expr(variable.value)

def declaration_head_to_expr(hd: hydra.ext.haskell.syntax.DeclarationHead) -> hydra.ast.Expr:
    r"""Convert a declaration head to an AST expression."""

    match hd:
        case hydra.ext.haskell.syntax.DeclarationHeadApplication(value=app_head):
            fun = app_head.function
            op = app_head.operand
            return hydra.serialization.space_sep(hydra.lib.lists.cons(declaration_head_to_expr(fun), (variable_to_expr(op),)))

        case hydra.ext.haskell.syntax.DeclarationHeadSimple(value=name):
            return name_to_expr(name)

        case _:
            raise TypeError("Unsupported DeclarationHead")

def declaration_to_expr(decl: hydra.ext.haskell.syntax.Declaration) -> hydra.ast.Expr:
    r"""Convert a declaration to an AST expression."""

    match decl:
        case hydra.ext.haskell.syntax.DeclarationData(value=data_decl):
            kw = data_decl.keyword
            hd = data_decl.head
            cons = data_decl.constructors
            deriv = data_decl.deriving
            @lru_cache(1)
            def deriv_cat() -> frozenlist[hydra.ext.haskell.syntax.Name]:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda v1: v1.value), deriv))
            @lru_cache(1)
            def constructors() -> hydra.ast.Expr:
                return hydra.serialization.or_sep(hydra.serialization.half_block_style, hydra.lib.lists.map((lambda x1: constructor_with_comments_to_expr(x1)), cons))
            @lru_cache(1)
            def deriving_clause() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(deriv_cat()), (lambda : ()), (lambda : (hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("deriving"), (hydra.serialization.paren_list(False, hydra.lib.lists.map((lambda x1: name_to_expr(x1)), deriv_cat())),))),)))
            @lru_cache(1)
            def main_parts() -> frozenlist[hydra.ast.Expr]:
                return (hydra.serialization.space_sep(hydra.lib.lists.cons(data_or_newtype_to_expr(kw), hydra.lib.lists.cons(declaration_head_to_expr(hd), (hydra.serialization.cst("="),)))), constructors())
            return hydra.serialization.indent_block(hydra.lib.lists.concat2(main_parts(), deriving_clause()))

        case hydra.ext.haskell.syntax.DeclarationType(value=type_decl):
            hd = type_decl.name
            typ = type_decl.type
            return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("type"), hydra.lib.lists.cons(declaration_head_to_expr(hd), hydra.lib.lists.cons(hydra.serialization.cst("="), (type_to_expr(typ),)))))

        case hydra.ext.haskell.syntax.DeclarationValueBinding(value=vb):
            return value_binding_to_expr(vb)

        case hydra.ext.haskell.syntax.DeclarationTypedBinding(value=typed_binding):
            type_sig = typed_binding.type_signature
            vb = typed_binding.value_binding
            name = type_sig.name
            htype = type_sig.type
            return hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.structural_space_sep((name_to_expr(name), hydra.serialization.cst("::"), type_to_expr(htype))), (value_binding_to_expr(vb),)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def declaration_with_comments_to_expr(decl_with_comments: hydra.ext.haskell.syntax.DeclarationWithComments) -> hydra.ast.Expr:
    r"""Convert a declaration with comments to an AST expression."""

    body = decl_with_comments.body
    mc = decl_with_comments.comments
    return hydra.lib.maybes.maybe((lambda : declaration_to_expr(body)), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), (declaration_to_expr(body),)))), mc)

def import_export_spec_to_expr(spec: hydra.ext.haskell.syntax.ImportExportSpec) -> hydra.ast.Expr:
    r"""Convert an import/export specification to an AST expression."""

    return name_to_expr(spec.name)

def import_to_expr(import_: hydra.ext.haskell.syntax.Import) -> hydra.ast.Expr:
    r"""Convert an import statement to an AST expression."""

    qual = import_.qualified
    mod_name = import_.module
    mod = import_.as_
    mspec = import_.spec
    name = mod_name.value
    def hiding_sec(spec: hydra.ext.haskell.syntax.SpecImport) -> hydra.ast.Expr:
        match spec:
            case hydra.ext.haskell.syntax.SpecImportHiding(value=names):
                return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("hiding "), (hydra.serialization.parens(hydra.serialization.comma_sep(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: import_export_spec_to_expr(x1)), names))),)))

            case _:
                raise TypeError("Unsupported SpecImport")
    @lru_cache(1)
    def parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.cat((Just(hydra.serialization.cst("import")), hydra.lib.logic.if_else(qual, (lambda : Just(hydra.serialization.cst("qualified"))), (lambda : Nothing())), Just(hydra.serialization.cst(name)), hydra.lib.maybes.map((lambda m: hydra.serialization.cst(hydra.lib.strings.cat2("as ", m.value))), mod), hydra.lib.maybes.map((lambda x1: hiding_sec(x1)), mspec)))
    return hydra.serialization.space_sep(parts())

def module_head_to_expr(module_head: hydra.ext.haskell.syntax.ModuleHead) -> hydra.ast.Expr:
    r"""Convert a module head to an AST expression."""

    mc = module_head.comments
    mod_name = module_head.name
    mname = mod_name.value
    @lru_cache(1)
    def head() -> hydra.ast.Expr:
        return hydra.serialization.space_sep(hydra.lib.lists.cons(hydra.serialization.cst("module"), hydra.lib.lists.cons(hydra.serialization.cst(mname), (hydra.serialization.cst("where"),))))
    return hydra.lib.maybes.maybe((lambda : head()), (lambda c: hydra.serialization.newline_sep(hydra.lib.lists.cons(hydra.serialization.cst(to_haskell_comments(c)), hydra.lib.lists.cons(hydra.serialization.cst(""), (head(),))))), mc)

def to_simple_comments(c: str) -> str:
    r"""Convert a string to simple line comments."""

    return hydra.lib.strings.intercalate("\n", hydra.lib.lists.map((lambda s: hydra.lib.strings.cat2("-- ", s)), hydra.lib.strings.lines(c)))

def module_to_expr(module: hydra.ext.haskell.syntax.Module) -> hydra.ast.Expr:
    r"""Convert a Haskell module to an AST expression."""

    mh = module.head
    imports = module.imports
    decls = module.declarations
    @lru_cache(1)
    def warning() -> frozenlist[hydra.ast.Expr]:
        return (hydra.serialization.cst(to_simple_comments(hydra.constants.warning_auto_generated_file)),)
    @lru_cache(1)
    def header_line() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda h: (module_head_to_expr(h),)), mh)
    @lru_cache(1)
    def decl_lines() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: declaration_with_comments_to_expr(x1)), decls)
    @lru_cache(1)
    def import_lines() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(imports), (lambda : ()), (lambda : (hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: import_to_expr(x1)), imports)),)))
    return hydra.serialization.double_newline_sep(hydra.lib.lists.concat((warning(), header_line(), import_lines(), decl_lines())))
