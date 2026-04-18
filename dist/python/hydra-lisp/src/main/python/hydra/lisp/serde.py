# Note: this is an automatically generated file. Do not edit.

r"""Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.formatting
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.lisp.syntax
import hydra.serialization

def symbol_to_expr(s: hydra.lisp.syntax.Symbol) -> hydra.ast.Expr:
    return hydra.serialization.cst(s.value)

def lambda_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "fn"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "lambda"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:lambda"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "lambda"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def list_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "list"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "list"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:list"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "list"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def false_expr(d: hydra.lisp.syntax.Dialect) -> hydra.ast.Expr:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.cst("false")

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.cst("nil")

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.cst("cl:nil")

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.cst("#f")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def format_lisp_float(d: hydra.lisp.syntax.Dialect, v: Decimal):
    @lru_cache(1)
    def s() -> str:
        return hydra.lib.literals.show_bigfloat(v)
    def _hoist_s_body_1(v1):
        match v1:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return "Double/NaN"

            case hydra.lisp.syntax.Dialect.SCHEME:
                return "+nan.0"

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return "+hydra-nan+"

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return "0.0e+NaN"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_s_body_2(v1):
        match v1:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return "Double/POSITIVE_INFINITY"

            case hydra.lisp.syntax.Dialect.SCHEME:
                return "+inf.0"

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return "+hydra-pos-inf+"

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return "1.0e+INF"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_s_body_3(v1):
        match v1:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return "Double/NEGATIVE_INFINITY"

            case hydra.lisp.syntax.Dialect.SCHEME:
                return "-inf.0"

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return "+hydra-neg-inf+"

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return "-1.0e+INF"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "NaN"), (lambda : _hoist_s_body_1(d)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "Infinity"), (lambda : _hoist_s_body_2(d)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s(), "-Infinity"), (lambda : _hoist_s_body_3(d)), (lambda : s()))))))

def keyword_to_expr(d: hydra.lisp.syntax.Dialect, k: hydra.lisp.syntax.Keyword) -> hydra.ast.Expr:
    name = k.name
    ns = k.namespace
    match d:
        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst(name)))

        case _:
            return hydra.serialization.cst(hydra.lib.maybes.maybe((lambda : hydra.lib.strings.cat2(":", name)), (lambda n: hydra.lib.strings.cat((n, "/:", name))), ns))

def nil_expr(d: hydra.lisp.syntax.Dialect) -> hydra.ast.Expr:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.cst("nil")

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.cst("nil")

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.cst("cl:nil")

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.cst("'()")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def true_expr(d: hydra.lisp.syntax.Dialect) -> hydra.ast.Expr:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.cst("true")

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.cst("t")

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.cst("cl:t")

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.cst("#t")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_to_expr(d: hydra.lisp.syntax.Dialect, lit: hydra.lisp.syntax.Literal):
    match lit:
        case hydra.lisp.syntax.LiteralInteger(value=i):
            return hydra.serialization.cst(hydra.lib.literals.show_bigint(i.value))

        case hydra.lisp.syntax.LiteralFloat(value=f):
            return hydra.serialization.cst(format_lisp_float(d, f.value))

        case hydra.lisp.syntax.LiteralString(value=s):
            @lru_cache(1)
            def e1() -> str:
                return hydra.lib.strings.intercalate("\\\\", hydra.lib.strings.split_on("\\", s))
            def _hoist_e1_body_1(v1):
                match v1:
                    case hydra.lisp.syntax.Dialect.COMMON_LISP:
                        @lru_cache(1)
                        def escaped() -> str:
                            return hydra.lib.strings.intercalate("\\\"", hydra.lib.strings.split_on("\"", e1()))
                        return hydra.serialization.cst(hydra.lib.strings.cat(("\"", escaped(), "\"")))

                    case hydra.lisp.syntax.Dialect.CLOJURE:
                        @lru_cache(1)
                        def e2() -> str:
                            return hydra.lib.strings.intercalate("\\n", hydra.lib.strings.split_on(hydra.lib.strings.from_list((10,)), e1()))
                        @lru_cache(1)
                        def e3() -> str:
                            return hydra.lib.strings.intercalate("\\r", hydra.lib.strings.split_on(hydra.lib.strings.from_list((13,)), e2()))
                        @lru_cache(1)
                        def e4() -> str:
                            return hydra.lib.strings.intercalate("\\t", hydra.lib.strings.split_on(hydra.lib.strings.from_list((9,)), e3()))
                        @lru_cache(1)
                        def escaped() -> str:
                            return hydra.lib.strings.intercalate("\\\"", hydra.lib.strings.split_on("\"", e4()))
                        return hydra.serialization.cst(hydra.lib.strings.cat(("\"", escaped(), "\"")))

                    case hydra.lisp.syntax.Dialect.EMACS_LISP:
                        @lru_cache(1)
                        def e2() -> str:
                            return hydra.lib.strings.intercalate("\\n", hydra.lib.strings.split_on(hydra.lib.strings.from_list((10,)), e1()))
                        @lru_cache(1)
                        def e3() -> str:
                            return hydra.lib.strings.intercalate("\\r", hydra.lib.strings.split_on(hydra.lib.strings.from_list((13,)), e2()))
                        @lru_cache(1)
                        def e4() -> str:
                            return hydra.lib.strings.intercalate("\\t", hydra.lib.strings.split_on(hydra.lib.strings.from_list((9,)), e3()))
                        @lru_cache(1)
                        def escaped() -> str:
                            return hydra.lib.strings.intercalate("\\\"", hydra.lib.strings.split_on("\"", e4()))
                        return hydra.serialization.cst(hydra.lib.strings.cat(("\"", escaped(), "\"")))

                    case hydra.lisp.syntax.Dialect.SCHEME:
                        @lru_cache(1)
                        def e2() -> str:
                            return hydra.lib.strings.intercalate("\\n", hydra.lib.strings.split_on(hydra.lib.strings.from_list((10,)), e1()))
                        @lru_cache(1)
                        def e3() -> str:
                            return hydra.lib.strings.intercalate("\\r", hydra.lib.strings.split_on(hydra.lib.strings.from_list((13,)), e2()))
                        @lru_cache(1)
                        def e4() -> str:
                            return hydra.lib.strings.intercalate("\\t", hydra.lib.strings.split_on(hydra.lib.strings.from_list((9,)), e3()))
                        @lru_cache(1)
                        def escaped() -> str:
                            return hydra.lib.strings.intercalate("\\\"", hydra.lib.strings.split_on("\"", e4()))
                        return hydra.serialization.cst(hydra.lib.strings.cat(("\"", escaped(), "\"")))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_e1_body_1(d)

        case hydra.lisp.syntax.LiteralCharacter(value=c):
            ch = c.value
            def _hoist_ch_body_1(v1):
                match v1:
                    case hydra.lisp.syntax.Dialect.CLOJURE:
                        return hydra.serialization.cst(hydra.lib.strings.cat2("\\", ch))

                    case hydra.lisp.syntax.Dialect.EMACS_LISP:
                        return hydra.serialization.cst(hydra.lib.strings.cat2("?", ch))

                    case hydra.lisp.syntax.Dialect.COMMON_LISP:
                        return hydra.serialization.cst(hydra.lib.strings.cat2("#\\", ch))

                    case hydra.lisp.syntax.Dialect.SCHEME:
                        return hydra.serialization.cst(hydra.lib.strings.cat2("#\\", ch))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_ch_body_1(d)

        case hydra.lisp.syntax.LiteralBoolean(value=b):
            return hydra.lib.logic.if_else(b, (lambda : true_expr(d)), (lambda : false_expr(d)))

        case hydra.lisp.syntax.LiteralNil():
            return nil_expr(d)

        case hydra.lisp.syntax.LiteralKeyword(value=k):
            return keyword_to_expr(d, k)

        case hydra.lisp.syntax.LiteralSymbol(value=s2):
            return hydra.serialization.no_sep((hydra.serialization.cst("'"), symbol_to_expr(s2)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def s_expression_to_expr(sexpr: hydra.lisp.syntax.SExpression) -> hydra.ast.Expr:
    match sexpr:
        case hydra.lisp.syntax.SExpressionAtom(value=a):
            return hydra.serialization.cst(a)

        case hydra.lisp.syntax.SExpressionList(value=elems):
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda x1: s_expression_to_expr(x1)), elems)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def variable_reference_to_expr(d: hydra.lisp.syntax.Dialect, vref: hydra.lisp.syntax.VariableReference):
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(vref.name)
    is_fn_ns = vref.function_namespace
    def _hoist_name_body_1(v1):
        match v1:
            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return hydra.serialization.no_sep((hydra.serialization.cst("#'"), name()))

            case hydra.lisp.syntax.Dialect.CLOJURE:
                return name()

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return name()

            case hydra.lisp.syntax.Dialect.SCHEME:
                return name()

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(is_fn_ns, (lambda : _hoist_name_body_1(d)), (lambda : name()))

def and_expression_to_expr(d: hydra.lisp.syntax.Dialect, and_expr: hydra.lisp.syntax.AndExpression) -> hydra.ast.Expr:
    return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("and"),), hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), and_expr.expressions))))

def application_to_expr(d: hydra.lisp.syntax.Dialect, app: hydra.lisp.syntax.Application) -> hydra.ast.Expr:
    fun_expr = app.function
    @lru_cache(1)
    def fun() -> hydra.ast.Expr:
        return expression_to_expr(d, fun_expr)
    @lru_cache(1)
    def args() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), app.arguments)
    @lru_cache(1)
    def needs_funcall():
        def _hoist_needs_funcall_1(v1):
            match v1:
                case hydra.lisp.syntax.ExpressionVariable():
                    return False

                case _:
                    return True
        match d:
            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return _hoist_needs_funcall_1(fun_expr)

            case _:
                return False
    @lru_cache(1)
    def all_parts() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(needs_funcall(), (lambda : hydra.lib.lists.concat2((hydra.serialization.cst("funcall"), fun()), args())), (lambda : hydra.lib.lists.concat2((fun(),), args())))
    return hydra.serialization.parens(hydra.serialization.space_sep(all_parts()))

def case_expression_to_expr(d: hydra.lisp.syntax.Dialect, case_expr: hydra.lisp.syntax.CaseExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def scrutinee() -> hydra.ast.Expr:
        return expression_to_expr(d, case_expr.scrutinee)
    clauses = case_expr.clauses
    dflt = case_expr.default
    @lru_cache(1)
    def clause_exprs() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda c: hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), c.keys))), expression_to_expr(d, c.body))))), clauses)
    @lru_cache(1)
    def default_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("else"), expression_to_expr(d, e)))),)), dflt)
    return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("case"), scrutinee()), clause_exprs(), default_part()))))

def cond_expression_to_expr(d: hydra.lisp.syntax.Dialect, cond_expr: hydra.lisp.syntax.CondExpression) -> hydra.ast.Expr:
    clauses = cond_expr.clauses
    dflt = cond_expr.default
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            @lru_cache(1)
            def clause_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda c: (expression_to_expr(d, c.condition), expression_to_expr(d, c.body))), clauses))
            @lru_cache(1)
            def default_part():
                return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (hydra.serialization.cst(":else"), expression_to_expr(d, e))), dflt)
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cond"),), clause_exprs(), default_part()))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            @lru_cache(1)
            def clause_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda c: hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, c.condition), expression_to_expr(d, c.body))))), clauses)
            @lru_cache(1)
            def default_part():
                return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("t"), expression_to_expr(d, e)))),)), dflt)
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cond"),), clause_exprs(), default_part()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            @lru_cache(1)
            def clause_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda c: hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, c.condition), expression_to_expr(d, c.body))))), clauses)
            @lru_cache(1)
            def default_part():
                return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("t"), expression_to_expr(d, e)))),)), dflt)
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cond"),), clause_exprs(), default_part()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            @lru_cache(1)
            def clause_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda c: hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, c.condition), expression_to_expr(d, c.body))))), clauses)
            @lru_cache(1)
            def default_part():
                return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("else"), expression_to_expr(d, e)))),)), dflt)
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cond"),), clause_exprs(), default_part()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def do_expression_to_expr(d: hydra.lisp.syntax.Dialect, do_expr: hydra.lisp.syntax.DoExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def kw() -> str:
        match d:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return "do"

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return "progn"

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return "progn"

            case hydra.lisp.syntax.Dialect.SCHEME:
                return "begin"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(kw()),), hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), do_expr.expressions))))

def expression_to_expr(d: hydra.lisp.syntax.Dialect, expr: hydra.lisp.syntax.Expression):
    def _hoist_hydra_lisp_serde_expression_to_expr_1(d, u, v1):
        match v1:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return hydra.serialization.no_sep((hydra.serialization.cst("~"), expression_to_expr(d, u.body)))

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return hydra.serialization.no_sep((hydra.serialization.cst(","), expression_to_expr(d, u.body)))

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return hydra.serialization.no_sep((hydra.serialization.cst(","), expression_to_expr(d, u.body)))

            case hydra.lisp.syntax.Dialect.SCHEME:
                return hydra.serialization.no_sep((hydra.serialization.cst(","), expression_to_expr(d, u.body)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_lisp_serde_expression_to_expr_2(d, su, v1):
        match v1:
            case hydra.lisp.syntax.Dialect.CLOJURE:
                return hydra.serialization.no_sep((hydra.serialization.cst("~@"), expression_to_expr(d, su.body)))

            case hydra.lisp.syntax.Dialect.EMACS_LISP:
                return hydra.serialization.no_sep((hydra.serialization.cst(",@"), expression_to_expr(d, su.body)))

            case hydra.lisp.syntax.Dialect.COMMON_LISP:
                return hydra.serialization.no_sep((hydra.serialization.cst(",@"), expression_to_expr(d, su.body)))

            case hydra.lisp.syntax.Dialect.SCHEME:
                return hydra.serialization.no_sep((hydra.serialization.cst(",@"), expression_to_expr(d, su.body)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match expr:
        case hydra.lisp.syntax.ExpressionApplication(value=a):
            return application_to_expr(d, a)

        case hydra.lisp.syntax.ExpressionLambda(value=l):
            return lambda_to_expr(d, l)

        case hydra.lisp.syntax.ExpressionLet(value=l2):
            return let_expression_to_expr(d, l2)

        case hydra.lisp.syntax.ExpressionIf(value=i):
            return if_expression_to_expr(d, i)

        case hydra.lisp.syntax.ExpressionCond(value=c):
            return cond_expression_to_expr(d, c)

        case hydra.lisp.syntax.ExpressionCase(value=c2):
            return case_expression_to_expr(d, c2)

        case hydra.lisp.syntax.ExpressionAnd(value=a2):
            return and_expression_to_expr(d, a2)

        case hydra.lisp.syntax.ExpressionOr(value=o):
            return or_expression_to_expr(d, o)

        case hydra.lisp.syntax.ExpressionNot(value=n):
            return not_expression_to_expr(d, n)

        case hydra.lisp.syntax.ExpressionDo(value=e):
            return do_expression_to_expr(d, e)

        case hydra.lisp.syntax.ExpressionBegin(value=e2):
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("begin"),), hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), e2.expressions))))

        case hydra.lisp.syntax.ExpressionVariable(value=v):
            return variable_reference_to_expr(d, v)

        case hydra.lisp.syntax.ExpressionLiteral(value=l3):
            return literal_to_expr(d, l3)

        case hydra.lisp.syntax.ExpressionList(value=l4):
            return list_literal_to_expr(d, l4)

        case hydra.lisp.syntax.ExpressionVector(value=v2):
            return vector_literal_to_expr(d, v2)

        case hydra.lisp.syntax.ExpressionMap(value=m):
            return map_literal_to_expr(d, m)

        case hydra.lisp.syntax.ExpressionSet(value=s):
            return set_literal_to_expr(d, s)

        case hydra.lisp.syntax.ExpressionCons(value=c3):
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("cons"), expression_to_expr(d, c3.head), expression_to_expr(d, c3.tail))))

        case hydra.lisp.syntax.ExpressionDottedPair(value=p):
            return hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, p.car), hydra.serialization.cst("."), expression_to_expr(d, p.cdr))))

        case hydra.lisp.syntax.ExpressionFieldAccess(value=fa):
            return field_access_to_expr(d, fa)

        case hydra.lisp.syntax.ExpressionTypeAnnotation(value=ta):
            return expression_to_expr(d, ta.expression)

        case hydra.lisp.syntax.ExpressionQuote(value=q):
            return hydra.serialization.no_sep((hydra.serialization.cst("'"), expression_to_expr(d, q.body)))

        case hydra.lisp.syntax.ExpressionQuasiquote(value=q2):
            return hydra.serialization.no_sep((hydra.serialization.cst("`"), expression_to_expr(d, q2.body)))

        case hydra.lisp.syntax.ExpressionUnquote(value=u):
            return _hoist_hydra_lisp_serde_expression_to_expr_1(d, u, d)

        case hydra.lisp.syntax.ExpressionSplicingUnquote(value=su):
            return _hoist_hydra_lisp_serde_expression_to_expr_2(d, su, d)

        case hydra.lisp.syntax.ExpressionSExpression(value=s2):
            return s_expression_to_expr(s2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def field_access_to_expr(d: hydra.lisp.syntax.Dialect, fa: hydra.lisp.syntax.FieldAccess) -> hydra.ast.Expr:
    @lru_cache(1)
    def rtype() -> hydra.ast.Expr:
        return symbol_to_expr(fa.record_type)
    @lru_cache(1)
    def field() -> hydra.ast.Expr:
        return symbol_to_expr(fa.field)
    @lru_cache(1)
    def target() -> hydra.ast.Expr:
        return expression_to_expr(d, fa.target)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.no_sep((hydra.serialization.cst(":"), field())), target())))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.no_sep((rtype(), hydra.serialization.cst("-"), field())), target())))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.no_sep((rtype(), hydra.serialization.cst("-"), field())), target())))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.no_sep((rtype(), hydra.serialization.cst("-"), field())), target())))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def if_expression_to_expr(d: hydra.lisp.syntax.Dialect, if_expr: hydra.lisp.syntax.IfExpression) -> hydra.ast.Expr:
    @lru_cache(1)
    def cond() -> hydra.ast.Expr:
        return expression_to_expr(d, if_expr.condition)
    @lru_cache(1)
    def then() -> hydra.ast.Expr:
        return expression_to_expr(d, if_expr.then)
    else_ = if_expr.else_
    @lru_cache(1)
    def else_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda e: (expression_to_expr(d, e),)), else_)
    return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("if"), cond(), then()), else_part()))))

def lambda_to_expr(d: hydra.lisp.syntax.Dialect, lam: hydra.lisp.syntax.Lambda) -> hydra.ast.Expr:
    @lru_cache(1)
    def params() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), lam.params)
    @lru_cache(1)
    def body() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), lam.body)
    mname = lam.name
    @lru_cache(1)
    def kw() -> str:
        return lambda_keyword(d)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(params())),), body()))))), (lambda sym: hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()), symbol_to_expr(sym)), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(params())),), body()))))), mname)

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def let_expression_to_expr(d: hydra.lisp.syntax.Dialect, let_expr: hydra.lisp.syntax.LetExpression):
    kind = let_expr.kind
    bindings = let_expr.bindings
    @lru_cache(1)
    def body() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), let_expr.body)
    @lru_cache(1)
    def binding_pairs():
        def _hoist_binding_pairs_1(v1):
            match v1:
                case hydra.lisp.syntax.LetBindingSimple(value=sb):
                    return (symbol_to_expr(sb.name), expression_to_expr(d, sb.value))

                case hydra.lisp.syntax.LetBindingDestructuring():
                    return (hydra.serialization.cst("<destructuring>"), hydra.serialization.cst("<destructuring>"))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return hydra.lib.lists.map((lambda b: _hoist_binding_pairs_1(b)), bindings)
    def _hoist_kind_body_1(v1):
        match v1:
            case hydra.lisp.syntax.LetKind.RECURSIVE:
                @lru_cache(1)
                def fn_specs():
                    def _hoist_fn_specs_1(v12):
                        match v12:
                            case hydra.lisp.syntax.LetBindingSimple(value=sb):
                                @lru_cache(1)
                                def name() -> hydra.ast.Expr:
                                    return symbol_to_expr(sb.name)
                                val = sb.value
                                def _hoist_name_body_1(v13):
                                    match v13:
                                        case hydra.lisp.syntax.ExpressionLambda(value=lam):
                                            @lru_cache(1)
                                            def params() -> frozenlist[hydra.ast.Expr]:
                                                return hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), lam.params)
                                            @lru_cache(1)
                                            def lbody() -> frozenlist[hydra.ast.Expr]:
                                                return hydra.lib.lists.map((lambda v14: expression_to_expr(d, v14)), lam.body)
                                            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((name(),), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(params())),), lbody()))))

                                        case _:
                                            return hydra.serialization.parens(hydra.serialization.space_sep((name(), expression_to_expr(d, val))))
                                return _hoist_name_body_1(val)

                            case hydra.lisp.syntax.LetBindingDestructuring():
                                return hydra.serialization.cst("<destructuring>")

                            case _:
                                raise AssertionError("Unreachable: all variants handled")
                    return hydra.lib.lists.map((lambda b: _hoist_fn_specs_1(b)), bindings)
                return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("letfn"),), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(fn_specs())),), body()))))

            case hydra.lisp.syntax.LetKind.PARALLEL:
                return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("let"),), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), binding_pairs())))),), body()))))

            case hydra.lisp.syntax.LetKind.SEQUENTIAL:
                return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("let"),), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), binding_pairs())))),), body()))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return _hoist_kind_body_1(kind)

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            @lru_cache(1)
            def kw():
                def _hoist_kw_1(v1):
                    match v1:
                        case hydra.lisp.syntax.LetKind.PARALLEL:
                            return "let"

                        case hydra.lisp.syntax.LetKind.SEQUENTIAL:
                            return "let*"

                        case hydra.lisp.syntax.LetKind.RECURSIVE:
                            return "letrec"

                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return _hoist_kw_1(kind)
            @lru_cache(1)
            def binding_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda p: hydra.serialization.parens(hydra.serialization.space_sep((hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))))), binding_pairs())
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(binding_exprs())),), body()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            @lru_cache(1)
            def kw():
                def _hoist_kw_1(v1):
                    match v1:
                        case hydra.lisp.syntax.LetKind.PARALLEL:
                            return "let"

                        case hydra.lisp.syntax.LetKind.SEQUENTIAL:
                            return "let*"

                        case hydra.lisp.syntax.LetKind.RECURSIVE:
                            return "letrec"

                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return _hoist_kw_1(kind)
            @lru_cache(1)
            def binding_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda p: hydra.serialization.parens(hydra.serialization.space_sep((hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))))), binding_pairs())
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(binding_exprs())),), body()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            @lru_cache(1)
            def kw():
                def _hoist_kw_1(v1):
                    match v1:
                        case hydra.lisp.syntax.LetKind.PARALLEL:
                            return "let"

                        case hydra.lisp.syntax.LetKind.SEQUENTIAL:
                            return "let*"

                        case hydra.lisp.syntax.LetKind.RECURSIVE:
                            return "letrec"

                        case _:
                            raise AssertionError("Unreachable: all variants handled")
                return _hoist_kw_1(kind)
            @lru_cache(1)
            def binding_exprs() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda p: hydra.serialization.parens(hydra.serialization.space_sep((hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))))), binding_pairs())
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst(kw()),), (hydra.serialization.parens(hydra.serialization.space_sep(binding_exprs())),), body()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def list_literal_to_expr(d: hydra.lisp.syntax.Dialect, ll: hydra.lisp.syntax.ListLiteral) -> hydra.ast.Expr:
    @lru_cache(1)
    def elems() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), ll.elements)
    quoted = ll.quoted
    return hydra.lib.logic.if_else(quoted, (lambda : hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.parens(hydra.serialization.space_sep(elems()))))), (lambda : hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(list_keyword(d)),), elems())))))

def map_literal_to_expr(d: hydra.lisp.syntax.Dialect, ml: hydra.lisp.syntax.MapLiteral) -> hydra.ast.Expr:
    entries = ml.entries
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.inline_style, hydra.serialization.space_sep(hydra.lib.lists.concat(hydra.lib.lists.map((lambda e: (expression_to_expr(d, e.key), expression_to_expr(d, e.value))), entries))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda e: hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, e.key), hydra.serialization.cst("."), expression_to_expr(d, e.value))))), entries)))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda e: hydra.serialization.parens(hydra.serialization.space_sep((expression_to_expr(d, e.key), hydra.serialization.cst("."), expression_to_expr(d, e.value))))), entries)))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("list"),), hydra.lib.lists.map((lambda e: hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("cons"), expression_to_expr(d, e.key), expression_to_expr(d, e.value))))), entries))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def not_expression_to_expr(d: hydra.lisp.syntax.Dialect, not_expr: hydra.lisp.syntax.NotExpression) -> hydra.ast.Expr:
    return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("not"), expression_to_expr(d, not_expr.expression))))

def or_expression_to_expr(d: hydra.lisp.syntax.Dialect, or_expr: hydra.lisp.syntax.OrExpression) -> hydra.ast.Expr:
    return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("or"),), hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), or_expr.expressions))))

def set_literal_to_expr(d: hydra.lisp.syntax.Dialect, sl: hydra.lisp.syntax.SetLiteral) -> hydra.ast.Expr:
    @lru_cache(1)
    def elems() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), sl.elements)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.no_sep((hydra.serialization.cst("#"), hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.inline_style, hydra.serialization.space_sep(elems()))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("list"),), elems())))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("cl:list"),), elems())))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("list"),), elems())))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def vector_literal_to_expr(d: hydra.lisp.syntax.Dialect, vl: hydra.lisp.syntax.VectorLiteral) -> hydra.ast.Expr:
    @lru_cache(1)
    def elems() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), vl.elements)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(elems()))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(elems()))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.no_sep((hydra.serialization.cst("#"), hydra.serialization.parens(hydra.serialization.space_sep(elems()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.no_sep((hydra.serialization.cst("#"), hydra.serialization.parens(hydra.serialization.space_sep(elems()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def comment_to_expr(c: hydra.lisp.syntax.Comment) -> hydra.ast.Expr:
    text = c.text
    return hydra.serialization.cst(hydra.lib.strings.cat2("; ", text))

def defconst_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "def"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "defconst"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:defconstant"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "define"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def constant_definition_to_expr(d: hydra.lisp.syntax.Dialect, cdef: hydra.lisp.syntax.ConstantDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(cdef.name)
    @lru_cache(1)
    def value() -> hydra.ast.Expr:
        return expression_to_expr(d, cdef.value)
    return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst(defconst_keyword(d)), name(), value())))

def def_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "def"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "defvar"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:defvar"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "define"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def defn_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "defn"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "defun"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:defun"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "define"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def defrecord_keyword(d: hydra.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return "defrecord"

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return "cl-defstruct"

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return "cl:defstruct"

        case hydra.lisp.syntax.Dialect.SCHEME:
            return "define-record-type"

        case _:
            raise AssertionError("Unreachable: all variants handled")

def docstring_to_expr(ds: hydra.lisp.syntax.Docstring) -> hydra.ast.Expr:
    return hydra.serialization.cst(hydra.lib.strings.cat((";; ", ds.value)))

def export_declaration_to_expr(d: hydra.lisp.syntax.Dialect, edecl: hydra.lisp.syntax.ExportDeclaration) -> hydra.ast.Expr:
    @lru_cache(1)
    def syms() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), edecl.symbols)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.cst("")

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.newline_sep(hydra.lib.lists.map((lambda s: hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("provide"), hydra.serialization.no_sep((hydra.serialization.cst("'"), s)))))), syms()))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(":export"),), hydra.lib.lists.map((lambda s: hydra.serialization.no_sep((hydra.serialization.cst(":"), s))), syms()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("export"),), syms())))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_definition_to_expr(d: hydra.lisp.syntax.Dialect, fdef: hydra.lisp.syntax.FunctionDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(fdef.name)
    @lru_cache(1)
    def params() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), fdef.params)
    @lru_cache(1)
    def body() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), fdef.body)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defn"), name()), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defun"), name()), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defun"), name()), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("define"),), (hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((name(),), params()))),), body()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def import_declaration_to_expr(d: hydra.lisp.syntax.Dialect, idecl: hydra.lisp.syntax.ImportDeclaration) -> hydra.ast.Expr:
    mod_name = idecl.module.value
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst(":require"), hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep((hydra.serialization.cst(mod_name),))))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("require"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst(mod_name))))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst(":use"), hydra.serialization.cst(hydra.lib.strings.cat2(":", mod_name)))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("import"), hydra.serialization.parens(hydra.serialization.cst(mod_name)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def macro_definition_to_expr(d: hydra.lisp.syntax.Dialect, mdef: hydra.lisp.syntax.MacroDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(mdef.name)
    @lru_cache(1)
    def params() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), mdef.params)
    @lru_cache(1)
    def body() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: expression_to_expr(d, v1)), mdef.body)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defmacro"), name()), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defmacro"), name()), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defmacro"), name()), (hydra.serialization.parens(hydra.serialization.space_sep(params())),), body()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("define-syntax"), name()), body()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def module_declaration_to_expr(d: hydra.lisp.syntax.Dialect, mdecl: hydra.lisp.syntax.ModuleDeclaration) -> hydra.ast.Expr:
    name = mdecl.name.value
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("ns"), hydra.serialization.cst(name))))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.newline_sep((hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("require"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib")))))), hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("provide"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst(name))))))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.newline_sep((hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("defpackage"), hydra.serialization.cst(hydra.lib.strings.cat2(":", name))))), hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("in-package"), hydra.serialization.cst(hydra.lib.strings.cat2(":", name)))))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("define-library"), hydra.serialization.parens(hydra.serialization.cst(name)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def record_type_definition_to_expr(d: hydra.lisp.syntax.Dialect, rdef: hydra.lisp.syntax.RecordTypeDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(rdef.name)
    @lru_cache(1)
    def fields() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda f: symbol_to_expr(f.name)), rdef.fields)
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            name_str = rdef.name.value
            @lru_cache(1)
            def field_names() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda f: f.name.value), rdef.fields)
            @lru_cache(1)
            def defrecord_form() -> hydra.ast.Expr:
                return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("defrecord"), name(), hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(fields())))))
            @lru_cache(1)
            def make_alias() -> hydra.ast.Expr:
                return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("defn"), hydra.serialization.cst(hydra.lib.strings.cat2("make-", name_str))), (hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep(fields())),), (hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(hydra.lib.strings.cat2("->", name_str)),), hydra.lib.lists.map((lambda fn: hydra.serialization.cst(fn)), field_names())))),)))))
            return hydra.serialization.newline_sep((defrecord_form(), make_alias()))

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cl-defstruct"), name()), fields()))))

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("cl:defstruct"), name()), fields()))))

        case hydra.lisp.syntax.Dialect.SCHEME:
            name_str = rdef.name.value
            @lru_cache(1)
            def field_names() -> frozenlist[str]:
                return hydra.lib.lists.map((lambda f: f.name.value), rdef.fields)
            @lru_cache(1)
            def constructor() -> hydra.ast.Expr:
                return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(hydra.lib.strings.cat2("make-", name_str)),), hydra.lib.lists.map((lambda fn: hydra.serialization.cst(fn)), field_names()))))
            @lru_cache(1)
            def predicate() -> hydra.ast.Expr:
                return hydra.serialization.cst(hydra.lib.strings.cat2(name_str, "?"))
            @lru_cache(1)
            def accessors() -> frozenlist[hydra.ast.Expr]:
                return hydra.lib.lists.map((lambda fn: hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst(fn), hydra.serialization.cst(hydra.lib.strings.cat((name_str, "-", fn))))))), field_names())
            return hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat(((hydra.serialization.cst("define-record-type"), name(), constructor(), predicate()), accessors()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def variable_definition_to_expr(d: hydra.lisp.syntax.Dialect, vdef: hydra.lisp.syntax.VariableDefinition) -> hydra.ast.Expr:
    @lru_cache(1)
    def name() -> hydra.ast.Expr:
        return symbol_to_expr(vdef.name)
    @lru_cache(1)
    def value() -> hydra.ast.Expr:
        return expression_to_expr(d, vdef.value)
    return hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst(def_keyword(d)), name(), value())))

def top_level_form_to_expr(d: hydra.lisp.syntax.Dialect, form: hydra.lisp.syntax.TopLevelForm) -> hydra.ast.Expr:
    match form:
        case hydra.lisp.syntax.TopLevelFormFunction(value=f):
            return function_definition_to_expr(d, f)

        case hydra.lisp.syntax.TopLevelFormVariable(value=v):
            return variable_definition_to_expr(d, v)

        case hydra.lisp.syntax.TopLevelFormConstant(value=c):
            return constant_definition_to_expr(d, c)

        case hydra.lisp.syntax.TopLevelFormRecordType(value=r):
            return record_type_definition_to_expr(d, r)

        case hydra.lisp.syntax.TopLevelFormMacro(value=m):
            return macro_definition_to_expr(d, m)

        case hydra.lisp.syntax.TopLevelFormExpression(value=e):
            return expression_to_expr(d, e)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def top_level_form_with_comments_to_expr(d: hydra.lisp.syntax.Dialect, fwc: hydra.lisp.syntax.TopLevelFormWithComments) -> hydra.ast.Expr:
    mdoc = fwc.doc
    mcomment = fwc.comment
    form = fwc.form
    @lru_cache(1)
    def doc_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda ds: (docstring_to_expr(ds),)), mdoc)
    @lru_cache(1)
    def comment_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda c: (comment_to_expr(c),)), mcomment)
    @lru_cache(1)
    def form_expr() -> hydra.ast.Expr:
        return top_level_form_to_expr(d, form)
    return hydra.serialization.newline_sep(hydra.lib.lists.concat((comment_part(), doc_part(), (form_expr(),))))

def program_to_expr(prog: hydra.lisp.syntax.Program) -> hydra.ast.Expr:
    d = prog.dialect
    mod_decl = prog.module
    imports = prog.imports
    exports = prog.exports
    forms = prog.forms
    @lru_cache(1)
    def form_part() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.map((lambda v1: top_level_form_with_comments_to_expr(d, v1)), forms)
    @lru_cache(1)
    def import_names() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda idecl: idecl.module.value), imports)
    @lru_cache(1)
    def export_syms() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda edecl: hydra.lib.lists.map((lambda x1: symbol_to_expr(x1)), edecl.symbols)), exports))
    match d:
        case hydra.lisp.syntax.Dialect.CLOJURE:
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.double_newline_sep(form_part())), (lambda m: (name_str := m.name.value, require_clauses := hydra.lib.lists.map((lambda imp: hydra.serialization.brackets(hydra.serialization.square_brackets, hydra.serialization.inline_style, hydra.serialization.space_sep((hydra.serialization.cst(imp), hydra.serialization.cst(":refer"), hydra.serialization.cst(":all"))))), import_names()), ns_form := hydra.lib.logic.if_else(hydra.lib.lists.null(require_clauses), (lambda : hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("ns"), hydra.serialization.cst(name_str))))), (lambda : hydra.serialization.parens(hydra.serialization.newline_sep((hydra.serialization.space_sep((hydra.serialization.cst("ns"), hydra.serialization.cst(name_str))), hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("  (:require"),), require_clauses)), hydra.serialization.cst(")")))))), var_names := hydra.lib.lists.concat(hydra.lib.lists.map((lambda fwc: (form := fwc.form, _hoist_form_body_1 := (lambda v1: (lambda vd: (symbol_to_expr(vd.name),))(v1.value) if isinstance(v1, hydra.lisp.syntax.TopLevelFormVariable) else (lambda fd: (symbol_to_expr(fd.name),))(v1.value) if isinstance(v1, hydra.lisp.syntax.TopLevelFormFunction) else ()), _hoist_form_body_1(form))[2]), forms)), declare_form := hydra.lib.logic.if_else(hydra.lib.lists.null(var_names), (lambda : ()), (lambda : (hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("declare"),), var_names))),))), hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((ns_form,), declare_form, form_part()))))[5]), mod_decl)

        case hydra.lisp.syntax.Dialect.EMACS_LISP:
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.double_newline_sep(form_part())), (lambda m: (name_str := m.name.value, require_cl_lib := hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("require"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst("cl-lib")))))), require_imports := hydra.lib.lists.map((lambda imp: hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("require"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst(imp))))))), import_names()), provide_form := hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("provide"), hydra.serialization.no_sep((hydra.serialization.cst("'"), hydra.serialization.cst(name_str)))))), hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((require_cl_lib,), require_imports, form_part(), (provide_form,)))))[4]), mod_decl)

        case hydra.lisp.syntax.Dialect.COMMON_LISP:
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.double_newline_sep(form_part())), (lambda m: (name_str := m.name.value, colon_name := hydra.lib.strings.cat2(":", name_str), use_clause := hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(":use"), hydra.serialization.cst(":cl")), hydra.lib.lists.map((lambda imp: hydra.serialization.cst(hydra.lib.strings.cat2(":", imp))), import_names())))), export_clause := hydra.lib.logic.if_else(hydra.lib.lists.null(export_syms()), (lambda : ()), (lambda : (hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst(":export"),), hydra.lib.lists.map((lambda s: hydra.serialization.no_sep((hydra.serialization.cst(":"), s))), export_syms())))),))), defpkg_form := hydra.serialization.parens(hydra.serialization.newline_sep(hydra.lib.lists.concat(((hydra.serialization.space_sep((hydra.serialization.cst("defpackage"), hydra.serialization.cst(colon_name))),), (use_clause,), export_clause)))), inpkg_form := hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("in-package"), hydra.serialization.cst(colon_name)))), hydra.serialization.double_newline_sep(hydra.lib.lists.concat(((defpkg_form, inpkg_form), form_part()))))[6]), mod_decl)

        case hydra.lisp.syntax.Dialect.SCHEME:
            return hydra.lib.maybes.maybe((lambda : hydra.serialization.double_newline_sep(form_part())), (lambda m: (name_str := m.name.value, name_parts := hydra.lib.lists.map((lambda p: hydra.formatting.convert_case_camel_to_lower_snake(p)), hydra.lib.strings.split_on(".", name_str)), name_expr := hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda p: hydra.serialization.cst(p)), name_parts))), domain_import_exprs := hydra.lib.lists.map((lambda idecl: (ns_name := idecl.module.value, ns_parts := hydra.lib.lists.map((lambda p: hydra.formatting.convert_case_camel_to_lower_snake(p)), hydra.lib.strings.split_on(".", ns_name)), hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.map((lambda p: hydra.serialization.cst(p)), ns_parts))))[2]), imports), scheme_base_expr := hydra.serialization.parens(hydra.serialization.space_sep((hydra.serialization.cst("scheme"), hydra.serialization.cst("base")))), all_import_exprs := hydra.lib.lists.concat2((scheme_base_expr,), domain_import_exprs), import_clause := hydra.serialization.parens(hydra.serialization.space_sep(hydra.lib.lists.concat2((hydra.serialization.cst("import"),), all_import_exprs))), export_clauses := hydra.lib.lists.map((lambda edecl: export_declaration_to_expr(d, edecl)), exports), begin_clause := hydra.serialization.parens(hydra.serialization.newline_sep(hydra.lib.lists.concat2((hydra.serialization.cst("begin"),), form_part()))), hydra.serialization.parens(hydra.serialization.newline_sep(hydra.lib.lists.concat(((hydra.serialization.space_sep((hydra.serialization.cst("define-library"), name_expr)),), export_clauses, (import_clause,), (begin_clause,))))))[9]), mod_decl)

        case _:
            raise AssertionError("Unreachable: all variants handled")
