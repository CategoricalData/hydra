# Note: this is an automatically generated file. Do not edit.

r"""Lisp code generator: converts Hydra type and term modules to Lisp AST."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.core
import hydra.environment
import hydra.ext.lisp.language
import hydra.ext.lisp.syntax
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.show.core
import hydra.sorting
import hydra.strip
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def dialect_cadr(d: hydra.ext.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.ext.lisp.syntax.Dialect.CLOJURE:
            return "second"

        case _:
            return "cadr"

def dialect_car(d: hydra.ext.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.ext.lisp.syntax.Dialect.CLOJURE:
            return "first"

        case _:
            return "car"

def dialect_constructor_prefix(d: hydra.ext.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.ext.lisp.syntax.Dialect.CLOJURE:
            return "->"

        case _:
            return "make-"

def dialect_equal(d: hydra.ext.lisp.syntax.Dialect) -> str:
    match d:
        case hydra.ext.lisp.syntax.Dialect.CLOJURE:
            return "="

        case hydra.ext.lisp.syntax.Dialect.COMMON_LISP:
            return "equal"

        case hydra.ext.lisp.syntax.Dialect.EMACS_LISP:
            return "equal"

        case _:
            return "equal?"

def lisp_app(fun: hydra.ext.lisp.syntax.Expression, args: frozenlist[hydra.ext.lisp.syntax.Expression]) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionApplication(hydra.ext.lisp.syntax.Application(fun, args)))

def lisp_keyword(name: str) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralKeyword(hydra.ext.lisp.syntax.Keyword(name, Nothing())))))

def lisp_lambda_expr(params: frozenlist[str], body: hydra.ext.lisp.syntax.Expression) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLambda(hydra.ext.lisp.syntax.Lambda(Nothing(), hydra.lib.lists.map((lambda p: hydra.ext.lisp.syntax.Symbol(p)), params), Nothing(), (body,))))

def lisp_var(name: str) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionVariable(hydra.ext.lisp.syntax.VariableReference(hydra.ext.lisp.syntax.Symbol(name), False)))

def qualified_snake_name(name: hydra.core.Name) -> str:
    raw = name.value
    parts = hydra.lib.strings.split_on(".", raw)
    @lru_cache(1)
    def snake_parts() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda p: hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(p)), parts)
    @lru_cache(1)
    def joined() -> str:
        return hydra.lib.strings.intercalate("_", snake_parts())
    return hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), joined())

def lisp_named_lambda_expr(name: str, params: frozenlist[str], body: hydra.ext.lisp.syntax.Expression) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLambda(hydra.ext.lisp.syntax.Lambda(Just(hydra.ext.lisp.syntax.Symbol(name)), hydra.lib.lists.map((lambda p: hydra.ext.lisp.syntax.Symbol(p)), params), Nothing(), (body,))))

def encode_literal(lit: hydra.core.Literal):
    def _hoist_hydra_ext_lisp_coder_encode_literal_1(v1):
        match v1:
            case hydra.core.FloatValueFloat32(value=f):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralFloat(hydra.ext.lisp.syntax.FloatLiteral(hydra.lib.literals.float32_to_bigfloat(f), Nothing())))))

            case hydra.core.FloatValueFloat64(value=f):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralFloat(hydra.ext.lisp.syntax.FloatLiteral(hydra.lib.literals.float64_to_bigfloat(f), Nothing())))))

            case hydra.core.FloatValueBigfloat(value=f):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralFloat(hydra.ext.lisp.syntax.FloatLiteral(f, Nothing())))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_ext_lisp_coder_encode_literal_2(v1):
        match v1:
            case hydra.core.IntegerValueInt8(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int8_to_bigint(i), False)))))

            case hydra.core.IntegerValueInt16(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int16_to_bigint(i), False)))))

            case hydra.core.IntegerValueInt32(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(i), False)))))

            case hydra.core.IntegerValueInt64(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int64_to_bigint(i), False)))))

            case hydra.core.IntegerValueUint8(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint8_to_bigint(i), False)))))

            case hydra.core.IntegerValueUint16(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint16_to_bigint(i), False)))))

            case hydra.core.IntegerValueUint32(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint32_to_bigint(i), False)))))

            case hydra.core.IntegerValueUint64(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.uint64_to_bigint(i), False)))))

            case hydra.core.IntegerValueBigint(value=i):
                return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(i, True)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lit:
        case hydra.core.LiteralBoolean(value=b):
            return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralBoolean(b))))

        case hydra.core.LiteralString(value=s):
            return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralString(s))))

        case hydra.core.LiteralFloat(value=fv):
            return _hoist_hydra_ext_lisp_coder_encode_literal_1(fv)

        case hydra.core.LiteralInteger(value=iv):
            return _hoist_hydra_ext_lisp_coder_encode_literal_2(iv)

        case hydra.core.LiteralBinary(value=b2):
            @lru_cache(1)
            def byte_values() -> frozenlist[int]:
                return hydra.lib.literals.binary_to_bytes(b2)
            return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionVector(hydra.ext.lisp.syntax.VectorLiteral(hydra.lib.lists.map((lambda bv: cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralInteger(hydra.ext.lisp.syntax.IntegerLiteral(hydra.lib.literals.int32_to_bigint(bv), False)))))), byte_values()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def lisp_list_expr(elements: frozenlist[hydra.ext.lisp.syntax.Expression]) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionList(hydra.ext.lisp.syntax.ListLiteral(elements, False)))

lisp_nil_expr = cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralNil())))

def is_primitive_ref(prim_name: str, term: hydra.core.Term):
    while True:
        def _hoist_hydra_ext_lisp_coder_is_primitive_ref_1(prim_name, v1):
            match v1:
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.lib.equality.equal(name.value, prim_name)

                case _:
                    return False
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_ext_lisp_coder_is_primitive_ref_1(prim_name, f)

            case hydra.core.TermVariable(value=name):
                return hydra.lib.equality.equal(name.value, prim_name)

            case hydra.core.TermAnnotated(value=at):
                prim_name = prim_name
                term = at.body
                continue

            case hydra.core.TermTypeApplication(value=ta):
                prim_name = prim_name
                term = ta.body
                continue

            case hydra.core.TermTypeLambda(value=tl):
                prim_name = prim_name
                term = tl.body
                continue

            case _:
                return False

def wrap_in_thunk(expr: hydra.ext.lisp.syntax.Expression) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLambda(hydra.ext.lisp.syntax.Lambda(Nothing(), (), Nothing(), (expr,))))

def encode_application(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, raw_fun: hydra.core.Term, raw_arg: hydra.core.Term):
    @lru_cache(1)
    def d_fun() -> hydra.core.Term:
        return hydra.strip.deannotate_term(raw_fun)
    @lru_cache(1)
    def normal() -> Either[hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression]:
        return hydra.lib.eithers.bind(encode_term(dialect, cx, g, raw_fun), (lambda fun: hydra.lib.eithers.bind(encode_term(dialect, cx, g, raw_arg), (lambda arg: Right(lisp_app(fun, (arg,)))))))
    def enc(t: hydra.core.Term) -> Either[T2, hydra.ext.lisp.syntax.Expression]:
        return encode_term(dialect, cx, g, t)
    match d_fun():
        case hydra.core.TermApplication(value=app2):
            mid_fun = app2.function
            mid_arg = app2.argument
            @lru_cache(1)
            def d_mid_fun() -> hydra.core.Term:
                return hydra.strip.deannotate_term(mid_fun)
            @lru_cache(1)
            def is_lazy2() -> bool:
                return hydra.lib.logic.or_(is_primitive_ref("hydra.lib.eithers.fromLeft", d_mid_fun()), hydra.lib.logic.or_(is_primitive_ref("hydra.lib.eithers.fromRight", d_mid_fun()), is_primitive_ref("hydra.lib.maybes.fromMaybe", d_mid_fun())))
            def _hoist_is_lazy2_body_1(v1):
                match v1:
                    case hydra.core.TermApplication(value=app3):
                        inner_fun = app3.function
                        inner_arg = app3.argument
                        @lru_cache(1)
                        def d_inner_fun() -> hydra.core.Term:
                            return hydra.strip.deannotate_term(inner_fun)
                        return hydra.lib.logic.if_else(is_primitive_ref("hydra.lib.logic.ifElse", d_inner_fun()), (lambda : hydra.lib.eithers.bind(enc(inner_arg), (lambda e_c: hydra.lib.eithers.bind(enc(mid_arg), (lambda e_t: hydra.lib.eithers.bind(enc(raw_arg), (lambda e_e: Right(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionIf(hydra.ext.lisp.syntax.IfExpression(e_c, e_t, Just(e_e)))))))))))), (lambda : hydra.lib.logic.if_else(is_primitive_ref("hydra.lib.maybes.maybe", d_inner_fun()), (lambda : hydra.lib.eithers.bind(enc(inner_fun), (lambda e_p: hydra.lib.eithers.bind(enc(inner_arg), (lambda e_def: hydra.lib.eithers.bind(enc(mid_arg), (lambda e_f: hydra.lib.eithers.bind(enc(raw_arg), (lambda e_m: Right(lisp_app(lisp_app(lisp_app(e_p, (wrap_in_thunk(e_def),)), (e_f,)), (e_m,)))))))))))), (lambda : hydra.lib.logic.if_else(is_primitive_ref("hydra.lib.maybes.cases", d_inner_fun()), (lambda : hydra.lib.eithers.bind(enc(inner_fun), (lambda e_p: hydra.lib.eithers.bind(enc(inner_arg), (lambda e_m: hydra.lib.eithers.bind(enc(mid_arg), (lambda e_n: hydra.lib.eithers.bind(enc(raw_arg), (lambda e_j: Right(lisp_app(lisp_app(lisp_app(e_p, (e_m,)), (wrap_in_thunk(e_n),)), (e_j,)))))))))))), (lambda : normal()))))))

                    case _:
                        return normal()
            return hydra.lib.logic.if_else(is_lazy2(), (lambda : hydra.lib.eithers.bind(enc(mid_fun), (lambda e_prim: hydra.lib.eithers.bind(enc(mid_arg), (lambda e_def: hydra.lib.eithers.bind(enc(raw_arg), (lambda e_arg: Right(lisp_app(lisp_app(e_prim, (wrap_in_thunk(e_def),)), (e_arg,)))))))))), (lambda : _hoist_is_lazy2_body_1(d_mid_fun())))

        case _:
            return normal()

def encode_elimination(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, elim: hydra.core.Elimination, marg: Maybe[hydra.core.Term]) -> Either[T2, hydra.ext.lisp.syntax.Expression]:
    match elim:
        case hydra.core.EliminationRecord(value=proj):
            @lru_cache(1)
            def fname() -> str:
                return hydra.formatting.convert_case_camel_to_lower_snake(proj.field.value)
            @lru_cache(1)
            def tname() -> str:
                return qualified_snake_name(proj.type_name)
            return hydra.lib.maybes.cases(marg, (lambda : Right(lisp_lambda_expr(("v",), cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionFieldAccess(hydra.ext.lisp.syntax.FieldAccess(hydra.ext.lisp.syntax.Symbol(tname()), hydra.ext.lisp.syntax.Symbol(fname()), lisp_var("v"))))))), (lambda arg: hydra.lib.eithers.bind(encode_term(dialect, cx, g, arg), (lambda sarg: Right(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionFieldAccess(hydra.ext.lisp.syntax.FieldAccess(hydra.ext.lisp.syntax.Symbol(tname()), hydra.ext.lisp.syntax.Symbol(fname()), sarg))))))))

        case hydra.core.EliminationUnion(value=cs):
            @lru_cache(1)
            def tname() -> str:
                return hydra.names.local_name_of(cs.type_name)
            case_fields = cs.cases
            def_case = cs.default
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda cf: (cfname := hydra.formatting.convert_case_camel_to_lower_snake(cf.name.value), cfterm := cf.term, cond_expr := lisp_app(lisp_var(dialect_equal(dialect)), (lisp_app(lisp_var(dialect_car(dialect)), (lisp_var("match_target"),)), lisp_keyword(cfname))), hydra.lib.eithers.bind(encode_term(dialect, cx, g, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cfterm, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("match_value"))))))), (lambda body_expr: Right(hydra.ext.lisp.syntax.CondClause(cond_expr, body_expr)))))[3]), case_fields), (lambda clauses: hydra.lib.eithers.bind(hydra.lib.maybes.cases(def_case, (lambda : Right(Nothing())), (lambda dt: hydra.lib.eithers.bind(encode_term(dialect, cx, g, dt), (lambda def_body: Right(Just(def_body)))))), (lambda def_expr: (cond_expr := cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionCond(hydra.ext.lisp.syntax.CondExpression(clauses, def_expr))), inner_expr := lisp_app(lisp_lambda_expr(("match_value",), cond_expr), (lisp_app(lisp_var(dialect_cadr(dialect)), (lisp_var("match_target"),)),)), hydra.lib.maybes.cases(marg, (lambda : Right(lisp_lambda_expr(("match_target",), inner_expr))), (lambda arg: hydra.lib.eithers.bind(encode_term(dialect, cx, g, arg), (lambda sarg: Right(lisp_app(lisp_lambda_expr(("match_target",), inner_expr), (sarg,))))))))[2]))))

        case hydra.core.EliminationWrap():
            return hydra.lib.maybes.cases(marg, (lambda : Right(lisp_lambda_expr(("v",), lisp_var("v")))), (lambda arg: encode_term(dialect, cx, g, arg)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_function(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, fun: hydra.core.Function) -> Either[hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression]:
    match fun:
        case hydra.core.FunctionLambda(value=lam):
            @lru_cache(1)
            def param() -> str:
                return hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), lam.parameter.value))
            return hydra.lib.eithers.bind(encode_term(dialect, cx, g, lam.body), (lambda body: Right(lisp_lambda_expr((param(),), body))))

        case hydra.core.FunctionPrimitive(value=name):
            return Right(lisp_var(hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), name.value))))

        case hydra.core.FunctionElimination(value=elim):
            return encode_elimination(dialect, cx, g, elim, Nothing())

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_let_as_native(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> Either[hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression]:
    @lru_cache(1)
    def is_clojure_top() -> bool:
        match dialect:
            case hydra.ext.lisp.syntax.Dialect.CLOJURE:
                return True

            case _:
                return False
    return hydra.lib.eithers.bind(encode_term(dialect, cx, g, body), (lambda body_expr: (sorted_bindings := hydra.lib.logic.if_else(True, (lambda : (all_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), bindings)), adj_list := hydra.lib.lists.map((lambda b: (b.name, hydra.lib.sets.to_list(hydra.lib.sets.intersection(all_names, hydra.variables.free_variables_in_term(b.term))))), bindings), sort_result := hydra.sorting.topological_sort(adj_list), name_to_binding := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b)), bindings)), hydra.lib.eithers.either((lambda _: bindings), (lambda sorted: hydra.lib.lists.map((lambda name: hydra.lib.maybes.from_maybe((lambda : hydra.lib.lists.head(bindings)), hydra.lib.maps.lookup(name, name_to_binding))), sorted)), sort_result))[4]), (lambda : bindings)), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda b: (bname := hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), b.name.value)), is_self_ref := hydra.lib.sets.member(b.name, hydra.variables.free_variables_in_term(b.term)), is_lambda := (_hoist_is_lambda_1 := (lambda v1: (lambda _: True)(v1.value) if isinstance(v1, hydra.core.FunctionLambda) else False), _hoist_is_lambda_2 := (lambda v1: (lambda f: _hoist_is_lambda_1(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else False), _hoist_is_lambda_2(hydra.strip.deannotate_term(b.term)))[2], hydra.lib.eithers.bind(encode_term(dialect, cx, g, b.term), (lambda bval: (is_clojure := (_hoist_is_clojure_1 := (lambda v1: (lambda _: True)(v1) if v1 else False), _hoist_is_clojure_1(dialect))[1], wrapped_val := (_hoist_wrapped_val_1 := (lambda v1: (lambda lam: cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLambda(hydra.ext.lisp.syntax.Lambda(Just(hydra.ext.lisp.syntax.Symbol(bname)), lam.params, lam.rest_param, lam.body))))(v1.value) if isinstance(v1, hydra.ext.lisp.syntax.ExpressionLambda) else bval), hydra.lib.logic.if_else(is_clojure, (lambda : hydra.lib.logic.if_else(is_self_ref, (lambda : hydra.lib.logic.if_else(is_lambda, (lambda : _hoist_wrapped_val_1(bval)), (lambda : lisp_named_lambda_expr(bname, ("_arg",), lisp_app(bval, (lisp_var("_arg"),)))))), (lambda : bval))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(is_self_ref, hydra.lib.logic.not_(is_lambda)), (lambda : lisp_lambda_expr(("_arg",), lisp_app(bval, (lisp_var("_arg"),)))), (lambda : bval)))))[1], Right((bname, wrapped_val)))[2])))[3]), sorted_bindings), (lambda encoded_bindings: (all_binding_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), bindings)), has_cross_refs := hydra.lib.lists.foldl((lambda acc, b: hydra.lib.logic.or_(acc, hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.intersection(all_binding_names, hydra.variables.free_variables_in_term(b.term)))))), False, bindings), has_self_ref := hydra.lib.lists.foldl((lambda acc, b: hydra.lib.logic.or_(acc, hydra.lib.sets.member(b.name, hydra.variables.free_variables_in_term(b.term)))), False, bindings), is_recursive := has_self_ref, let_kind := hydra.lib.logic.if_else(is_recursive, (lambda : hydra.ext.lisp.syntax.LetKind.RECURSIVE), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.lists.tail(bindings)), (lambda : hydra.ext.lisp.syntax.LetKind.PARALLEL), (lambda : hydra.ext.lisp.syntax.LetKind.SEQUENTIAL)))), lisp_bindings := hydra.lib.lists.map((lambda eb: cast(hydra.ext.lisp.syntax.LetBinding, hydra.ext.lisp.syntax.LetBindingSimple(hydra.ext.lisp.syntax.SimpleBinding(hydra.ext.lisp.syntax.Symbol(hydra.lib.pairs.first(eb)), hydra.lib.pairs.second(eb))))), encoded_bindings), Right(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLet(hydra.ext.lisp.syntax.LetExpression(let_kind, lisp_bindings, (body_expr,))))))[6])))[1]))

def encode_term(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, term: hydra.core.Term) -> Either[T2, hydra.ext.lisp.syntax.Expression]:
    match term:
        case hydra.core.TermAnnotated(value=at):
            return encode_term(dialect, cx, g, at.body)

        case hydra.core.TermApplication(value=app):
            raw_fun = app.function
            raw_arg = app.argument
            return encode_application(dialect, cx, g, raw_fun, raw_arg)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.bind(encode_term(dialect, cx, g, l), (lambda sl: Right(lisp_app(lisp_var("list"), (lisp_keyword("left"), sl)))))), (lambda r: hydra.lib.eithers.bind(encode_term(dialect, cx, g, r), (lambda sr: Right(lisp_app(lisp_var("list"), (lisp_keyword("right"), sr)))))), e)

        case hydra.core.TermFunction(value=fun):
            return encode_function(dialect, cx, g, fun)

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            return encode_let_as_native(dialect, cx, g, bindings, body)

        case hydra.core.TermList(value=els):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term(dialect, cx, g, v1)), els), (lambda sels: Right(lisp_list_expr(sels))))

        case hydra.core.TermLiteral(value=lit):
            return Right(encode_literal(lit))

        case hydra.core.TermMap(value=m):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: hydra.lib.eithers.bind(encode_term(dialect, cx, g, hydra.lib.pairs.first(entry)), (lambda k: hydra.lib.eithers.bind(encode_term(dialect, cx, g, hydra.lib.pairs.second(entry)), (lambda v: Right(hydra.ext.lisp.syntax.MapEntry(k, v))))))), hydra.lib.maps.to_list(m)), (lambda pairs: Right(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionMap(hydra.ext.lisp.syntax.MapLiteral(pairs))))))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.cases(mt, (lambda : Right(lisp_app(lisp_var("list"), (lisp_keyword("nothing"),)))), (lambda val: hydra.lib.eithers.bind(encode_term(dialect, cx, g, val), (lambda sval: Right(lisp_app(lisp_var("list"), (lisp_keyword("just"), sval)))))))

        case hydra.core.TermPair(value=p):
            return hydra.lib.eithers.bind(encode_term(dialect, cx, g, hydra.lib.pairs.first(p)), (lambda f: hydra.lib.eithers.bind(encode_term(dialect, cx, g, hydra.lib.pairs.second(p)), (lambda s: Right(lisp_list_expr((f, s)))))))

        case hydra.core.TermRecord(value=rec):
            rname = rec.type_name
            fields = rec.fields
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_term(dialect, cx, g, f.term)), fields), (lambda sfields: (constructor_name := hydra.lib.strings.cat2(dialect_constructor_prefix(dialect), qualified_snake_name(rname)), Right(lisp_app(lisp_var(constructor_name), sfields)))[1]))

        case hydra.core.TermSet(value=s):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term(dialect, cx, g, v1)), hydra.lib.sets.to_list(s)), (lambda sels: Right(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionSet(hydra.ext.lisp.syntax.SetLiteral(sels))))))

        case hydra.core.TermUnion(value=inj):
            @lru_cache(1)
            def tname() -> str:
                return hydra.names.local_name_of(inj.type_name)
            field = inj.field
            fname = field.name.value
            fterm = field.term
            @lru_cache(1)
            def dterm() -> hydra.core.Term:
                return hydra.strip.deannotate_term(fterm)
            @lru_cache(1)
            def is_unit():
                def _hoist_is_unit_1(v1):
                    match v1:
                        case hydra.core.TermUnit():
                            return True

                        case hydra.core.TermRecord(value=rt):
                            return hydra.lib.lists.null(rt.fields)

                        case _:
                            return False
                return _hoist_is_unit_1(dterm())
            return hydra.lib.logic.if_else(is_unit(), (lambda : Right(lisp_app(lisp_var("list"), (lisp_keyword(hydra.formatting.convert_case_camel_to_lower_snake(fname)), lisp_nil_expr)))), (lambda : hydra.lib.eithers.bind(encode_term(dialect, cx, g, fterm), (lambda sval: Right(lisp_app(lisp_var("list"), (lisp_keyword(hydra.formatting.convert_case_camel_to_lower_snake(fname)), sval)))))))

        case hydra.core.TermUnit():
            return Right(lisp_nil_expr)

        case hydra.core.TermVariable(value=name):
            return Right(lisp_var(hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), name.value))))

        case hydra.core.TermTypeApplication(value=ta):
            return encode_term(dialect, cx, g, ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return encode_term(dialect, cx, g, tl.body)

        case hydra.core.TermWrap(value=wt):
            return encode_term(dialect, cx, g, wt.body)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_field_def(ft: hydra.core.FieldType) -> hydra.ext.lisp.syntax.FieldDefinition:
    fname = ft.name.value
    return hydra.ext.lisp.syntax.FieldDefinition(hydra.ext.lisp.syntax.Symbol(hydra.formatting.convert_case_camel_to_lower_snake(fname)), Nothing())

def encode_let_as_lambda_app(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> Either[hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.Expression]:
    return hydra.lib.eithers.bind(encode_term(dialect, cx, g, body), (lambda body_expr: hydra.lib.eithers.foldl((lambda acc, b: (bname := hydra.formatting.convert_case_camel_or_underscore_to_lower_snake(hydra.formatting.sanitize_with_underscores(hydra.ext.lisp.language.lisp_reserved_words(), b.name.value)), hydra.lib.eithers.bind(encode_term(dialect, cx, g, b.term), (lambda bval: Right(lisp_app(lisp_lambda_expr((bname,), acc), (bval,))))))[1]), body_expr, hydra.lib.lists.reverse(bindings))))

def lisp_top_form(form: hydra.ext.lisp.syntax.TopLevelForm) -> hydra.ext.lisp.syntax.TopLevelFormWithComments:
    return hydra.ext.lisp.syntax.TopLevelFormWithComments(Nothing(), Nothing(), form)

def encode_term_definition(dialect: hydra.ext.lisp.syntax.Dialect, cx: T0, g: T1, tdef: hydra.packaging.TermDefinition):
    name = tdef.name
    term = tdef.term
    @lru_cache(1)
    def lname() -> str:
        return qualified_snake_name(name)
    @lru_cache(1)
    def dterm() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    def _hoist_dterm_body_1(v1):
        match v1:
            case hydra.core.FunctionLambda():
                return hydra.lib.eithers.bind(encode_term(dialect, cx, g, term), (lambda sterm: Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormVariable(hydra.ext.lisp.syntax.VariableDefinition(hydra.ext.lisp.syntax.Symbol(lname()), sterm, Nothing())))))))

            case _:
                return hydra.lib.eithers.bind(encode_term(dialect, cx, g, term), (lambda sterm: Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormVariable(hydra.ext.lisp.syntax.VariableDefinition(hydra.ext.lisp.syntax.Symbol(lname()), sterm, Nothing())))))))
    match dterm():
        case hydra.core.TermFunction(value=fun):
            return _hoist_dterm_body_1(fun)

        case _:
            return hydra.lib.eithers.bind(encode_term(dialect, cx, g, term), (lambda sterm: Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormVariable(hydra.ext.lisp.syntax.VariableDefinition(hydra.ext.lisp.syntax.Symbol(lname()), sterm, Nothing())))))))

def encode_type(cx: T0, g: T1, t: hydra.core.Type):
    @lru_cache(1)
    def typ() -> hydra.core.Type:
        return hydra.strip.deannotate_type(t)
    def _hoist_typ_body_1(v1):
        match v1:
            case hydra.core.LiteralTypeBinary():
                return cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("ByteArray")))

            case hydra.core.LiteralTypeBoolean():
                return cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Boolean")))

            case hydra.core.LiteralTypeFloat():
                return cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Float")))

            case hydra.core.LiteralTypeInteger():
                return cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Integer")))

            case hydra.core.LiteralTypeString():
                return cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("String")))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match typ():
        case hydra.core.TypeAnnotated(value=at):
            return encode_type(cx, g, at.body)

        case hydra.core.TypeApplication(value=at2):
            return encode_type(cx, g, at2.function)

        case hydra.core.TypeUnit():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierUnit()))

        case hydra.core.TypeLiteral(value=lt):
            return Right(_hoist_typ_body_1(lt))

        case hydra.core.TypeList(value=inner):
            return hydra.lib.eithers.map((lambda enc: cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierList(enc))), encode_type(cx, g, inner))

        case hydra.core.TypeSet(value=inner2):
            return hydra.lib.eithers.map((lambda enc: cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierSet(enc))), encode_type(cx, g, inner2))

        case hydra.core.TypeMap():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Map"))))

        case hydra.core.TypeMaybe(value=inner3):
            return hydra.lib.eithers.map((lambda enc: cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierMaybe(enc))), encode_type(cx, g, inner3))

        case hydra.core.TypeEither():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Either"))))

        case hydra.core.TypePair():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Pair"))))

        case hydra.core.TypeFunction():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Function"))))

        case hydra.core.TypeRecord():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Record"))))

        case hydra.core.TypeUnion():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Union"))))

        case hydra.core.TypeWrap():
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Wrapper"))))

        case hydra.core.TypeVariable(value=name):
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol(name.value))))

        case hydra.core.TypeForall(value=fa):
            return encode_type(cx, g, fa.body)

        case _:
            return Right(cast(hydra.ext.lisp.syntax.TypeSpecifier, hydra.ext.lisp.syntax.TypeSpecifierNamed(hydra.ext.lisp.syntax.Symbol("Any"))))

def encode_type_body(lname: str, orig_typ: hydra.core.Type, typ: hydra.core.Type) -> Either[T0, hydra.ext.lisp.syntax.TopLevelFormWithComments]:
    while True:
        match typ:
            case hydra.core.TypeForall(value=ft):
                lname = lname
                orig_typ = orig_typ
                typ = ft.body
                continue

            case hydra.core.TypeRecord(value=rt):
                return (fields := hydra.lib.lists.map((lambda x1: encode_field_def(x1)), rt), Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormRecordType(hydra.ext.lisp.syntax.RecordTypeDefinition(hydra.ext.lisp.syntax.Symbol(lname), fields, Nothing()))))))[1]

            case hydra.core.TypeUnion(value=rt2):
                return (variant_names := hydra.lib.lists.map((lambda f: cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralKeyword(hydra.ext.lisp.syntax.Keyword(hydra.formatting.convert_case_camel_to_lower_snake(f.name.value), Nothing())))))), rt2), Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormVariable(hydra.ext.lisp.syntax.VariableDefinition(hydra.ext.lisp.syntax.Symbol(hydra.lib.strings.cat2(lname, "-variants")), lisp_list_expr(variant_names), Just(hydra.ext.lisp.syntax.Docstring(hydra.lib.strings.cat2("Variants of the ", lname)))))))))[1]

            case hydra.core.TypeWrap():
                return Right(lisp_top_form(cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormRecordType(hydra.ext.lisp.syntax.RecordTypeDefinition(hydra.ext.lisp.syntax.Symbol(lname), (hydra.ext.lisp.syntax.FieldDefinition(hydra.ext.lisp.syntax.Symbol("value"), Nothing()),), Nothing())))))

            case _:
                return Right(hydra.ext.lisp.syntax.TopLevelFormWithComments(Nothing(), Just(hydra.ext.lisp.syntax.Comment(hydra.ext.lisp.syntax.CommentStyle.LINE, hydra.lib.strings.cat2(hydra.lib.strings.cat2(lname, " = "), hydra.show.core.type(orig_typ)))), cast(hydra.ext.lisp.syntax.TopLevelForm, hydra.ext.lisp.syntax.TopLevelFormExpression(cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(cast(hydra.ext.lisp.syntax.Literal, hydra.ext.lisp.syntax.LiteralNil())))))))

def encode_type_definition(cx: T0, g: T1, tdef: hydra.packaging.TypeDefinition) -> Either[T2, hydra.ext.lisp.syntax.TopLevelFormWithComments]:
    name = tdef.name
    typ = tdef.type.type
    @lru_cache(1)
    def lname() -> str:
        return qualified_snake_name(name)
    @lru_cache(1)
    def dtyp() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    return encode_type_body(lname(), typ, dtyp())

def is_cases_primitive(name: hydra.core.Name) -> bool:
    return hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.cases"))

def is_lazy2_arg_primitive(name: hydra.core.Name) -> bool:
    return hydra.lib.logic.or_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.eithers.fromLeft")), hydra.lib.logic.or_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.eithers.fromRight")), hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.fromMaybe"))))

def is_lazy3_arg_primitive(name: hydra.core.Name) -> bool:
    return hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.maybe"))

def lisp_lit_expr(lit: hydra.ext.lisp.syntax.Literal) -> hydra.ext.lisp.syntax.Expression:
    return cast(hydra.ext.lisp.syntax.Expression, hydra.ext.lisp.syntax.ExpressionLiteral(lit))

def lisp_symbol(name: str) -> hydra.ext.lisp.syntax.Symbol:
    return hydra.ext.lisp.syntax.Symbol(name)

def lisp_top_form_with_comments(mdoc: Maybe[str], form: hydra.ext.lisp.syntax.TopLevelForm) -> hydra.ext.lisp.syntax.TopLevelFormWithComments:
    return hydra.ext.lisp.syntax.TopLevelFormWithComments(hydra.lib.maybes.map((lambda d: hydra.ext.lisp.syntax.Docstring(d)), mdoc), Nothing(), form)

def module_exports(forms: frozenlist[hydra.ext.lisp.syntax.TopLevelFormWithComments]) -> frozenlist[hydra.ext.lisp.syntax.ExportDeclaration]:
    @lru_cache(1)
    def symbols():
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda fwc: (form := fwc.form, _hoist_form_body_1 := (lambda v1: (lambda vd: (vd.name,))(v1.value) if isinstance(v1, hydra.ext.lisp.syntax.TopLevelFormVariable) else (lambda rdef: (rname := rdef.name.value, fields := rdef.fields, field_syms := hydra.lib.lists.map((lambda f: (fn := f.name.value, hydra.ext.lisp.syntax.Symbol(hydra.lib.strings.cat((rname, "-", fn))))[1]), fields), hydra.lib.lists.concat(((hydra.ext.lisp.syntax.Symbol(hydra.lib.strings.cat2("make-", rname)), hydra.ext.lisp.syntax.Symbol(hydra.lib.strings.cat2(rname, "?"))), field_syms)))[3])(v1.value) if isinstance(v1, hydra.ext.lisp.syntax.TopLevelFormRecordType) else ()), _hoist_form_body_1(form))[2]), forms))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(symbols()), (lambda : ()), (lambda : (hydra.ext.lisp.syntax.ExportDeclaration(symbols()),)))

def module_imports(focus_ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.Definition]) -> frozenlist[hydra.ext.lisp.syntax.ImportDeclaration]:
    @lru_cache(1)
    def dep_nss() -> frozenlist[hydra.packaging.Namespace]:
        return hydra.lib.sets.to_list(hydra.lib.sets.delete(focus_ns, hydra.analysis.definition_dependency_namespaces(defs)))
    return hydra.lib.lists.map((lambda ns: hydra.ext.lisp.syntax.ImportDeclaration(hydra.ext.lisp.syntax.NamespaceName(ns.value), cast(hydra.ext.lisp.syntax.ImportSpec, hydra.ext.lisp.syntax.ImportSpecAll()))), dep_nss())

def module_to_lisp(dialect: hydra.ext.lisp.syntax.Dialect, mod: hydra.packaging.Module, defs0: frozenlist[hydra.packaging.Definition], cx: T0, g: T1) -> Either[hydra.ext.lisp.syntax.Program, hydra.ext.lisp.syntax.Program]:
    @lru_cache(1)
    def defs() -> frozenlist[hydra.packaging.Definition]:
        return hydra.environment.reorder_defs(defs0)
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs())
    @lru_cache(1)
    def all_type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_defs() -> frozenlist[hydra.packaging.TermDefinition]:
        return hydra.lib.pairs.second(partitioned())
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: hydra.predicates.is_nominal_type(td.type.type)), all_type_defs())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_type_definition(cx, g, v1)), type_defs()), (lambda type_items: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_term_definition(dialect, cx, g, v1)), term_defs()), (lambda term_items: (all_items := hydra.lib.lists.concat2(type_items, term_items), ns_name := mod.namespace.value, focus_ns := mod.namespace, imports := module_imports(focus_ns, defs()), exports := module_exports(all_items), Right(hydra.ext.lisp.syntax.Program(dialect, Just(hydra.ext.lisp.syntax.ModuleDeclaration(hydra.ext.lisp.syntax.NamespaceName(ns_name), Nothing())), imports, exports, all_items)))[5]))))

def qualified_type_name(name: hydra.core.Name) -> str:
    return hydra.formatting.capitalize(hydra.names.local_name_of(name))
