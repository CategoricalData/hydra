# Note: this is an automatically generated file. Do not edit.

r"""Python code generator: converts Hydra modules to Python source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.arity
import hydra.checking
import hydra.coder_utils
import hydra.coders
import hydra.context
import hydra.core
import hydra.error
import hydra.ext.python.helpers
import hydra.ext.python.names
import hydra.ext.python.serde
import hydra.ext.python.syntax
import hydra.ext.python.utils
import hydra.formatting
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names
import hydra.reduction
import hydra.rewriting
import hydra.schemas
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def should_thunk_binding(g: hydra.graph.Graph, b: hydra.core.Binding) -> bool:
    r"""Determine if a binding should be thunked based on its complexity and triviality."""

    return hydra.lib.logic.and_(hydra.coder_utils.is_complex_binding(g, b), hydra.lib.logic.not_(hydra.coder_utils.is_trivial_term(b.term)))

def python_binding_metadata(g: hydra.graph.Graph, b: hydra.core.Binding) -> Maybe[hydra.core.Term]:
    r"""Like bindingMetadata, but only for bindings that will actually be thunked."""

    return hydra.lib.logic.if_else(should_thunk_binding(g, b), (lambda : hydra.coder_utils.binding_metadata(g, b)), (lambda : Nothing()))

def python_environment_get_graph(env: hydra.ext.python.helpers.PythonEnvironment) -> hydra.graph.Graph:
    r"""Get the Graph from a PythonEnvironment."""

    return env.graph

def python_environment_set_graph(tc: hydra.graph.Graph, env: hydra.ext.python.helpers.PythonEnvironment) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Set the Graph in a PythonEnvironment."""

    return hydra.ext.python.helpers.PythonEnvironment(env.namespaces, env.bound_type_variables, tc, env.nullary_bindings, env.version, env.skip_casts, env.inline_variables)

def analyze_python_function(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, term: hydra.core.Term) -> Either[T0, hydra.typing.FunctionStructure[hydra.ext.python.helpers.PythonEnvironment]]:
    r"""Analyze a function term with Python-specific Graph management."""

    return hydra.coder_utils.analyze_function_term_with(cx, (lambda x1, x2: python_binding_metadata(x1, x2)), (lambda x1: python_environment_get_graph(x1)), (lambda x1, x2: python_environment_set_graph(x1, x2)), env, term)

def class_variant_pattern_unit(py_variant_name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a class pattern for a unit variant (no value captured)."""

    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternClass(hydra.ext.python.syntax.ClassPattern(hydra.ext.python.syntax.NameOrAttribute((py_variant_name,)), Nothing(), Nothing())))

def class_variant_pattern_with_capture(env: hydra.ext.python.helpers.PythonEnvironment, py_variant_name: hydra.ext.python.syntax.Name, var_name: hydra.core.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a class pattern for a variant with captured value."""

    @lru_cache(1)
    def py_var_name_attr() -> hydra.ext.python.syntax.NameOrAttribute:
        return hydra.ext.python.syntax.NameOrAttribute((py_variant_name,))
    @lru_cache(1)
    def capture_pattern() -> hydra.ext.python.syntax.ClosedPattern:
        return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternCapture(hydra.ext.python.syntax.CapturePattern(hydra.ext.python.syntax.PatternCaptureTarget(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, var_name)))))
    @lru_cache(1)
    def keyword_pattern() -> hydra.ext.python.syntax.KeywordPattern:
        return hydra.ext.python.syntax.KeywordPattern(hydra.ext.python.syntax.Name("value"), cast(hydra.ext.python.syntax.Pattern, hydra.ext.python.syntax.PatternOr(hydra.ext.python.syntax.OrPattern((capture_pattern(),)))))
    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternClass(hydra.ext.python.syntax.ClassPattern(py_var_name_attr(), Nothing(), Just(hydra.ext.python.syntax.KeywordPatterns((keyword_pattern(),))))))

def is_type_variable_name(name: hydra.core.Name) -> bool:
    r"""Check if a name is a type variable (unqualified - no dots)."""

    return hydra.lib.equality.equal(1, hydra.lib.lists.length(hydra.lib.strings.split_on(".", name.value)))

def collect_type_variables(initial: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Collect type variables from a type."""

    while True:
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                return (v := ft.parameter, (body := ft.body, collect_type_variables(hydra.lib.sets.insert(v, initial), body))[1])[1]

            case _:
                return (free_vars := hydra.rewriting.free_variables_in_type(typ), (is_type_var := (lambda n: is_type_variable_name(n)), (filtered_list := hydra.lib.lists.filter((lambda x1: is_type_var(x1)), hydra.lib.sets.to_list(free_vars)), hydra.lib.sets.union(initial, hydra.lib.sets.from_list(filtered_list)))[1])[1])[1]

def cond_import_symbol(name: T0, flag: bool) -> Maybe[T0]:
    r"""Conditionally include a symbol name based on a boolean flag."""

    return hydra.lib.logic.if_else(flag, (lambda : Just(name)), (lambda : Nothing()))

@lru_cache(1)
def dataclass_decorator() -> hydra.ext.python.syntax.NamedExpression:
    r"""Create a @dataclass(frozen=True) decorator."""

    return cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_rhs(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("dataclass"))))), cast(hydra.ext.python.syntax.PrimaryRhs, hydra.ext.python.syntax.PrimaryRhsCall(hydra.ext.python.syntax.Args((), (cast(hydra.ext.python.syntax.KwargOrStarred, hydra.ext.python.syntax.KwargOrStarredKwarg(hydra.ext.python.syntax.Kwarg(hydra.ext.python.syntax.Name("frozen"), hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTrue()))))),), ())))))))

def deconflict_variant_name(is_qualified: bool, env: hydra.ext.python.helpers.PythonEnvironment, union_name: hydra.core.Name, fname: hydra.core.Name, g: hydra.graph.Graph) -> hydra.ext.python.syntax.Name:
    r"""Deconflict a variant name to avoid collisions with type names."""

    @lru_cache(1)
    def candidate_hydra_name() -> hydra.core.Name:
        return hydra.core.Name(hydra.lib.strings.cat2(union_name.value, hydra.formatting.capitalize(fname.value)))
    @lru_cache(1)
    def elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lexical.graph_to_bindings(g)
    @lru_cache(1)
    def collision() -> bool:
        return hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name.value, candidate_hydra_name().value)), elements()))
    return hydra.lib.logic.if_else(collision(), (lambda : hydra.ext.python.syntax.Name(hydra.lib.strings.cat2(hydra.ext.python.names.variant_name(is_qualified, env, union_name, fname).value, "_"))), (lambda : hydra.ext.python.names.variant_name(is_qualified, env, union_name, fname)))

def deduplicate_case_variables(cases_: frozenlist[hydra.core.Field]) -> frozenlist[hydra.core.Field]:
    r"""Rewrite case statements to avoid variable name collisions."""

    def rewrite_case(state: tuple[FrozenDict[hydra.core.Name, int], frozenlist[hydra.core.Field]], field: hydra.core.Field):
        @lru_cache(1)
        def count_by_name() -> FrozenDict[hydra.core.Name, int]:
            return hydra.lib.pairs.first(state)
        @lru_cache(1)
        def done() -> frozenlist[hydra.core.Field]:
            return hydra.lib.pairs.second(state)
        fname = field.name
        fterm = field.term
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    v = lam.parameter
                    mdom = lam.domain
                    body = lam.body
                    return hydra.lib.maybes.maybe((lambda : (hydra.lib.maps.insert(v, 1, count_by_name()), hydra.lib.lists.cons(field, done()))), (lambda count: (count2 := hydra.lib.math.add(count, 1), v2 := hydra.core.Name(hydra.lib.strings.cat2(v.value, hydra.lib.literals.show_int32(count2))), new_body := hydra.reduction.alpha_convert(v, v2, body), new_lam := hydra.core.Lambda(v2, mdom, new_body), new_term := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(new_lam)))), new_field := hydra.core.Field(fname, new_term), (hydra.lib.maps.insert(v, count2, count_by_name()), hydra.lib.lists.cons(new_field, done())))[6]), hydra.lib.maps.lookup(v, count_by_name()))

                case _:
                    return (count_by_name(), hydra.lib.lists.cons(field, done()))
        match hydra.rewriting.deannotate_and_detype_term(fterm):
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)

            case _:
                return (count_by_name(), hydra.lib.lists.cons(field, done()))
    @lru_cache(1)
    def result() -> tuple[FrozenDict[hydra.core.Name, int], frozenlist[hydra.core.Field]]:
        return hydra.lib.lists.foldl((lambda x1, x2: rewrite_case(x1, x2)), (hydra.lib.maps.empty(), ()), cases_)
    return hydra.lib.lists.reverse(hydra.lib.pairs.second(result()))

def set_meta_uses_node(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, b, m.uses_nothing, m.uses_right, m.uses_type_var)

def dig_for_wrap(is_term_annot: bool, meta: hydra.ext.python.helpers.PythonModuleMetadata, typ: hydra.core.Type) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Recursively dig through forall types to find wrap types."""

    while True:
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                is_term_annot = is_term_annot
                meta = meta
                typ = ft.body
                continue

            case hydra.core.TypeWrap():
                return hydra.lib.logic.if_else(is_term_annot, (lambda : meta), (lambda : set_meta_uses_node(meta, True)))

            case _:
                return meta

def eliminate_unit_var(v: hydra.core.Name, term0: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute unit for a variable in a term (for unit variant case handling)."""

    def rewrite_field(rewrite: Callable[[hydra.core.Term], hydra.core.Term], fld: hydra.core.Field) -> hydra.core.Field:
        return hydra.core.Field(fld.name, rewrite(fld.term))
    def rewrite_binding(rewrite: Callable[[hydra.core.Term], hydra.core.Term], bnd: hydra.core.Binding) -> hydra.core.Binding:
        return hydra.core.Binding(bnd.name, rewrite(bnd.term), bnd.type)
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term):
        def _hoist_rewrite_1(recurse, term, v1):
            match v1:
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map((lambda v12: rewrite_field(recurse, v12)), cs.cases))))))))

                case _:
                    return term
        def _hoist_rewrite_2(recurse, term, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(lam.parameter, v), (lambda : term), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(lam.parameter, lam.domain, recurse(lam.body))))))))

                case hydra.core.FunctionElimination(value=e):
                    return _hoist_rewrite_1(recurse, term, e)

                case _:
                    return term
        match hydra.rewriting.deannotate_and_detype_term(term):
            case hydra.core.TermVariable(value=n):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(n, v), (lambda : cast(hydra.core.Term, hydra.core.TermUnit())), (lambda : term))

            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))

            case hydra.core.TermApplication(value=app):
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(app.function), recurse(app.argument))))

            case hydra.core.TermFunction(value=f):
                return _hoist_rewrite_2(recurse, term, f)

            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda v1: rewrite_binding(recurse, v1)), lt.bindings), recurse(lt.body))))

            case hydra.core.TermList(value=ts):
                return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(recurse, ts)))

            case hydra.core.TermMap(value=m):
                return cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda kv: (recurse(hydra.lib.pairs.first(kv)), recurse(hydra.lib.pairs.second(kv)))), hydra.lib.maps.to_list(m)))))

            case hydra.core.TermRecord(value=rec):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rec.type_name, hydra.lib.lists.map((lambda v1: rewrite_field(recurse, v1)), rec.fields))))

            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(recurse, s)))

            case hydra.core.TermUnion(value=inj):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, rewrite_field(recurse, inj.field))))

            case hydra.core.TermMaybe(value=mt):
                return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(recurse, mt)))

            case hydra.core.TermPair(value=p):
                return cast(hydra.core.Term, hydra.core.TermPair((recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))))

            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))

            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.bimap(recurse, recurse, e)))

            case hydra.core.TermTypeApplication(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(ta.body), ta.type)))

            case hydra.core.TermTypeLambda(value=tl):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, recurse(tl.body))))

            case _:
                return term
    def go(term: hydra.core.Term) -> hydra.core.Term:
        return rewrite((lambda x1: go(x1)), term)
    return go(term0)

def empty_metadata(ns: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Create an initial empty metadata record with given namespaces."""

    return hydra.ext.python.helpers.PythonModuleMetadata(ns, hydra.lib.sets.empty(), False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False)

def py_int(n: int) -> hydra.ext.python.syntax.Expression:
    r"""Create integer literal expression."""

    return hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(n)))))

def wrap_in_nullary_lambda(expr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation."""

    return cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionLambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(Nothing(), (), (), Nothing()), expr)))

def make_thunk(pbody: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization."""

    return hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("lru_cache"))))), (py_int(1),))), (wrap_in_nullary_lambda(pbody),))

def make_simple_lambda(arity: int, lhs: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Wrap a bare reference to a polymorphic function in an uncurried lambda."""

    @lru_cache(1)
    def args() -> frozenlist[hydra.ext.python.syntax.Name]:
        return hydra.lib.lists.map((lambda i: hydra.ext.python.syntax.Name(hydra.lib.strings.cat2("x", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, arity))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(arity, 0), (lambda : lhs), (lambda : cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionLambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(Nothing(), hydra.lib.lists.map((lambda a: hydra.ext.python.syntax.LambdaParamNoDefault(a)), args()), (), Nothing()), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(lhs), hydra.lib.lists.map((lambda a: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(a))))), Nothing()))))))))), ()))),)),))))), args())))))))

def make_uncurried_lambda(params: frozenlist[hydra.ext.python.syntax.Name], body: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Create an uncurried lambda with multiple parameters."""

    return cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionLambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(Nothing(), hydra.lib.lists.map((lambda p: hydra.ext.python.syntax.LambdaParamNoDefault(p)), params), (), Nothing()), body)))

def encode_variable(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, args: frozenlist[hydra.ext.python.syntax.Expression]) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression]:
    r"""Encode a variable reference to a Python expression."""

    @lru_cache(1)
    def g() -> hydra.graph.Graph:
        return python_environment_get_graph(env)
    tc = env.graph
    tc_types = tc.bound_types
    tc_lambda_vars = tc.lambda_variables
    tc_metadata = tc.metadata
    inline_vars = env.inline_variables
    @lru_cache(1)
    def m_typ_scheme() -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maps.lookup(name, tc_types)
    @lru_cache(1)
    def m_typ() -> Maybe[hydra.core.Type]:
        return hydra.lib.maybes.map((lambda ts_: ts_.type), m_typ_scheme())
    @lru_cache(1)
    def as_variable() -> hydra.ext.python.syntax.Expression:
        return hydra.ext.python.names.term_variable_reference(env, name)
    @lru_cache(1)
    def as_function_call() -> hydra.ext.python.syntax.Expression:
        return hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), args)
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(args)), (lambda : hydra.lib.maybes.maybe((lambda : Right(as_function_call())), (lambda prim: (prim_arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.equal(prim_arity, hydra.lib.lists.length(args)), (lambda : Right(as_function_call())), (lambda : (num_remaining := hydra.lib.math.sub(prim_arity, hydra.lib.lists.length(args)), (remaining_params := hydra.lib.lists.map((lambda i: hydra.ext.python.syntax.Name(hydra.lib.strings.cat2("x", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, num_remaining)), (remaining_exprs := hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(n))))), Nothing()))))))))), ()))),)),))))), remaining_params), (all_args := hydra.lib.lists.concat2(args, remaining_exprs), (full_call := hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), all_args), Right(make_uncurried_lambda(remaining_params, full_call)))[1])[1])[1])[1])[1])))[1]), hydra.lexical.lookup_primitive(g(), name))), (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc_lambda_vars), (lambda : Right(as_variable())), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, inline_vars), (lambda : Right(as_variable())), (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("Unknown variable: ", name.value)))), cx))), (lambda _: Right(as_function_call())), hydra.lib.maps.lookup(name, tc_metadata))), (lambda el: (el_trivial1 := hydra.coder_utils.is_trivial_term(el.term), hydra.lib.maybes.maybe((lambda : Right(as_variable())), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_scheme_arity(ts), 0), hydra.coder_utils.is_complex_binding(tc, el)), hydra.lib.logic.not_(el_trivial1)), (lambda : Right(as_function_call())), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : make_simple_lambda(hydra.arity.type_arity(ts.type), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]))), el.type))[1]), hydra.lexical.lookup_element(g(), name))), (lambda prim: (prim_arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.equal(prim_arity, 0), (lambda : Right(as_function_call())), (lambda : (ts := prim.type, (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : make_simple_lambda(hydra.arity.type_arity(ts.type), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1])[1])))[1]), hydra.lexical.lookup_primitive(g(), name))))))), (lambda typ: hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc_lambda_vars), (lambda : Right(as_variable())), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, inline_vars), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.member(name, tc_metadata)), (lambda : hydra.lib.maybes.maybe((lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]), (lambda el: (el_trivial := hydra.coder_utils.is_trivial_term(el.term), hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), hydra.lib.logic.not_(el_trivial)), (lambda : Right(as_function_call())), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]))), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), hydra.coder_utils.is_complex_binding(tc, el)), hydra.lib.logic.not_(el_trivial)), (lambda : Right(as_function_call())), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]))), el.type))[1]), hydra.lexical.lookup_element(g(), name))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), hydra.coder_utils.is_complex_variable(tc, name)), (lambda : Right(as_function_call())), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable())), (lambda : as_variable())), Right(as_function_ref))[1]))))))))), m_typ())))

def make_curried_lambda(params: frozenlist[hydra.ext.python.syntax.Name], body: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Create a curried lambda chain from a list of parameter names and a body."""

    return hydra.lib.lists.foldl((lambda acc, p: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionLambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(Nothing(), (hydra.ext.python.syntax.LambdaParamNoDefault(p),), (), Nothing()), acc)))), body, hydra.lib.lists.reverse(params))

def unsupported_expression(msg: str) -> hydra.ext.python.syntax.Expression:
    r"""Create an expression that calls hydra.dsl.python.unsupported(message) at runtime."""

    return hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("hydra")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.syntax.Name("dsl")), hydra.ext.python.syntax.Name("python")), hydra.ext.python.syntax.Name("unsupported"))), (hydra.ext.python.utils.string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.DOUBLE, msg),))

def encode_float_value(fv: hydra.core.FloatValue) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a float value to a Python expression."""

    match fv:
        case hydra.core.FloatValueBigfloat(value=f):
            return Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Decimal")), (hydra.ext.python.utils.single_quoted_string(hydra.lib.literals.show_bigfloat(f)),)))

        case hydra.core.FloatValueFloat32(value=f2):
            return Right(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberFloat(hydra.lib.literals.float32_to_bigfloat(f2)))))))

        case hydra.core.FloatValueFloat64(value=f3):
            return Right(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberFloat(hydra.lib.literals.float64_to_bigfloat(f3)))))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_integer_value(iv: hydra.core.IntegerValue) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode an integer value to a Python expression."""

    def to_py_int(n: int) -> Either[T1, hydra.ext.python.syntax.Expression]:
        return Right(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(n))))))
    match iv:
        case hydra.core.IntegerValueBigint(value=i):
            return to_py_int(i)

        case hydra.core.IntegerValueInt8(value=i2):
            return to_py_int(hydra.lib.literals.int8_to_bigint(i2))

        case hydra.core.IntegerValueInt16(value=i3):
            return to_py_int(hydra.lib.literals.int16_to_bigint(i3))

        case hydra.core.IntegerValueInt32(value=i4):
            return to_py_int(hydra.lib.literals.int32_to_bigint(i4))

        case hydra.core.IntegerValueInt64(value=i5):
            return to_py_int(hydra.lib.literals.int64_to_bigint(i5))

        case hydra.core.IntegerValueUint8(value=i6):
            return to_py_int(hydra.lib.literals.uint8_to_bigint(i6))

        case hydra.core.IntegerValueUint16(value=i7):
            return to_py_int(hydra.lib.literals.uint16_to_bigint(i7))

        case hydra.core.IntegerValueUint32(value=i8):
            return to_py_int(hydra.lib.literals.uint32_to_bigint(i8))

        case hydra.core.IntegerValueUint64(value=i9):
            return to_py_int(hydra.lib.literals.uint64_to_bigint(i9))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal(lit: hydra.core.Literal) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a literal value to a Python expression."""

    match lit:
        case hydra.core.LiteralBinary(value=bs):
            @lru_cache(1)
            def byte_values() -> frozenlist[int]:
                return hydra.lib.literals.binary_to_bytes(bs)
            return Right(hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("bytes"))))), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomList(hydra.ext.python.utils.py_list(hydra.lib.lists.map((lambda byte_val: hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(byte_val))))))), byte_values()))))),)))

        case hydra.core.LiteralBoolean(value=b):
            return Right(hydra.ext.python.utils.py_atom_to_py_expression(hydra.lib.logic.if_else(b, (lambda : cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTrue())), (lambda : cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomFalse())))))

        case hydra.core.LiteralFloat(value=f):
            return encode_float_value(f)

        case hydra.core.LiteralInteger(value=i):
            return encode_integer_value(i)

        case hydra.core.LiteralString(value=s):
            return Right(hydra.ext.python.utils.string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.DOUBLE, s))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_type(lt: hydra.core.LiteralType) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a literal type to a Python type expression."""

    @lru_cache(1)
    def find_name():
        def _hoist_find_name_1(v1):
            match v1:
                case hydra.core.FloatType.BIGFLOAT:
                    return "Decimal"

                case hydra.core.FloatType.FLOAT32:
                    return "float"

                case hydra.core.FloatType.FLOAT64:
                    return "float"

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match lt:
            case hydra.core.LiteralTypeBinary():
                return "bytes"

            case hydra.core.LiteralTypeBoolean():
                return "bool"

            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_find_name_1(ft)

            case hydra.core.LiteralTypeInteger():
                return "int"

            case hydra.core.LiteralTypeString():
                return "str"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return Right(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name(find_name())))))), Nothing()))))))))), ()))),)),)))))

def encode_application_type(env: hydra.ext.python.helpers.PythonEnvironment, at: hydra.core.ApplicationType) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode an application type to Python expression."""

    def gather_params(t: hydra.core.Type, ps: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Type, frozenlist[hydra.core.Type]]:
        while True:
            match hydra.rewriting.deannotate_type(t):
                case hydra.core.TypeApplication(value=app_t):
                    t = app_t.function
                    ps = hydra.lib.lists.cons(app_t.argument, ps)
                    continue

                case hydra.core.TypeAnnotated():
                    return (t, ps)

                case hydra.core.TypeFunction():
                    return (t, ps)

                case hydra.core.TypeForall():
                    return (t, ps)

                case hydra.core.TypeList():
                    return (t, ps)

                case hydra.core.TypeLiteral():
                    return (t, ps)

                case hydra.core.TypeMap():
                    return (t, ps)

                case hydra.core.TypeMaybe():
                    return (t, ps)

                case hydra.core.TypeEither():
                    return (t, ps)

                case hydra.core.TypePair():
                    return (t, ps)

                case hydra.core.TypeRecord():
                    return (t, ps)

                case hydra.core.TypeSet():
                    return (t, ps)

                case hydra.core.TypeUnion():
                    return (t, ps)

                case hydra.core.TypeUnit():
                    return (t, ps)

                case hydra.core.TypeVariable():
                    return (t, ps)

                case hydra.core.TypeWrap():
                    return (t, ps)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def body_and_args() -> tuple[hydra.core.Type, frozenlist[hydra.core.Type]]:
        return gather_params(cast(hydra.core.Type, hydra.core.TypeApplication(at)), ())
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return hydra.lib.pairs.first(body_and_args())
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.second(body_and_args())
    return hydra.lib.eithers.bind(encode_type(env, body()), (lambda py_body: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_type(env, v1)), args()), (lambda py_args: Right(hydra.ext.python.utils.primary_and_params(hydra.ext.python.utils.py_expression_to_py_primary(py_body), py_args))))))

def encode_forall_type(env: hydra.ext.python.helpers.PythonEnvironment, lt: hydra.core.ForallType) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a forall type to Python expression."""

    def gather_params(t: hydra.core.Type, ps: frozenlist[hydra.core.Name]) -> tuple[hydra.core.Type, frozenlist[hydra.core.Name]]:
        while True:
            match hydra.rewriting.deannotate_type(t):
                case hydra.core.TypeForall(value=forall_t):
                    t = forall_t.body
                    ps = hydra.lib.lists.cons(forall_t.parameter, ps)
                    continue

                case hydra.core.TypeAnnotated():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeApplication():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeFunction():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeList():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeLiteral():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeMap():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeMaybe():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeEither():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypePair():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeRecord():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeSet():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeUnion():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeUnit():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeVariable():
                    return (t, hydra.lib.lists.reverse(ps))

                case hydra.core.TypeWrap():
                    return (t, hydra.lib.lists.reverse(ps))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def body_and_params() -> tuple[hydra.core.Type, frozenlist[hydra.core.Name]]:
        return gather_params(cast(hydra.core.Type, hydra.core.TypeForall(lt)), ())
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return hydra.lib.pairs.first(body_and_params())
    @lru_cache(1)
    def params() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.second(body_and_params())
    return hydra.lib.eithers.bind(encode_type(env, body()), (lambda py_body: Right(hydra.ext.python.utils.primary_and_params(hydra.ext.python.utils.py_expression_to_py_primary(py_body), hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name(n.value)))))), Nothing()))))))))), ()))),)),))))), params())))))

def encode_function_type(env: hydra.ext.python.helpers.PythonEnvironment, ft: hydra.core.FunctionType) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a function type to Python Callable expression."""

    def gather_params(rdoms: frozenlist[hydra.core.Type], ftype: hydra.core.FunctionType) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        while True:
            inner_cod = ftype.codomain
            dom = ftype.domain
            match hydra.rewriting.deannotate_type(inner_cod):
                case hydra.core.TypeFunction(value=ft2):
                    rdoms = hydra.lib.lists.cons(dom, rdoms)
                    ftype = ft2
                    continue

                case hydra.core.TypeAnnotated():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeApplication():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeForall():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeList():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeLiteral():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeMap():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeMaybe():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeEither():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypePair():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeRecord():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeSet():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeUnion():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeUnit():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeVariable():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case hydra.core.TypeWrap():
                    return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom, rdoms)), inner_cod)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def doms_and_cod() -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        return gather_params((), ft)
    @lru_cache(1)
    def doms() -> frozenlist[hydra.core.Type]:
        return hydra.lib.pairs.first(doms_and_cod())
    @lru_cache(1)
    def cod() -> hydra.core.Type:
        return hydra.lib.pairs.second(doms_and_cod())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_type(env, v1)), doms()), (lambda pydoms: hydra.lib.eithers.bind(encode_type(env, cod()), (lambda pycod: Right(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Callable"))))), hydra.ext.python.utils.py_primary_to_py_slice(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomList(hydra.ext.python.utils.py_list(pydoms)))))), (cast(hydra.ext.python.syntax.SliceOrStarredExpression, hydra.ext.python.syntax.SliceOrStarredExpressionSlice(hydra.ext.python.utils.py_expression_to_py_slice(pycod))),))))))))

def encode_type(env: hydra.ext.python.helpers.PythonEnvironment, typ: hydra.core.Type) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a Hydra type to a Python type expression."""

    @lru_cache(1)
    def dflt() -> Either[T1, hydra.ext.python.syntax.Expression]:
        return Right(hydra.ext.python.utils.double_quoted_string(hydra.lib.strings.cat2("type = ", hydra.show.core.type(hydra.rewriting.deannotate_type(typ)))))
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeApplication(value=at):
            return encode_application_type(env, at)

        case hydra.core.TypeFunction(value=ft):
            return encode_function_type(env, ft)

        case hydra.core.TypeForall(value=lt):
            return encode_forall_type(env, lt)

        case hydra.core.TypeList(value=et):
            return hydra.lib.eithers.bind(encode_type(env, et), (lambda pyet: Right(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("frozenlist"), (pyet,)))))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.eithers.bind(encode_type(env, mt.keys), (lambda pykt: hydra.lib.eithers.bind(encode_type(env, mt.values), (lambda pyvt: Right(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("FrozenDict"), (pykt, pyvt)))))))

        case hydra.core.TypeLiteral(value=lt2):
            return encode_literal_type(lt2)

        case hydra.core.TypeMaybe(value=et2):
            return hydra.lib.eithers.bind(encode_type(env, et2), (lambda ptype: Right(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Maybe"))))), (ptype,))))))

        case hydra.core.TypeEither(value=either_t):
            return hydra.lib.eithers.bind(encode_type(env, either_t.left), (lambda pyleft: hydra.lib.eithers.bind(encode_type(env, either_t.right), (lambda pyright: Right(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Either"))))), (pyleft, pyright))))))))

        case hydra.core.TypePair(value=pair_t):
            return hydra.lib.eithers.bind(encode_type(env, pair_t.first), (lambda py_first: hydra.lib.eithers.bind(encode_type(env, pair_t.second), (lambda py_second: Right(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("tuple"), (py_first, py_second)))))))

        case hydra.core.TypeRecord():
            return dflt()

        case hydra.core.TypeSet(value=et3):
            return hydra.lib.eithers.bind(encode_type(env, et3), (lambda pyet: Right(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("frozenset"), (pyet,)))))

        case hydra.core.TypeUnion():
            return dflt()

        case hydra.core.TypeUnit():
            return Right(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.utils.py_none))

        case hydra.core.TypeVariable(value=name):
            return Right(hydra.ext.python.names.type_variable_reference(env, name))

        case hydra.core.TypeWrap():
            return dflt()

        case hydra.core.TypeAnnotated():
            return dflt()

        case _:
            raise AssertionError("Unreachable: all variants handled")

def with_let_inline(env: hydra.ext.python.helpers.PythonEnvironment, lt: hydra.core.Let, body: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    r"""Execute a computation with inline let context (for walrus operators)."""

    @lru_cache(1)
    def binding_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda b: b.name), lt.bindings)
    @lru_cache(1)
    def inline_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(binding_names())
    def no_metadata(tc: T1, b: T2) -> Maybe[T3]:
        return Nothing()
    return hydra.schemas.with_let_context((lambda x1: python_environment_get_graph(x1)), (lambda x1, x2: python_environment_set_graph(x1, x2)), (lambda x1, x2: no_metadata(x1, x2)), env, lt, (lambda inner_env: (updated_env := hydra.ext.python.helpers.PythonEnvironment(inner_env.namespaces, inner_env.bound_type_variables, inner_env.graph, inner_env.nullary_bindings, inner_env.version, inner_env.skip_casts, hydra.lib.sets.union(inline_vars(), inner_env.inline_variables)), body(updated_env))[1]))

def with_type_lambda(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.TypeLambda, v3: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    r"""Execute a computation with type lambda context."""

    return hydra.schemas.with_type_lambda_context((lambda x1: python_environment_get_graph(x1)), (lambda x1, x2: python_environment_set_graph(x1, x2)), v1, v2, v3)

def is_variant_unit_type(row_type: frozenlist[hydra.core.FieldType], field_name: hydra.core.Name) -> bool:
    r"""Check if a variant field has unit type."""

    @lru_cache(1)
    def mfield() -> Maybe[hydra.core.FieldType]:
        return hydra.lib.lists.find((lambda ft: hydra.lib.equality.equal(ft.name, field_name)), row_type)
    return hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda ft: hydra.schemas.is_unit_type(hydra.rewriting.deannotate_type(ft.type))), mfield()))

def wrap_lazy_arguments(name: hydra.core.Name, args: frozenlist[hydra.ext.python.syntax.Expression]) -> frozenlist[hydra.ext.python.syntax.Expression]:
    r"""Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation."""

    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.logic.ifElse")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : (hydra.lib.lists.at(0, args), wrap_in_nullary_lambda(hydra.lib.lists.at(1, args)), wrap_in_nullary_lambda(hydra.lib.lists.at(2, args)))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.cases")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : (hydra.lib.lists.at(0, args), wrap_in_nullary_lambda(hydra.lib.lists.at(1, args)), hydra.lib.lists.at(2, args))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.or_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.maybe")), hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.maybes.fromMaybe"))), hydra.lib.equality.gte(hydra.lib.lists.length(args), 1)), (lambda : hydra.lib.lists.cons(wrap_in_nullary_lambda(hydra.lib.lists.at(0, args)), hydra.lib.lists.tail(args))), (lambda : args))))))

def function_arity_with_primitives(graph: hydra.graph.Graph, f: hydra.core.Function) -> int:
    r"""Calculate function arity with proper primitive handling."""

    match f:
        case hydra.core.FunctionElimination():
            return 1

        case hydra.core.FunctionLambda(value=lam):
            return hydra.lib.math.add(1, term_arity_with_primitives(graph, lam.body))

        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.maybes.maybe((lambda : 0), (lambda prim: hydra.arity.primitive_arity(prim)), hydra.lib.maps.lookup(name, graph.primitives))

        case _:
            return 0

def term_arity_with_primitives(graph: hydra.graph.Graph, term: hydra.core.Term) -> int:
    r"""Calculate term arity with proper primitive handling."""

    match hydra.rewriting.deannotate_and_detype_term(term):
        case hydra.core.TermApplication(value=app):
            return hydra.lib.math.max(0, hydra.lib.math.sub(term_arity_with_primitives(graph, app.function), 1))

        case hydra.core.TermFunction(value=f):
            return function_arity_with_primitives(graph, f)

        case hydra.core.TermVariable(value=name):
            return hydra.lib.maybes.maybe((lambda : 0), (lambda el: hydra.lib.maybes.maybe((lambda : hydra.arity.term_arity(el.term)), (lambda ts: hydra.arity.type_scheme_arity(ts)), el.type)), hydra.lexical.lookup_element(graph, name))

        case _:
            return 0

def encode_application(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, app: hydra.core.Application) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression]:
    r"""Encode a function application to a Python expression."""

    @lru_cache(1)
    def g() -> hydra.graph.Graph:
        return python_environment_get_graph(env)
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermApplication(app))
    @lru_cache(1)
    def gathered() -> tuple[hydra.core.Term, frozenlist[hydra.core.Term]]:
        return hydra.coder_utils.gather_args(term(), ())
    @lru_cache(1)
    def fun() -> hydra.core.Term:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.second(gathered())
    @lru_cache(1)
    def known_arity() -> int:
        return term_arity_with_primitives(g(), fun())
    @lru_cache(1)
    def arity() -> int:
        return hydra.lib.math.max(known_arity(), hydra.lib.lists.length(args()))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda t: encode_term_inline(cx, env, False, t)), args()), (lambda pargs: (hargs := hydra.lib.lists.take(arity(), pargs), rargs := hydra.lib.lists.drop(arity(), pargs), hydra.lib.eithers.bind(encode_application_inner(cx, env, fun(), hargs, rargs), (lambda result: (lhs := hydra.lib.pairs.first(result), remaining_rargs := hydra.lib.pairs.second(result), pyapp := hydra.lib.lists.foldl((lambda t, a: hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(t), (a,))), lhs, remaining_rargs), Right(pyapp))[3])))[2]))

def encode_application_inner(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, fun: hydra.core.Term, hargs: frozenlist[hydra.ext.python.syntax.Expression], rargs: frozenlist[hydra.ext.python.syntax.Expression]):
    r"""Inner helper for encodeApplication."""

    @lru_cache(1)
    def first_arg() -> hydra.ext.python.syntax.Expression:
        return hydra.lib.lists.head(hargs)
    @lru_cache(1)
    def rest_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
        return hydra.lib.lists.tail(hargs)
    def with_rest(e: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(rest_args()), (lambda : e), (lambda : hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(e), rest_args())))
    @lru_cache(1)
    def default_case() -> Either[hydra.context.InContext[hydra.error.Error], tuple[hydra.ext.python.syntax.Expression, frozenlist[hydra.ext.python.syntax.Expression]]]:
        return hydra.lib.eithers.bind(encode_term_inline(cx, env, False, fun), (lambda pfun: Right((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(pfun), hargs), rargs))))
    def _hoist_body_1(v1):
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                fname = proj.field
                @lru_cache(1)
                def field_expr() -> hydra.ext.python.syntax.Expression:
                    return hydra.ext.python.utils.project_from_expression(first_arg(), hydra.ext.python.names.encode_field_name(env, fname))
                return Right((with_rest(field_expr()), rargs))

            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.eithers.bind(encode_union_elimination_inline(cx, env, cs, first_arg()), (lambda inline_expr: Right((with_rest(inline_expr), rargs))))

            case hydra.core.EliminationWrap():
                @lru_cache(1)
                def value_expr() -> hydra.ext.python.syntax.Expression:
                    return hydra.ext.python.utils.project_from_expression(first_arg(), hydra.ext.python.syntax.Name("value"))
                @lru_cache(1)
                def all_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
                    return hydra.lib.lists.concat2(rest_args(), rargs)
                return hydra.lib.logic.if_else(hydra.lib.lists.null(all_args()), (lambda : Right((value_expr(), ()))), (lambda : Right((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(value_expr()), all_args()), ()))))

            case _:
                return default_case()
    def _hoist_body_2(v1):
        match v1:
            case hydra.core.FunctionElimination(value=elm):
                return _hoist_body_1(elm)

            case hydra.core.FunctionPrimitive(value=name):
                @lru_cache(1)
                def wrapped_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
                    return wrap_lazy_arguments(name, hargs)
                return hydra.lib.eithers.bind(encode_variable(cx, env, name, wrapped_args()), (lambda expr: Right((expr, rargs))))

            case hydra.core.FunctionLambda():
                return hydra.lib.eithers.bind(encode_term_inline(cx, env, False, fun), (lambda pfun: Right((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(pfun), hargs), rargs))))

            case _:
                return default_case()
    match hydra.rewriting.deannotate_and_detype_term(fun):
        case hydra.core.TermFunction(value=f):
            return _hoist_body_2(f)

        case hydra.core.TermVariable(value=name):
            @lru_cache(1)
            def g() -> hydra.graph.Graph:
                return python_environment_get_graph(env)
            @lru_cache(1)
            def all_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
                return hydra.lib.lists.concat2(hargs, rargs)
            return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(encode_variable(cx, env, name, hargs), (lambda expr: Right((expr, rargs))))), (lambda el: hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(encode_variable(cx, env, name, hargs), (lambda expr: Right((expr, rargs))))), (lambda ts: (el_arity := hydra.arity.type_scheme_arity(ts), consume_count := hydra.lib.math.min(el_arity, hydra.lib.lists.length(all_args())), consumed_args := hydra.lib.lists.take(consume_count, all_args()), remaining_args := hydra.lib.lists.drop(consume_count, all_args()), hydra.lib.logic.if_else(hydra.lib.lists.null(consumed_args), (lambda : hydra.lib.eithers.bind(encode_variable(cx, env, name, ()), (lambda expr: Right((expr, rargs))))), (lambda : Right((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), consumed_args), remaining_args)))))[4]), el.type)), hydra.lexical.lookup_element(g(), name))

        case _:
            return default_case()

def encode_binding_as_assignment(cx: hydra.context.Context, allow_thunking: bool, env: hydra.ext.python.helpers.PythonEnvironment, binding: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.NamedExpression]:
    r"""Encode a binding as a walrus operator assignment."""

    name = binding.name
    term = binding.term
    mts = binding.type
    @lru_cache(1)
    def py_name() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, name)
    return hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda pbody: (tc := env.graph, is_complex_var := hydra.coder_utils.is_complex_variable(tc, name), term_is_complex := hydra.coder_utils.is_complex_term(tc, term), is_trivial := hydra.coder_utils.is_trivial_term(term), needs_thunk := hydra.lib.logic.if_else(is_trivial, (lambda : False), (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.logic.and_(allow_thunking, hydra.lib.logic.or_(is_complex_var, term_is_complex))), (lambda ts: hydra.lib.logic.and_(allow_thunking, hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_scheme_arity(ts), 0), hydra.lib.logic.or_(is_complex_var, term_is_complex)))), mts))), pterm := hydra.lib.logic.if_else(needs_thunk, (lambda : make_thunk(pbody)), (lambda : pbody)), Right(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionAssignment(hydra.ext.python.syntax.AssignmentExpression(py_name(), pterm)))))[6]))

def encode_function(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, f: hydra.core.Function):
    def _hoist_hydra_ext_python_coder_encode_function_1(env, v1):
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                fname = proj.field
                return Right(make_curried_lambda((hydra.ext.python.syntax.Name("v1"),), hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("v1")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.names.encode_field_name(env, fname))))

            case hydra.core.EliminationWrap():
                return Right(make_curried_lambda((hydra.ext.python.syntax.Name("v1"),), hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("v1")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.syntax.Name("value"))))

            case hydra.core.EliminationUnion():
                return Right(unsupported_expression("case expressions as values are not yet supported"))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match f:
        case hydra.core.FunctionLambda(value=lam):
            return hydra.lib.eithers.bind(analyze_python_function(cx, env, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(lam))))), (lambda fs: (params := fs.params, bindings := fs.bindings, inner_body := fs.body, inner_env0 := fs.environment, binding_names := hydra.lib.lists.map((lambda b: b.name), bindings), inner_env := hydra.ext.python.helpers.PythonEnvironment(inner_env0.namespaces, inner_env0.bound_type_variables, inner_env0.graph, inner_env0.nullary_bindings, inner_env0.version, inner_env0.skip_casts, hydra.lib.sets.union(hydra.lib.sets.from_list(binding_names), inner_env0.inline_variables)), hydra.lib.eithers.bind(encode_term_inline(cx, inner_env, False, inner_body), (lambda pbody: (pparams := hydra.lib.lists.map((lambda v1: hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, inner_env, v1)), params), hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : Right(make_uncurried_lambda(pparams, pbody))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_binding_as_assignment(cx, False, inner_env, v1)), bindings), (lambda pbinding_exprs: (pbinding_star_exprs := hydra.lib.lists.map((lambda ne: cast(hydra.ext.python.syntax.StarNamedExpression, hydra.ext.python.syntax.StarNamedExpressionSimple(ne))), pbinding_exprs), pbody_star_expr := hydra.ext.python.utils.py_expression_to_py_star_named_expression(pbody), tuple_elements := hydra.lib.lists.concat2(pbinding_star_exprs, (pbody_star_expr,)), tuple_expr := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(tuple_elements)))), index_value := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(hydra.lib.lists.length(bindings))))))), indexed_expr := hydra.ext.python.utils.primary_with_expression_slices(hydra.ext.python.utils.py_expression_to_py_primary(tuple_expr), (index_value,)), Right(make_uncurried_lambda(pparams, hydra.ext.python.utils.py_primary_to_py_expression(indexed_expr))))[6])))))[1])))[6]))

        case hydra.core.FunctionPrimitive(value=name):
            return encode_variable(cx, env, name, ())

        case hydra.core.FunctionElimination(value=e):
            return _hoist_hydra_ext_python_coder_encode_function_1(env, e)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_term_inline(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, no_cast: bool, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression]:
    r"""Encode a term to a Python expression (inline form)."""

    def encode(t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression]:
        return encode_term_inline(cx, env, False, t)
    def strip_type_apps(t: hydra.core.Term) -> hydra.core.Term:
        while True:
            match t:
                case hydra.core.TermAnnotated(value=ann):
                    t = ann.body
                    continue

                case hydra.core.TermTypeApplication(value=ta):
                    t = ta.body
                    continue

                case _:
                    return t
    def with_cast(pyexp: hydra.ext.python.syntax.Expression) -> Either[T1, hydra.ext.python.syntax.Expression]:
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(no_cast, env.skip_casts), (lambda : Right(pyexp)), (lambda : (tc := env.graph, (mtyp := hydra.lib.eithers.map((lambda _r: hydra.lib.pairs.first(_r)), hydra.checking.type_of(cx, tc, (), term)), hydra.lib.eithers.either((lambda _: Right(pyexp)), (lambda typ: hydra.lib.eithers.either((lambda _: Right(pyexp)), (lambda pytyp: Right(hydra.ext.python.utils.cast_to(pytyp, pyexp))), encode_type(env, typ))), mtyp))[1])[1]))
    match hydra.rewriting.deannotate_and_detype_term(term):
        case hydra.core.TermApplication(value=app):
            return encode_application(cx, env, app)

        case hydra.core.TermEither(value=et):
            return hydra.lib.eithers.either((lambda t1: hydra.lib.eithers.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Left")), (pyexp,)))))), (lambda t1: hydra.lib.eithers.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Right")), (pyexp,)))))), et)

        case hydra.core.TermFunction(value=f):
            return encode_function(cx, env, f)

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            body = lt.body
            return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : encode_term_inline(cx, env, False, body)), (lambda : with_let_inline(env, lt, (lambda inner_env: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_binding_as_assignment(cx, False, inner_env, v1)), bindings), (lambda pbinding_exprs: hydra.lib.eithers.bind(encode_term_inline(cx, inner_env, False, body), (lambda pbody: (pbinding_star_exprs := hydra.lib.lists.map((lambda ne: cast(hydra.ext.python.syntax.StarNamedExpression, hydra.ext.python.syntax.StarNamedExpressionSimple(ne))), pbinding_exprs), pbody_star_expr := hydra.ext.python.utils.py_expression_to_py_star_named_expression(pbody), tuple_elements := hydra.lib.lists.concat2(pbinding_star_exprs, (pbody_star_expr,)), tuple_expr := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(tuple_elements)))), index_value := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(hydra.lib.lists.length(bindings))))))), indexed_expr := hydra.ext.python.utils.primary_with_expression_slices(hydra.ext.python.utils.py_expression_to_py_primary(tuple_expr), (index_value,)), Right(hydra.ext.python.utils.py_primary_to_py_expression(indexed_expr)))[6]))))))))

        case hydra.core.TermList(value=terms):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), terms), (lambda py_exprs: Right(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_expression_to_py_star_named_expression(x1)), py_exprs))))))))

        case hydra.core.TermLiteral(value=lit):
            return encode_literal(lit)

        case hydra.core.TermMap(value=m):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.eithers.bind(encode(k), (lambda py_k: hydra.lib.eithers.bind(encode(v), (lambda py_v: Right(cast(hydra.ext.python.syntax.DoubleStarredKvpair, hydra.ext.python.syntax.DoubleStarredKvpairPair(hydra.ext.python.syntax.Kvpair(py_k, py_v)))))))))[2]), hydra.lib.maps.to_list(m)), (lambda pairs: Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("FrozenDict")), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomDict(hydra.ext.python.syntax.Dict(pairs)))),)))))

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Nothing")), ()))), (lambda t1: hydra.lib.eithers.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Just")), (pyexp,)))))), mt)

        case hydra.core.TermPair(value=p):
            @lru_cache(1)
            def t1() -> hydra.core.Term:
                return hydra.lib.pairs.first(p)
            @lru_cache(1)
            def t2() -> hydra.core.Term:
                return hydra.lib.pairs.second(p)
            return hydra.lib.eithers.bind(encode(t1()), (lambda py_expr1: hydra.lib.eithers.bind(encode(t2()), (lambda py_expr2: Right(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple((hydra.ext.python.utils.py_expression_to_py_star_named_expression(py_expr1), hydra.ext.python.utils.py_expression_to_py_star_named_expression(py_expr2)))))))))))

        case hydra.core.TermRecord(value=r):
            tname = r.type_name
            fields = r.fields
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda fld: encode(fld.term)), fields), (lambda pargs: Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name_qualified(env, tname)), pargs))))

        case hydra.core.TermSet(value=s):
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode(x1)), hydra.lib.sets.to_list(s)), (lambda py_els: Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("frozenset")), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomSet(hydra.ext.python.syntax.Set(hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_expression_to_py_star_named_expression(x1)), py_els))))),)))))

        case hydra.core.TermTypeApplication(value=ta):
            body = ta.body
            return hydra.lib.eithers.bind(encode_term_inline(cx, env, True, strip_type_apps(body)), (lambda pybase: with_cast(pybase)))

        case hydra.core.TermTypeLambda(value=tl):
            body = tl.body
            return with_type_lambda(env, tl, (lambda env2: encode_term_inline(cx, env2, no_cast, body)))

        case hydra.core.TermUnion(value=inj):
            tname = inj.type_name
            field = inj.field
            return hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(rt), (lambda : Right(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.names.encode_name_qualified(env, tname)), hydra.ext.python.names.encode_enum_value(env, field.name)))), (lambda : (fname := field.name, (is_unit_variant := hydra.lib.maybes.maybe((lambda : False), (lambda ft: hydra.schemas.is_unit_type(hydra.rewriting.deannotate_type(ft.type))), hydra.lib.lists.find((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), rt)), hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.schemas.is_unit_term(field.term), is_unit_variant), (lambda : Right(())), (lambda : hydra.lib.eithers.bind(encode(field.term), (lambda parg: Right((parg,)))))), (lambda args: (deconflicted_name := deconflict_variant_name(True, env, tname, fname, env.graph), Right(hydra.ext.python.utils.cast_to(hydra.ext.python.names.type_variable_reference(env, tname), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(deconflicted_name), args))))[1])))[1])[1]))))

        case hydra.core.TermUnit():
            return Right(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.utils.py_none))

        case hydra.core.TermVariable(value=name):
            return encode_variable(cx, env, name, ())

        case hydra.core.TermWrap(value=wrapped):
            tname = wrapped.type_name
            inner = wrapped.body
            return hydra.lib.eithers.bind(encode(inner), (lambda parg: Right(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name_qualified(env, tname)), (parg,)))))

        case _:
            raise TypeError("Unsupported Term")

def encode_union_elimination_inline(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, cs: hydra.core.CaseStatement, py_arg: hydra.ext.python.syntax.Expression) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Expression]:
    r"""Encode a union elimination as an inline conditional chain (isinstance-based ternary)."""

    tname = cs.type_name
    mdefault = cs.default
    cases_ = cs.cases
    return hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), value_expr := hydra.ext.python.utils.project_from_expression(py_arg, hydra.ext.python.syntax.Name("value")), isinstance_primary := hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("isinstance")), hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(unsupported_expression("no matching case in inline union elimination"))), (lambda dflt: encode_term_inline(cx, env, False, dflt)), mdefault), (lambda py_default: (encode_branch := (lambda field: (fname := field.name, fterm := field.term, is_unit_variant := is_variant_unit_type(rt, fname), py_variant_name := deconflict_variant_name(True, env, tname, fname, env.graph), isinstance_check := hydra.lib.logic.if_else(is_enum, (lambda : cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.utils.py_expression_to_bitwise_or(py_arg), (hydra.ext.python.syntax.CompareOpBitwiseOrPair(hydra.ext.python.syntax.CompareOp.EQ, hydra.ext.python.utils.py_expression_to_bitwise_or(hydra.ext.python.utils.py_name_to_py_expression(py_variant_name))),)))),)),))))), (lambda : hydra.ext.python.utils.function_call(isinstance_primary, (py_arg, hydra.ext.python.utils.py_name_to_py_expression(py_variant_name))))), hydra.lib.eithers.bind(encode_term_inline(cx, env, False, fterm), (lambda py_branch: (py_result := hydra.lib.logic.if_else(is_enum, (lambda : hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(py_branch), (py_arg,))), (lambda : hydra.lib.logic.if_else(is_unit_variant, (lambda : hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(py_branch), (py_arg,))), (lambda : hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(py_branch), (value_expr,)))))), Right((isinstance_check, py_result)))[1])))[5]), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_branch(x1)), cases_), (lambda encoded_branches: (build_chain := (lambda else_expr, branch_pair: (check_expr := hydra.lib.pairs.first(branch_pair), result_expr := hydra.lib.pairs.second(branch_pair), cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionConditional(hydra.ext.python.syntax.Conditional(hydra.ext.python.utils.py_expression_to_disjunction(result_expr), hydra.ext.python.utils.py_expression_to_disjunction(check_expr), else_expr))))[2]), Right(hydra.lib.lists.foldl((lambda x1, x2: build_chain(x1, x2)), py_default, hydra.lib.lists.reverse(encoded_branches))))[1])))[1])))[3]))

def enum_variant_pattern(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a value pattern for an enum variant."""

    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternValue(hydra.ext.python.syntax.ValuePattern(hydra.ext.python.syntax.Attribute((hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, type_name), hydra.ext.python.names.encode_enum_value(env, field_name))))))

def variant_closed_pattern(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name, py_variant_name: hydra.ext.python.syntax.Name, row_type: T0, is_enum: bool, var_name: hydra.core.Name, should_capture: bool) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a ClosedPattern for a variant based on its characteristics."""

    return hydra.lib.logic.if_else(is_enum, (lambda : enum_variant_pattern(env, type_name, field_name)), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(should_capture), (lambda : class_variant_pattern_unit(py_variant_name)), (lambda : class_variant_pattern_with_capture(env, py_variant_name, var_name)))))

def encode_case_block(cx: T0, env: hydra.ext.python.helpers.PythonEnvironment, tname: hydra.core.Name, row_type: frozenlist[hydra.core.FieldType], is_enum: bool, encode_body: Callable[[hydra.ext.python.helpers.PythonEnvironment, hydra.core.Term], Either[T1, frozenlist[hydra.ext.python.syntax.Statement]]], field: hydra.core.Field) -> Either[frozenlist[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.CaseBlock]:
    r"""Encode a single case (Field) into a CaseBlock for a match statement."""

    fname = field.name
    fterm = field.term
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.rewriting.deannotate_and_detype_term(fterm)
    @lru_cache(1)
    def effective_lambda():
        def _hoist_effective_lambda_1(v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return lam

                case _:
                    return (synthetic_var2 := hydra.core.Name("_matchValue"), hydra.core.Lambda(synthetic_var2, Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(stripped(), cast(hydra.core.Term, hydra.core.TermVariable(synthetic_var2)))))))[1]
        match stripped():
            case hydra.core.TermFunction(value=f):
                return _hoist_effective_lambda_1(f)

            case _:
                return (synthetic_var := hydra.core.Name("_matchValue"), hydra.core.Lambda(synthetic_var, Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(stripped(), cast(hydra.core.Term, hydra.core.TermVariable(synthetic_var)))))))[1]
    v = effective_lambda().parameter
    raw_body = effective_lambda().body
    @lru_cache(1)
    def is_unit_variant() -> bool:
        return is_variant_unit_type(row_type, fname)
    @lru_cache(1)
    def effective_body() -> hydra.core.Term:
        return hydra.lib.logic.if_else(is_unit_variant(), (lambda : eliminate_unit_var(v, raw_body)), (lambda : raw_body))
    @lru_cache(1)
    def should_capture() -> bool:
        return hydra.lib.logic.not_(hydra.lib.logic.or_(is_unit_variant(), hydra.lib.logic.or_(hydra.rewriting.is_free_variable_in_term(v, raw_body), hydra.schemas.is_unit_term(raw_body))))
    @lru_cache(1)
    def env2() -> hydra.ext.python.helpers.PythonEnvironment:
        return python_environment_set_graph(hydra.schemas.extend_graph_for_lambda(python_environment_get_graph(env), effective_lambda()), env)
    @lru_cache(1)
    def py_variant_name() -> hydra.ext.python.syntax.Name:
        return deconflict_variant_name(True, env2(), tname, fname, env2().graph)
    @lru_cache(1)
    def pattern() -> hydra.ext.python.syntax.ClosedPattern:
        return variant_closed_pattern(env2(), tname, fname, py_variant_name(), row_type, is_enum, v, should_capture())
    return hydra.lib.eithers.bind(encode_body(env2(), effective_body()), (lambda stmts: (py_body := hydra.ext.python.utils.indented_block(Nothing(), (stmts,)), Right(hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.py_closed_pattern_to_py_patterns(pattern()), Nothing(), py_body)))[1]))

def encode_default_case_block(encode_term: Callable[[T0], Either[T1, hydra.ext.python.syntax.Expression]], is_full: bool, mdflt: Maybe[T0], tname: hydra.core.Name) -> Either[hydra.ext.python.syntax.Statement, frozenlist[hydra.ext.python.syntax.CaseBlock]]:
    r"""Encode the default (wildcard) case block for a match statement."""

    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(hydra.lib.logic.if_else(is_full, (lambda : hydra.ext.python.utils.raise_assertion_error("Unreachable: all variants handled")), (lambda : hydra.ext.python.utils.raise_type_error(hydra.lib.strings.cat2("Unsupported ", hydra.names.local_name_of(tname))))))), (lambda d: hydra.lib.eithers.bind(encode_term(d), (lambda pyexpr: Right(hydra.ext.python.utils.return_single(pyexpr))))), mdflt), (lambda stmt: (patterns := hydra.ext.python.utils.py_closed_pattern_to_py_patterns(cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternWildcard())), body := hydra.ext.python.utils.indented_block(Nothing(), ((stmt,),)), Right((hydra.ext.python.syntax.CaseBlock(patterns, Nothing(), body),)))[2]))

def is_cases_full(row_type: frozenlist[T0], cases_: frozenlist[T1]) -> bool:
    r"""Check if union cases are fully covered."""

    @lru_cache(1)
    def num_cases() -> int:
        return hydra.lib.lists.length(cases_)
    @lru_cache(1)
    def num_fields() -> int:
        return hydra.lib.lists.length(row_type)
    return hydra.lib.logic.not_(hydra.lib.equality.lt(num_cases(), num_fields()))

def encode_term_multiline_t_c_o(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, func_name: hydra.core.Name, param_names: frozenlist[hydra.core.Name], term: hydra.core.Term):
    r"""Encode a term body for TCO: tail self-calls become param reassignment + continue."""

    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.rewriting.deannotate_and_detype_term(term)
    @lru_cache(1)
    def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return hydra.coder_utils.gather_applications(stripped())
    @lru_cache(1)
    def gather_args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def gather_fun() -> hydra.core.Term:
        return hydra.lib.pairs.second(gathered())
    @lru_cache(1)
    def stripped_fun() -> hydra.core.Term:
        return hydra.rewriting.deannotate_and_detype_term(gather_fun())
    @lru_cache(1)
    def is_self_call() -> bool:
        match stripped_fun():
            case hydra.core.TermVariable(value=n):
                return hydra.lib.equality.equal(n, func_name)

            case _:
                return False
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(is_self_call(), hydra.lib.equality.equal(hydra.lib.lists.length(gather_args()), hydra.lib.lists.length(param_names))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda a: encode_term_inline(cx, env, False, a)), gather_args()), (lambda py_args: (assignments := hydra.lib.lists.map((lambda pair: (param_name := hydra.lib.pairs.first(pair), py_arg := hydra.lib.pairs.second(pair), hydra.ext.python.utils.assignment_statement(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, param_name), py_arg))[2]), hydra.lib.lists.zip(param_names, py_args)), continue_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementSimple((cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementContinue()),))), Right(hydra.lib.lists.concat2(assignments, (continue_stmt,))))[2]))), (lambda : (gathered2 := hydra.coder_utils.gather_applications(term), (args2 := hydra.lib.pairs.first(gathered2), (body2 := hydra.lib.pairs.second(gathered2), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args2), 1), (lambda : (arg := hydra.lib.lists.head(args2), (_hoist_body_1 := (lambda v1: (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), hydra.lib.eithers.bind(encode_term_inline(cx, env, False, arg), (lambda py_arg: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: encode_case_block(cx, env, tname, rt, is_enum, (lambda e2, t2: encode_term_multiline_t_c_o(cx, e2, func_name, param_names, t2)), v12)), deduplicate_case_variables(cases_)), (lambda py_cases: hydra.lib.eithers.bind(encode_default_case_block((lambda t2: encode_term_inline(cx, env, False, t2)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(py_arg)))), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.concat2(py_cases, py_dflt)))))), Right((match_stmt,)))[2])))))))[2])))[3])(v1.value) if isinstance(v1, hydra.core.EliminationUnion) else hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda expr: Right((hydra.ext.python.utils.return_single(expr),))))), _hoist_body_2 := (lambda v1: (lambda e: _hoist_body_1(e))(v1.value) if isinstance(v1, hydra.core.FunctionElimination) else hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda expr: Right((hydra.ext.python.utils.return_single(expr),))))), _hoist_body_3 := (lambda v1: (lambda f: _hoist_body_2(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda expr: Right((hydra.ext.python.utils.return_single(expr),))))), _hoist_body_3(hydra.rewriting.deannotate_and_detype_term(body2)))[3])[1]), (lambda : hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda expr: Right((hydra.ext.python.utils.return_single(expr),)))))))[1])[1])[1]))

@lru_cache(1)
def lru_cache_decorator() -> hydra.ext.python.syntax.NamedExpression:
    r"""Decorator for @lru_cache(1) to memoize zero-argument function results."""

    return cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("lru_cache"))))), (py_int(1),))))

def use_inline_type_params_for(version: hydra.ext.python.helpers.PythonVersion) -> bool:
    r"""Version-aware inline type parameters."""

    return hydra.lib.equality.equal(version, hydra.ext.python.helpers.PythonVersion.PYTHON312)

@lru_cache(1)
def use_inline_type_params() -> bool:
    r"""Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code."""

    return use_inline_type_params_for(hydra.ext.python.utils.target_python_version)

def extend_env_with_lambda_params(env: hydra.ext.python.helpers.PythonEnvironment, term: hydra.core.Term) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Extend environment with lambda parameters from a term."""

    def go(e: hydra.ext.python.helpers.PythonEnvironment, t: hydra.core.Term):
        def _hoist_go_1(e, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    @lru_cache(1)
                    def new_tc() -> hydra.graph.Graph:
                        return hydra.schemas.extend_graph_for_lambda(python_environment_get_graph(e), lam)
                    @lru_cache(1)
                    def new_env() -> hydra.ext.python.helpers.PythonEnvironment:
                        return python_environment_set_graph(new_tc(), e)
                    return go(new_env(), lam.body)

                case _:
                    return e
        match hydra.rewriting.deannotate_and_detype_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_go_1(e, f)

            case _:
                return e
    return go(env, term)

def extract_case_elimination(term: hydra.core.Term):
    def _hoist_hydra_ext_python_coder_extract_case_elimination_1(v1):
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                return Just(cs)

            case _:
                return Nothing()
    def _hoist_hydra_ext_python_coder_extract_case_elimination_2(v1):
        match v1:
            case hydra.core.FunctionElimination(value=e):
                return _hoist_hydra_ext_python_coder_extract_case_elimination_1(e)

            case _:
                return Nothing()
    match hydra.rewriting.deannotate_and_detype_term(term):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_ext_python_coder_extract_case_elimination_2(f)

        case _:
            return Nothing()

def gather_lambdas(term: hydra.core.Term) -> tuple[frozenlist[hydra.core.Name], hydra.core.Term]:
    r"""Extract lambdas and their bodies from a term."""

    def go(params: frozenlist[hydra.core.Name], t: hydra.core.Term):
        def _hoist_go_1(params, t, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return go(hydra.lib.lists.concat2(params, (l.parameter,)), l.body)

                case _:
                    return (params, t)
        match hydra.rewriting.deannotate_and_detype_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_go_1(params, t, f)

            case _:
                return (params, t)
    return go((), term)

def is_case_statement_application(term: hydra.core.Term):
    r"""Check if a term is a case statement applied to exactly one argument."""

    @lru_cache(1)
    def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return hydra.coder_utils.gather_applications(term)
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.pairs.second(gathered())
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(args()), 1)), (lambda : Nothing()), (lambda : (arg := hydra.lib.lists.head(args()), (_hoist_body_1 := (lambda v1: (lambda cs: Just((cs.type_name, (cs.default, (cs.cases, arg)))))(v1.value) if isinstance(v1, hydra.core.EliminationUnion) else Nothing()), _hoist_body_2 := (lambda v1: (lambda e: _hoist_body_1(e))(v1.value) if isinstance(v1, hydra.core.FunctionElimination) else Nothing()), _hoist_body_3 := (lambda v1: (lambda f: _hoist_body_2(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else Nothing()), _hoist_body_3(hydra.rewriting.deannotate_and_detype_term(body())))[3])[1]))

def encode_binding_as(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, binding: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a binding as a Python statement (function definition or assignment)."""

    name1 = binding.name
    term1 = binding.term
    mts = binding.type
    @lru_cache(1)
    def fname() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name1)
    return hydra.lib.maybes.maybe((lambda : (gathered := gather_lambdas(term1), (lambda_params := hydra.lib.pairs.first(gathered), (inner_body := hydra.lib.pairs.second(gathered), (mcsa := is_case_statement_application(inner_body), hydra.lib.maybes.maybe((lambda : (mcs := extract_case_elimination(term1), hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.map((lambda stmts: hydra.lib.lists.head(stmts)), encode_term_multiline(cx, env, term1))), (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), inner_param := hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("x"), Nothing()), param := hydra.ext.python.syntax.ParamNoDefault(inner_param, Nothing()), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((param,), (), Nothing()))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_case_block(cx, env, tname, rt, is_enum, (lambda e, t: encode_term_multiline(cx, e, t)), v1)), cases_), (lambda py_cases: hydra.lib.eithers.bind(encode_default_case_block((lambda t: encode_term_inline(cx, env, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.syntax.Name("x")))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), Right(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[5])))[3]), mcs))[1]), (lambda csa: hydra.lib.logic.if_else(hydra.lib.lists.null(lambda_params), (lambda : (mcs := extract_case_elimination(term1), hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.map((lambda stmts: hydra.lib.lists.head(stmts)), encode_term_multiline(cx, env, term1))), (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), inner_param := hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("x"), Nothing()), param := hydra.ext.python.syntax.ParamNoDefault(inner_param, Nothing()), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((param,), (), Nothing()))), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_case_block(cx, env, tname, rt, is_enum, (lambda e, t: encode_term_multiline(cx, e, t)), v1)), cases_), (lambda py_cases: hydra.lib.eithers.bind(encode_default_case_block((lambda t: encode_term_inline(cx, env, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.syntax.Name("x")))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), Right(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[5])))[3]), mcs))[1]), (lambda : (tname := hydra.lib.pairs.first(csa), (rest1 := hydra.lib.pairs.second(csa), (dflt := hydra.lib.pairs.first(rest1), (rest2 := hydra.lib.pairs.second(rest1), (cases_ := hydra.lib.pairs.first(rest2), hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), captured_var_names := hydra.lib.lists.init(lambda_params), match_lambda_param := hydra.lib.lists.last(lambda_params), captured_params := hydra.lib.lists.map((lambda n: hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, n), Nothing()), Nothing())), captured_var_names), match_arg_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, match_lambda_param), match_param := hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(match_arg_name, Nothing()), Nothing()), all_params := hydra.lib.lists.concat2(captured_params, (match_param,)), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(all_params, (), Nothing()))), env_with_params := extend_env_with_lambda_params(env, term1), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_case_block(cx, env_with_params, tname, rt, is_enum, (lambda e, t: encode_term_multiline(cx, e, t)), v1)), cases_), (lambda py_cases: hydra.lib.eithers.bind(encode_default_case_block((lambda t: encode_term_inline(cx, env_with_params, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(match_arg_name))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), Right(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[10])))[1])[1])[1])[1])[1]))), mcsa))[1])[1])[1])[1]), (lambda ts: hydra.lib.eithers.bind(hydra.annotations.get_term_description(cx, python_environment_get_graph(env), term1), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), encode_term_assignment(cx, env, name1, term1, ts, norm_comment))[1]))), mts)

def encode_function_definition(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], body: hydra.core.Term, doms: frozenlist[hydra.core.Type], mcod: Maybe[hydra.core.Type], comment: Maybe[str], prefixes: frozenlist[hydra.ext.python.syntax.Statement]) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a function definition with parameters and body."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pair: (arg_name := hydra.lib.pairs.first(pair), typ := hydra.lib.pairs.second(pair), hydra.lib.eithers.bind(encode_type(env, typ), (lambda py_typ: Right(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, arg_name), Just(hydra.ext.python.syntax.Annotation(py_typ))), Nothing())))))[2]), hydra.lib.lists.zip(args, doms)), (lambda py_args: (py_params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(py_args, (), Nothing()))), is_t_c_o := hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.lists.null(args)), hydra.coder_utils.is_self_tail_recursive(name, body)), hydra.lib.eithers.bind(hydra.lib.logic.if_else(is_t_c_o, (lambda : hydra.lib.eithers.bind(encode_term_multiline_t_c_o(cx, env, name, args, body), (lambda tco_stmts: (true_expr := cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTrue())))), while_body := hydra.ext.python.utils.indented_block(Nothing(), (hydra.lib.lists.concat2(prefixes, tco_stmts),)), while_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementWhile(hydra.ext.python.syntax.WhileStatement(true_expr, while_body, Nothing()))))), Right(hydra.ext.python.utils.indented_block(comment, ((while_stmt,),))))[3]))), (lambda : hydra.lib.eithers.bind(encode_term_multiline(cx, env, body), (lambda stmts: Right(hydra.ext.python.utils.indented_block(comment, (hydra.lib.lists.concat2(prefixes, stmts),))))))), (lambda block: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda cod: hydra.lib.eithers.bind(encode_type(env, cod), (lambda pytyp: Right(Just(pytyp))))), mcod), (lambda mreturn_type: (py_tparams := hydra.lib.logic.if_else(use_inline_type_params(), (lambda : hydra.lib.lists.map((lambda arg_: hydra.ext.python.utils.py_name_to_py_type_parameter(hydra.ext.python.names.encode_type_variable(arg_))), tparams)), (lambda : ())), is_thunk := hydra.lib.lists.null(args), m_decorators := hydra.lib.logic.if_else(is_thunk, (lambda : Just(hydra.ext.python.syntax.Decorators((lru_cache_decorator(),)))), (lambda : Nothing())), py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, name), Right(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(m_decorators, hydra.ext.python.syntax.FunctionDefRaw(False, py_name, py_tparams, Just(py_params), mreturn_type, Nothing(), block))))))))[4])))))[2]))

def encode_term_assignment(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, term: hydra.core.Term, ts: hydra.core.TypeScheme, comment: Maybe[str]) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a term assignment to a Python statement."""

    return hydra.lib.eithers.bind(analyze_python_function(cx, env, term), (lambda fs: (tparams := fs.type_params, params := fs.params, bindings := fs.bindings, body := fs.body, doms := fs.domains, mcod := fs.codomain, env2 := fs.environment, tc := env2.graph, binding := hydra.core.Binding(name, term, Just(ts)), is_complex := hydra.coder_utils.is_complex_binding(tc, binding), is_trivial := hydra.coder_utils.is_trivial_term(term), hydra.lib.logic.if_else(hydra.lib.logic.and_(is_complex, hydra.lib.logic.not_(is_trivial)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_binding_as(cx, env2, v1)), bindings), (lambda binding_stmts: encode_function_definition(cx, env2, name, tparams, params, body, doms, mcod, comment, binding_stmts)))), (lambda : hydra.lib.eithers.bind(encode_term_inline(cx, env2, False, body), (lambda body_expr: (py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env2, name), Right(hydra.ext.python.utils.annotated_statement(comment, hydra.ext.python.utils.assignment_statement(py_name, body_expr))))[1])))))[11]))

def encode_term_multiline(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, term: hydra.core.Term):
    r"""Encode a term to a list of statements with return as final statement."""

    @lru_cache(1)
    def dflt_logic() -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.ext.python.syntax.Statement]]:
        return hydra.lib.eithers.bind(analyze_python_function(cx, env, term), (lambda fs: (params := fs.params, bindings := fs.bindings, inner_body := fs.body, env2 := fs.environment, hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : hydra.lib.eithers.bind(encode_term_inline(cx, env, False, term), (lambda expr: Right((hydra.ext.python.utils.return_single(expr),))))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_binding_as(cx, env2, v1)), bindings), (lambda binding_stmts: hydra.lib.eithers.bind(encode_term_multiline(cx, env2, inner_body), (lambda body_stmts: Right(hydra.lib.lists.concat2(binding_stmts, body_stmts)))))))))[4]))
    @lru_cache(1)
    def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return hydra.coder_utils.gather_applications(term)
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.pairs.second(gathered())
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args()), 1), (lambda : (arg := hydra.lib.lists.head(args()), (_hoist_body_1 := (lambda v1: (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.eithers.bind(hydra.schemas.require_union_type(cx, python_environment_get_graph(env), tname), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), hydra.lib.eithers.bind(encode_term_inline(cx, env, False, arg), (lambda py_arg: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v12: encode_case_block(cx, env, tname, rt, is_enum, (lambda e2, t: encode_term_multiline(cx, e2, t)), v12)), deduplicate_case_variables(cases_)), (lambda py_cases: hydra.lib.eithers.bind(encode_default_case_block((lambda t: encode_term_inline(cx, env, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(py_arg)))), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.concat2(py_cases, py_dflt)))))), Right((match_stmt,)))[2])))))))[2])))[3])(v1.value) if isinstance(v1, hydra.core.EliminationUnion) else dflt_logic()), _hoist_body_2 := (lambda v1: (lambda e: _hoist_body_1(e))(v1.value) if isinstance(v1, hydra.core.FunctionElimination) else dflt_logic()), _hoist_body_3 := (lambda v1: (lambda f: _hoist_body_2(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else dflt_logic()), _hoist_body_3(hydra.rewriting.deannotate_and_detype_term(body())))[3])[1]), (lambda : dflt_logic()))

def encode_bindings_as_defs(env: T0, encode_binding: Callable[[T0, T1], Either[T2, T3]], bindings: frozenlist[T1]) -> Either[T2, frozenlist[T3]]:
    r"""Encode bindings as function definitions."""

    return hydra.lib.eithers.map_list((lambda v1: encode_binding(env, v1)), bindings)

def encode_field_type(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, field_type: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a field type for record definitions (field: type annotation)."""

    fname = field_type.name
    ftype = field_type.type
    return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, python_environment_get_graph(env), ftype), (lambda comment: (py_name := cast(hydra.ext.python.syntax.SingleTarget, hydra.ext.python.syntax.SingleTargetName(hydra.ext.python.names.encode_field_name(env, fname))), hydra.lib.eithers.bind(encode_type(env, ftype), (lambda py_type: (annotated_py_type := hydra.ext.python.utils.annotated_expression(comment, py_type), Right(hydra.ext.python.utils.py_assignment_to_py_statement(cast(hydra.ext.python.syntax.Assignment, hydra.ext.python.syntax.AssignmentTyped(hydra.ext.python.syntax.TypedAssignment(py_name, annotated_py_type, Nothing()))))))[1])))[1]))

def encode_name_constants(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Generate name constants for a type as class-level attributes."""

    def to_stmt(pair: tuple[hydra.ext.python.syntax.Name, hydra.core.Name]) -> hydra.ext.python.syntax.Statement:
        return hydra.ext.python.utils.assignment_statement(hydra.lib.pairs.first(pair), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name("hydra.core.Name"))), (hydra.ext.python.utils.double_quoted_string(hydra.lib.pairs.second(pair).value),)))
    @lru_cache(1)
    def name_pair() -> tuple[hydra.ext.python.syntax.Name, hydra.core.Name]:
        return (hydra.ext.python.names.encode_constant_for_type_name(env, name), name)
    @lru_cache(1)
    def field_pairs() -> frozenlist[tuple[hydra.ext.python.syntax.Name, hydra.core.Name]]:
        return hydra.lib.lists.map((lambda field: (hydra.ext.python.names.encode_constant_for_field_name(env, name, field.name), field.name)), fields)
    return hydra.lib.lists.map((lambda x1: to_stmt(x1)), hydra.lib.lists.cons(name_pair(), field_pairs()))

def generic_arg(tparam_list: frozenlist[hydra.core.Name]) -> Maybe[hydra.ext.python.syntax.Expression]:
    r"""Create Generic[...] argument expression for class definition."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(tparam_list), (lambda : Nothing()), (lambda : Just(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Generic"))))), hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.names.encode_type_variable(n)))))), Nothing()))))))))), ()))),)),))))), tparam_list))))))

def encode_record_type(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, row_type: frozenlist[hydra.core.FieldType], comment: Maybe[str]) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a record type as a Python dataclass."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_field_type(cx, env, v1)), row_type), (lambda py_fields: (const_stmts := encode_name_constants(env, name, row_type), body := hydra.ext.python.utils.indented_block(comment, (py_fields, const_stmts)), bound_vars := env.bound_type_variables, tparam_list := hydra.lib.pairs.first(bound_vars), m_generic_arg := generic_arg(tparam_list), args := hydra.lib.maybes.maybe((lambda : Nothing()), (lambda a: Just(hydra.ext.python.utils.py_expressions_to_py_args((a,)))), m_generic_arg), decs := Just(hydra.ext.python.syntax.Decorators((dataclass_decorator(),))), py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), no_type_params := (), Right(hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(decs, py_name, no_type_params, args, body))))[9]))

def environment_type_parameters(env: hydra.ext.python.helpers.PythonEnvironment) -> frozenlist[hydra.ext.python.syntax.TypeParameter]:
    r"""Get type parameters from environment as Python TypeParameters."""

    return hydra.lib.lists.map((lambda arg_: hydra.ext.python.utils.py_name_to_py_type_parameter(hydra.ext.python.names.encode_type_variable(arg_))), hydra.lib.pairs.first(env.bound_type_variables))

def type_alias_statement_for(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.ext.python.syntax.Name, tparams: frozenlist[hydra.ext.python.syntax.TypeParameter], mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Statement:
    r"""Version-aware type alias statement generation."""

    return hydra.lib.logic.if_else(use_inline_type_params_for(env.version), (lambda : hydra.ext.python.utils.type_alias_statement(name, tparams, mcomment, tyexpr)), (lambda : hydra.ext.python.utils.type_alias_statement310(name, tparams, mcomment, tyexpr)))

def encode_type_def_single(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, comment: Maybe[str], type_expr: hydra.ext.python.syntax.Expression) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Encode a simple type alias definition."""

    @lru_cache(1)
    def py_name() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name)
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.python.syntax.TypeParameter]:
        return environment_type_parameters(env)
    return (type_alias_statement_for(env, py_name(), tparams(), comment, type_expr),)

def encode_enum_value_assignment(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, field_type: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode an enum value assignment statement with optional comment."""

    fname = field_type.name
    ftype = field_type.type
    return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, python_environment_get_graph(env), ftype), (lambda mcomment: (py_name := hydra.ext.python.names.encode_enum_value(env, fname), fname_str := fname.value, py_value := hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name("hydra.core.Name"))), (hydra.ext.python.utils.double_quoted_string(fname_str),)), assign_stmt := hydra.ext.python.utils.assignment_statement(py_name, py_value), Right(hydra.lib.maybes.maybe((lambda : (assign_stmt,)), (lambda c: (assign_stmt, hydra.ext.python.utils.py_expression_to_py_statement(hydra.ext.python.utils.triple_quoted_string(c)))), mcomment)))[4]))

def encode_type_quoted(env: hydra.ext.python.helpers.PythonEnvironment, typ: hydra.core.Type) -> Either[T0, hydra.ext.python.syntax.Expression]:
    r"""Encode a type to a Python expression, quoting if the type has free variables."""

    return hydra.lib.eithers.bind(encode_type(env, typ), (lambda pytype: Right(hydra.lib.logic.if_else(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ)), (lambda : pytype), (lambda : hydra.ext.python.utils.double_quoted_string(hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(pytype))))))))

def find_type_params(env: hydra.ext.python.helpers.PythonEnvironment, typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Find type parameters in a type that are bound in the environment."""

    @lru_cache(1)
    def bound_vars() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.pairs.second(env.bound_type_variables)
    def is_bound(v: hydra.core.Name) -> bool:
        return hydra.lib.maybes.is_just(hydra.lib.maps.lookup(v, bound_vars()))
    return hydra.lib.lists.filter((lambda x1: is_bound(x1)), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(typ)))

def variant_args(ptype: hydra.ext.python.syntax.Expression, tparams: frozenlist[hydra.core.Name]) -> hydra.ext.python.syntax.Args:
    r"""Create args for variant (Node[type], Generic[tparams])."""

    return hydra.ext.python.utils.py_expressions_to_py_args(hydra.lib.maybes.cat((Just(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Node"))))), (ptype,)))), generic_arg(tparams))))

def encode_union_field(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, union_name: hydra.core.Name, field_type: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Statement]:
    r"""Encode a union field as a variant class."""

    fname = field_type.name
    ftype = field_type.type
    return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, python_environment_get_graph(env), ftype), (lambda fcomment: (is_unit := hydra.lib.equality.equal(hydra.rewriting.deannotate_type(ftype), cast(hydra.core.Type, hydra.core.TypeUnit())), var_name := deconflict_variant_name(False, env, union_name, fname, env.graph), tparam_names := find_type_params(env, ftype), tparam_py_names := hydra.lib.lists.map((lambda x1: hydra.ext.python.names.encode_type_variable(x1)), tparam_names), field_params := hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_name_to_py_type_parameter(x1)), tparam_py_names), body := hydra.lib.logic.if_else(is_unit, (lambda : hydra.ext.python.utils.indented_block(fcomment, (hydra.ext.python.utils.unit_variant_methods(var_name),))), (lambda : hydra.ext.python.utils.indented_block(fcomment, ()))), hydra.lib.eithers.bind(hydra.lib.logic.if_else(is_unit, (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(encode_type_quoted(env, ftype), (lambda quoted_type: Right(Just(variant_args(quoted_type, ()))))))), (lambda margs: Right(hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), var_name, field_params, margs, body))))))[6]))

def encode_union_field_alt(env: hydra.ext.python.helpers.PythonEnvironment, union_name: hydra.core.Name, field_type: hydra.core.FieldType) -> hydra.ext.python.syntax.Primary:
    r"""Encode a union field as a primary expression for | alternatives."""

    fname = field_type.name
    ftype = field_type.type
    @lru_cache(1)
    def tparam_names() -> frozenlist[hydra.core.Name]:
        return find_type_params(env, ftype)
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.python.syntax.Name]:
        return hydra.lib.lists.map((lambda x1: hydra.ext.python.names.encode_type_variable(x1)), tparam_names())
    @lru_cache(1)
    def name_prim() -> hydra.ext.python.syntax.Primary:
        return hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.variant_name(False, env, union_name, fname))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : name_prim()), (lambda : (tparam_exprs := hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_name_to_py_expression(x1)), tparams()), hydra.ext.python.utils.primary_with_expression_slices(name_prim(), tparam_exprs))[1]))

def union_type_statements_for(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.ext.python.syntax.Name, tparams: frozenlist[hydra.ext.python.syntax.TypeParameter], mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression, extra_stmts: frozenlist[hydra.ext.python.syntax.Statement]) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Version-aware union type statement generation."""

    return hydra.lib.logic.if_else(use_inline_type_params_for(env.version), (lambda : hydra.lib.lists.concat2((hydra.ext.python.utils.type_alias_statement(name, tparams, mcomment, tyexpr),), extra_stmts)), (lambda : hydra.ext.python.utils.union_type_class_statements310(name, mcomment, tyexpr, extra_stmts)))

def encode_union_type(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, row_type: frozenlist[hydra.core.FieldType], comment: Maybe[str]) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode a union type as an enum (for unit-only fields) or variant classes."""

    return hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(row_type), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_enum_value_assignment(cx, env, v1)), row_type), (lambda vals: (body := hydra.ext.python.utils.indented_block(comment, vals), enum_name := hydra.ext.python.syntax.Name("Enum"), args := Just(hydra.ext.python.utils.py_expressions_to_py_args((hydra.ext.python.utils.py_name_to_py_expression(enum_name),))), py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), type_const_stmt := hydra.ext.python.utils.dotted_assignment_statement(py_name, hydra.ext.python.names.encode_constant_for_type_name(env, name), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name("hydra.core.Name"))), (hydra.ext.python.utils.double_quoted_string(name.value),))), Right((hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), py_name, (), args, body)), type_const_stmt)))[5]))), (lambda : (const_stmts := encode_name_constants(env, name, row_type), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_union_field(cx, env, name, v1)), row_type), (lambda field_stmts: (tparams := environment_type_parameters(env), union_alts := hydra.lib.lists.map((lambda v1: encode_union_field_alt(env, name, v1)), row_type), union_stmts := union_type_statements_for(env, hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), tparams, comment, hydra.ext.python.utils.or_expression(union_alts), const_stmts), Right(hydra.lib.lists.concat2(field_stmts, union_stmts)))[3])))[1]))

def encode_wrapped_type(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> Either[T0, frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode a wrapped type (newtype) to a Python class definition."""

    @lru_cache(1)
    def tparam_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(env.bound_type_variables)
    return hydra.lib.eithers.bind(encode_type_quoted(env, typ), (lambda ptype_quoted: (py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), body := hydra.ext.python.utils.indented_block(comment, ()), type_const_stmt := hydra.ext.python.utils.dotted_assignment_statement(py_name, hydra.ext.python.names.encode_constant_for_type_name(env, name), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name("hydra.core.Name"))), (hydra.ext.python.utils.double_quoted_string(name.value),))), Right((hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), py_name, hydra.lib.lists.map((lambda arg_: hydra.ext.python.utils.py_name_to_py_type_parameter(hydra.ext.python.names.encode_type_variable(arg_))), find_type_params(env, typ)), Just(variant_args(ptype_quoted, tparam_list())), body)), type_const_stmt)))[3]))

def extend_env_with_type_var(env: hydra.ext.python.helpers.PythonEnvironment, var_: hydra.core.Name) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Extend a PythonEnvironment with a new bound type variable."""

    old_bound = env.bound_type_variables
    @lru_cache(1)
    def tparam_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(old_bound)
    @lru_cache(1)
    def tparam_map() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.pairs.second(old_bound)
    @lru_cache(1)
    def new_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.concat2(tparam_list(), (var_,))
    @lru_cache(1)
    def new_map() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.maps.insert(var_, hydra.ext.python.names.encode_type_variable(var_), tparam_map())
    return hydra.ext.python.helpers.PythonEnvironment(env.namespaces, (new_list(), new_map()), env.graph, env.nullary_bindings, env.version, env.skip_casts, env.inline_variables)

def encode_type_assignment_inner(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode the inner type definition, unwrapping forall types."""

    while True:
        @lru_cache(1)
        def stripped() -> hydra.core.Type:
            return hydra.rewriting.deannotate_type(typ)
        @lru_cache(1)
        def dflt() -> Either[T0, frozenlist[hydra.ext.python.syntax.Statement]]:
            return hydra.lib.eithers.bind(encode_type(env, typ), (lambda type_expr: Right(encode_type_def_single(env, name, comment, type_expr))))
        match stripped():
            case hydra.core.TypeForall(value=ft):
                return (tvar := ft.parameter, (body := ft.body, (new_env := extend_env_with_type_var(env, tvar), encode_type_assignment_inner(cx, new_env, name, body, comment))[1])[1])[1]

            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.eithers.map((lambda s: (s,)), encode_record_type(cx, env, name, rt, comment))

            case hydra.core.TypeUnion(value=rt2):
                return encode_union_type(cx, env, name, rt2, comment)

            case hydra.core.TypeWrap(value=wt):
                return encode_wrapped_type(env, name, wt, comment)

            case _:
                return dflt()

def encode_type_assignment(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]]:
    r"""Encode a type definition, dispatching based on type structure."""

    return hydra.lib.eithers.bind(encode_type_assignment_inner(cx, env, name, typ, comment), (lambda def_stmts: Right(hydra.lib.lists.map((lambda s: (s,)), def_stmts))))

def encode_definition(cx: hydra.context.Context, env: hydra.ext.python.helpers.PythonEnvironment, def_: hydra.module.Definition) -> Either[hydra.context.InContext[hydra.error.Error], frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]]:
    r"""Encode a definition (term or type) to Python statements."""

    match def_:
        case hydra.module.DefinitionTerm(value=td):
            name = td.name
            term = td.term
            typ = td.type
            return hydra.lib.eithers.bind(hydra.annotations.get_term_description(cx, python_environment_get_graph(env), term), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), hydra.lib.eithers.bind(encode_term_assignment(cx, env, name, term, typ, norm_comment), (lambda stmt: Right(((stmt,),)))))[1]))

        case hydra.module.DefinitionType(value=td2):
            name = td2.name
            typ = td2.type
            return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, python_environment_get_graph(env), typ), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), encode_type_assignment(cx, env, name, typ, norm_comment))[1]))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_field(cx: T0, env: hydra.ext.python.helpers.PythonEnvironment, field: hydra.core.Field, encode_term: Callable[[hydra.core.Term], Either[T1, T2]]) -> Either[tuple[hydra.ext.python.syntax.Name, T2], tuple[hydra.ext.python.syntax.Name, T2]]:
    r"""Encode a field (name-value pair) to a Python (Name, Expression) pair."""

    fname = field.name
    fterm = field.term
    return hydra.lib.eithers.bind(encode_term(fterm), (lambda pterm: Right((hydra.ext.python.names.encode_field_name(env, fname), pterm))))

def set_meta_type_variables(m: hydra.ext.python.helpers.PythonModuleMetadata, tvars: frozenset[hydra.core.Name]) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, tvars, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_annotated(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, b, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_callable(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, b, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_dataclass(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, b, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_decimal(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, b, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_either(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, b, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_enum(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, b, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_frozen_dict(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, b, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_frozen_list(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, b, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_generic(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, b, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_maybe(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, b, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def extend_meta_for_type(top_level: bool, is_term_annot: bool, typ: hydra.core.Type, meta: hydra.ext.python.helpers.PythonModuleMetadata):
    r"""Extend metadata based on a type (used during module encoding)."""

    current_tvars = meta.type_variables
    @lru_cache(1)
    def new_tvars() -> frozenset[hydra.core.Name]:
        return collect_type_variables(current_tvars, typ)
    @lru_cache(1)
    def meta_with_tvars() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return set_meta_type_variables(meta, new_tvars())
    @lru_cache(1)
    def meta_with_subtypes() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return hydra.lib.lists.foldl((lambda m, t: extend_meta_for_type(False, is_term_annot, t, m)), meta_with_tvars(), hydra.rewriting.subtypes(typ))
    def _hoist_body_1(v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return set_meta_uses_decimal(meta_with_subtypes(), True)

            case _:
                return meta_with_subtypes()
    def _hoist_body_2(v1):
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_body_1(ft)

            case _:
                return meta_with_subtypes()
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeFunction(value=ft):
            cod = ft.codomain
            dom = ft.domain
            @lru_cache(1)
            def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return extend_meta_for_type(top_level, is_term_annot, cod, meta_with_subtypes())
            @lru_cache(1)
            def meta3() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return extend_meta_for_type(False, is_term_annot, dom, meta2())
            return hydra.lib.logic.if_else(hydra.lib.logic.and_(is_term_annot, top_level), (lambda : meta3()), (lambda : set_meta_uses_callable(meta3(), True)))

        case hydra.core.TypeList():
            return set_meta_uses_frozen_list(meta_with_subtypes(), True)

        case hydra.core.TypeMap():
            return set_meta_uses_frozen_dict(meta_with_subtypes(), True)

        case hydra.core.TypeMaybe():
            return set_meta_uses_maybe(meta_with_subtypes(), True)

        case hydra.core.TypeEither():
            return set_meta_uses_either(meta_with_subtypes(), True)

        case hydra.core.TypeLiteral(value=lt):
            return _hoist_body_2(lt)

        case hydra.core.TypeUnion(value=rt):
            return hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(rt), (lambda : set_meta_uses_enum(meta_with_subtypes(), True)), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(rt)), (lambda : set_meta_uses_node(meta_with_subtypes(), True)), (lambda : meta_with_subtypes()))))

        case hydra.core.TypeForall(value=ft2):
            body = ft2.body
            @lru_cache(1)
            def meta_for_wrap() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return dig_for_wrap(is_term_annot, meta_with_subtypes(), body)
            def _hoist_body_1(v1):
                match v1:
                    case hydra.core.TypeRecord():
                        return set_meta_uses_generic(meta_for_wrap(), True)

                    case _:
                        return meta_for_wrap()
            return _hoist_body_1(hydra.rewriting.deannotate_type(body))

        case hydra.core.TypeRecord(value=rt2):
            @lru_cache(1)
            def has_annotated() -> bool:
                return hydra.lib.lists.foldl((lambda b, ft: hydra.lib.logic.or_(b, hydra.annotations.has_type_description(ft.type))), False, rt2)
            @lru_cache(1)
            def meta1() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(rt2), (lambda : meta_with_subtypes()), (lambda : set_meta_uses_dataclass(meta_with_subtypes(), True)))
            return hydra.lib.logic.if_else(has_annotated(), (lambda : set_meta_uses_annotated(meta1(), True)), (lambda : meta1()))

        case hydra.core.TypeWrap():
            return hydra.lib.logic.if_else(is_term_annot, (lambda : meta_with_subtypes()), (lambda : set_meta_uses_node(meta_with_subtypes(), True)))

        case _:
            return meta_with_subtypes()

def set_meta_uses_cast(b: bool, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, b, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_just(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, b, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_left(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, b, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_nothing(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, b, m.uses_right, m.uses_type_var)

def set_meta_uses_right(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, b, m.uses_type_var)

def extend_meta_for_term(top_level: bool, meta0: hydra.ext.python.helpers.PythonModuleMetadata, term: hydra.core.Term) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Extend metadata based on a term (used during module encoding)."""

    def step(meta: hydra.ext.python.helpers.PythonModuleMetadata, t: hydra.core.Term):
        def _hoist_step_1(meta, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.maybes.maybe((lambda : meta), (lambda dom: hydra.lib.logic.if_else(top_level, (lambda : extend_meta_for_type(True, False, dom, meta)), (lambda : meta))), lam.domain)

                case _:
                    return meta
        def _hoist_step_2(meta, v1):
            match v1:
                case hydra.core.FloatValueBigfloat():
                    return set_meta_uses_decimal(meta, True)

                case _:
                    return meta
        def _hoist_step_3(meta, v1):
            match v1:
                case hydra.core.LiteralFloat(value=fv):
                    return _hoist_step_2(meta, fv)

                case _:
                    return meta
        match t:
            case hydra.core.TermEither(value=e):
                @lru_cache(1)
                def meta_with_cast() -> hydra.ext.python.helpers.PythonModuleMetadata:
                    return set_meta_uses_cast(True, meta)
                return hydra.lib.eithers.either((lambda _: set_meta_uses_left(meta_with_cast(), True)), (lambda _: set_meta_uses_right(meta_with_cast(), True)), e)

            case hydra.core.TermFunction(value=f):
                return _hoist_step_1(meta, f)

            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                return hydra.lib.lists.foldl((for_binding := (lambda m, b: hydra.lib.maybes.maybe((lambda : m), (lambda ts: (term1 := b.term, hydra.lib.logic.if_else(hydra.coder_utils.is_simple_assignment(term1), (lambda : m), (lambda : extend_meta_for_type(True, True, ts.type, m))))[1]), b.type)), (lambda x1, x2: for_binding(x1, x2)))[1], meta, bindings)

            case hydra.core.TermLiteral(value=l):
                return _hoist_step_3(meta, l)

            case hydra.core.TermMap():
                return set_meta_uses_frozen_dict(meta, True)

            case hydra.core.TermMaybe(value=m):
                return hydra.lib.maybes.maybe((lambda : set_meta_uses_nothing(meta, True)), (lambda _: set_meta_uses_just(meta, True)), m)

            case hydra.core.TermUnion():
                return set_meta_uses_cast(True, meta)

            case _:
                return meta
    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: step(x1, x2)), meta0, term)

def set_meta_uses_lru_cache(b: bool, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, b, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_name(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, b, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def set_meta_uses_type_var(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, b)

def gather_metadata(focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Gather metadata from definitions."""

    @lru_cache(1)
    def start() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return empty_metadata(hydra.ext.python.utils.find_namespaces(focus_ns, defs))
    def add_def(meta: hydra.ext.python.helpers.PythonModuleMetadata, def_: hydra.module.Definition) -> hydra.ext.python.helpers.PythonModuleMetadata:
        match def_:
            case hydra.module.DefinitionTerm(value=term_def):
                term = term_def.term
                typ_scheme = term_def.type
                typ = typ_scheme.type
                @lru_cache(1)
                def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                    return extend_meta_for_type(True, True, typ, meta)
                return extend_meta_for_term(True, meta2(), term)

            case hydra.module.DefinitionType(value=type_def):
                typ = type_def.type
                @lru_cache(1)
                def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                    return set_meta_uses_name(meta, True)
                return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: extend_meta_for_type(True, False, t, m)), meta2(), typ)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def result() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return hydra.lib.lists.foldl((lambda x1, x2: add_def(x1, x2)), start(), defs)
    tvars = result().type_variables
    @lru_cache(1)
    def result2() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return set_meta_uses_cast(True, set_meta_uses_lru_cache(True, result()))
    return set_meta_uses_type_var(result2(), hydra.lib.logic.not_(hydra.lib.sets.null(tvars)))

# The target Python version for code generation.
target_python_version = hydra.ext.python.utils.target_python_version

def initial_environment(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], tcontext: hydra.graph.Graph) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Create an initial Python environment for code generation."""

    return hydra.ext.python.helpers.PythonEnvironment(namespaces, ((), hydra.lib.maps.empty()), tcontext, hydra.lib.sets.empty(), target_python_version, True, hydra.lib.sets.empty())

def is_type_module_check(defs: frozenlist[hydra.module.Definition]):
    def _hoist_hydra_ext_python_coder_is_type_module_check_1(v1):
        match v1:
            case hydra.module.DefinitionType():
                return True

            case _:
                return False
    return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda d: _hoist_hydra_ext_python_coder_is_type_module_check_1(d)), defs)))

def module_domain_imports(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]) -> frozenlist[hydra.ext.python.syntax.ImportStatement]:
    r"""Generate domain import statements from namespace mappings."""

    @lru_cache(1)
    def names() -> frozenlist[hydra.ext.python.syntax.DottedName]:
        return hydra.lib.lists.sort(hydra.lib.maps.elems(namespaces.mapping))
    return hydra.lib.lists.map((lambda ns: cast(hydra.ext.python.syntax.ImportStatement, hydra.ext.python.syntax.ImportStatementName(hydra.ext.python.syntax.ImportName((hydra.ext.python.syntax.DottedAsName(ns, Nothing()),))))), names())

def standard_import_statement(mod_name: str, symbols: frozenlist[str]) -> hydra.ext.python.syntax.ImportStatement:
    r"""Generate a single from-import statement."""

    return cast(hydra.ext.python.syntax.ImportStatement, hydra.ext.python.syntax.ImportStatementFrom(hydra.ext.python.syntax.ImportFrom((), Just(hydra.ext.python.syntax.DottedName((hydra.ext.python.syntax.Name(mod_name),))), cast(hydra.ext.python.syntax.ImportFromTargets, hydra.ext.python.syntax.ImportFromTargetsSimple(hydra.lib.lists.map((lambda s: hydra.ext.python.syntax.ImportFromAsName(hydra.ext.python.syntax.Name(s), Nothing())), symbols))))))

def module_standard_imports(meta: hydra.ext.python.helpers.PythonModuleMetadata) -> frozenlist[hydra.ext.python.syntax.ImportStatement]:
    r"""Generate standard import statements based on module metadata."""

    @lru_cache(1)
    def pairs() -> frozenlist[tuple[str, frozenlist[Maybe[str]]]]:
        return (("__future__", (cond_import_symbol("annotations", hydra.ext.python.names.use_future_annotations),)), ("collections.abc", (cond_import_symbol("Callable", meta.uses_callable),)), ("dataclasses", (cond_import_symbol("dataclass", meta.uses_dataclass),)), ("decimal", (cond_import_symbol("Decimal", meta.uses_decimal),)), ("enum", (cond_import_symbol("Enum", meta.uses_enum),)), ("functools", (cond_import_symbol("lru_cache", meta.uses_lru_cache),)), ("hydra.dsl.python", (cond_import_symbol("Either", meta.uses_either), cond_import_symbol("FrozenDict", meta.uses_frozen_dict), cond_import_symbol("Just", meta.uses_just), cond_import_symbol("Left", meta.uses_left), cond_import_symbol("Maybe", meta.uses_maybe), cond_import_symbol("Node", meta.uses_node), cond_import_symbol("Nothing", meta.uses_nothing), cond_import_symbol("Right", meta.uses_right), cond_import_symbol("frozenlist", meta.uses_frozen_list))), ("typing", (cond_import_symbol("Annotated", meta.uses_annotated), cond_import_symbol("Generic", meta.uses_generic), cond_import_symbol("TypeAlias", meta.uses_type_alias), cond_import_symbol("TypeVar", meta.uses_type_var), cond_import_symbol("cast", meta.uses_cast))))
    @lru_cache(1)
    def simplified() -> frozenlist[tuple[str, frozenlist[str]]]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda p: (mod_name := hydra.lib.pairs.first(p), symbols := hydra.lib.maybes.cat(hydra.lib.pairs.second(p)), hydra.lib.logic.if_else(hydra.lib.lists.null(symbols), (lambda : Nothing()), (lambda : Just((mod_name, symbols)))))[2]), pairs()))
    return hydra.lib.lists.map((lambda p: standard_import_statement(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), simplified())

def module_imports(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], meta: hydra.ext.python.helpers.PythonModuleMetadata) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Generate all import statements for a Python module."""

    return hydra.lib.lists.map((lambda imp: hydra.ext.python.utils.py_simple_statement_to_py_statement(cast(hydra.ext.python.syntax.SimpleStatement, hydra.ext.python.syntax.SimpleStatementImport(imp)))), hydra.lib.lists.concat((module_standard_imports(meta), module_domain_imports(namespaces))))

def reorder_defs(defs: frozenlist[hydra.module.Definition]) -> frozenlist[hydra.module.Definition]:
    r"""Reorder definitions: types first, then topologically sorted terms."""

    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.module.TypeDefinition], frozenlist[hydra.module.TermDefinition]]:
        return hydra.schemas.partition_definitions(defs)
    @lru_cache(1)
    def type_defs_raw() -> frozenlist[hydra.module.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_defs_raw() -> frozenlist[hydra.module.TermDefinition]:
        return hydra.lib.pairs.second(partitioned())
    @lru_cache(1)
    def name_first() -> frozenlist[hydra.module.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: hydra.lib.equality.equal(td.name, hydra.core.Name("hydra.core.Name"))), type_defs_raw())
    @lru_cache(1)
    def name_rest() -> frozenlist[hydra.module.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: hydra.lib.logic.not_(hydra.lib.equality.equal(td.name, hydra.core.Name("hydra.core.Name")))), type_defs_raw())
    @lru_cache(1)
    def sorted_type_defs() -> frozenlist[hydra.module.Definition]:
        return hydra.lib.lists.concat((hydra.lib.lists.map((lambda td: cast(hydra.module.Definition, hydra.module.DefinitionType(td))), name_first()), hydra.lib.lists.map((lambda td: cast(hydra.module.Definition, hydra.module.DefinitionType(td))), name_rest())))
    @lru_cache(1)
    def term_defs() -> frozenlist[hydra.module.Definition]:
        return hydra.lib.lists.map((lambda td: cast(hydra.module.Definition, hydra.module.DefinitionTerm(td))), term_defs_raw())
    @lru_cache(1)
    def sorted_term_defs():
        def _hoist_sorted_term_defs_1(v1):
            match v1:
                case hydra.module.DefinitionTerm(value=td):
                    return td.name

                case _:
                    raise TypeError("Unsupported Definition")
        def _hoist_sorted_term_defs_2(v1):
            match v1:
                case hydra.module.DefinitionTerm(value=td):
                    return hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_term(td.term))

                case _:
                    return ()
        return hydra.lib.lists.concat(hydra.sorting.topological_sort_nodes((lambda d: _hoist_sorted_term_defs_1(d)), (lambda d: _hoist_sorted_term_defs_2(d)), term_defs()))
    return hydra.lib.lists.concat((sorted_type_defs(), sorted_term_defs()))

def set_meta_uses_type_alias(m: hydra.ext.python.helpers.PythonModuleMetadata, b: bool) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, b, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def tvar_statement(name: hydra.ext.python.syntax.Name) -> hydra.ext.python.syntax.Statement:
    r"""Create a TypeVar assignment statement for a type variable name."""

    return hydra.ext.python.utils.assignment_statement(name, hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("TypeVar"))))), (hydra.ext.python.utils.double_quoted_string(name.value),)))

def with_let(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Let, v3: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    r"""Execute a computation with let context (adds let bindings to Graph)."""

    return hydra.schemas.with_let_context((lambda x1: python_environment_get_graph(x1)), (lambda x1, x2: python_environment_set_graph(x1, x2)), (lambda x1, x2: python_binding_metadata(x1, x2)), v1, v2, v3)

def with_definitions(env: hydra.ext.python.helpers.PythonEnvironment, defs: frozenlist[hydra.module.Definition], body: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    r"""Execute a computation with definitions in scope."""

    @lru_cache(1)
    def bindings():
        def _hoist_bindings_1(v1):
            match v1:
                case hydra.module.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, Just(td.type)))

                case hydra.module.DefinitionType():
                    return Nothing()

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda def_: _hoist_bindings_1(def_)), defs))
    @lru_cache(1)
    def dummy_let() -> hydra.core.Let:
        return hydra.core.Let(bindings(), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("dummy")))))
    return with_let(env, dummy_let(), body)

def encode_python_module(cx: hydra.context.Context, g: hydra.graph.Graph, mod: hydra.module.Module, defs0: frozenlist[hydra.module.Definition]) -> Either[hydra.context.InContext[hydra.error.Error], hydra.ext.python.syntax.Module]:
    r"""Encode a Hydra module to a Python module AST."""

    @lru_cache(1)
    def defs() -> frozenlist[hydra.module.Definition]:
        return reorder_defs(defs0)
    @lru_cache(1)
    def meta0() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return gather_metadata(mod.namespace, defs())
    namespaces0 = meta0().namespaces
    @lru_cache(1)
    def env0() -> hydra.ext.python.helpers.PythonEnvironment:
        return initial_environment(namespaces0, g)
    @lru_cache(1)
    def is_type_mod() -> bool:
        return is_type_module_check(defs0)
    return with_definitions(env0(), defs(), (lambda env: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda d: encode_definition(cx, env, d)), defs())), (lambda def_stmts: (meta2 := hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(is_type_mod()), use_inline_type_params()), (lambda : set_meta_uses_type_var(meta0(), False)), (lambda : meta0())), meta := hydra.lib.logic.if_else(hydra.lib.logic.and_(is_type_mod(), hydra.lib.equality.equal(target_python_version, hydra.ext.python.helpers.PythonVersion.PYTHON310)), (lambda : set_meta_uses_type_alias(meta2, True)), (lambda : meta2)), namespaces := meta0().namespaces, comment_stmts := hydra.lib.maybes.maybe((lambda : ()), (lambda c: (hydra.ext.python.utils.comment_statement(c),)), hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, mod.description)), import_stmts := module_imports(namespaces, meta), tvars := hydra.lib.logic.if_else(hydra.lib.logic.or_(is_type_mod(), hydra.lib.logic.not_(use_inline_type_params())), (lambda : meta.type_variables), (lambda : hydra.lib.sets.empty())), tvar_stmts := hydra.lib.lists.map((lambda tv: tvar_statement(hydra.ext.python.names.encode_type_variable(tv))), hydra.lib.sets.to_list(tvars)), body := hydra.lib.lists.filter((lambda group: hydra.lib.logic.not_(hydra.lib.lists.null(group))), hydra.lib.lists.concat(((comment_stmts, import_stmts, tvar_stmts), def_stmts))), Right(hydra.ext.python.syntax.Module(body)))[8]))))

def set_meta_namespaces(ns: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(ns, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def extend_meta_for_types(types: frozenlist[hydra.core.Type], meta: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Extend metadata for a list of types."""

    @lru_cache(1)
    def names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda t: hydra.rewriting.type_dependency_names(False, t)), types))
    current_ns = meta.namespaces
    @lru_cache(1)
    def updated_ns() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return hydra.schemas.add_names_to_namespaces((lambda x1: hydra.ext.python.names.encode_namespace(x1)), names(), current_ns)
    @lru_cache(1)
    def meta1() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return set_meta_namespaces(updated_ns(), meta)
    return hydra.lib.lists.foldl((lambda m, t: extend_meta_for_type(True, False, t, m)), meta1(), types)

def initial_metadata(ns: hydra.module.Namespace) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Create initial empty metadata for a Python module."""

    @lru_cache(1)
    def dotted_ns() -> hydra.ext.python.syntax.DottedName:
        return hydra.ext.python.names.encode_namespace(ns)
    @lru_cache(1)
    def empty_ns() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return hydra.module.Namespaces((ns, dotted_ns()), hydra.lib.maps.empty())
    return hydra.ext.python.helpers.PythonModuleMetadata(empty_ns(), hydra.lib.sets.empty(), False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False)

def make_py_graph(g: hydra.graph.Graph, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PyGraph:
    r"""Constructor for PyGraph record."""

    return hydra.ext.python.helpers.PyGraph(g, m)

def module_to_python(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.error.Error], FrozenDict[str, str]]:
    r"""Convert a Hydra module to Python source files."""

    return hydra.lib.eithers.bind(encode_python_module(cx, g, mod, defs), (lambda file: (s := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.python.serde.encode_module(file))), path := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.LOWER_SNAKE, hydra.module.FileExtension("py"), mod.namespace), Right(hydra.lib.maps.singleton(path, s)))[2]))

def py_graph_graph(pyg: hydra.ext.python.helpers.PyGraph) -> hydra.graph.Graph:
    r"""Accessor for the graph field of PyGraph."""

    return pyg.graph

def py_graph_metadata(pyg: hydra.ext.python.helpers.PyGraph) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Accessor for the metadata field of PyGraph."""

    return pyg.metadata

def wildcard_case_block(stmt: hydra.ext.python.syntax.Statement) -> hydra.ext.python.syntax.CaseBlock:
    r"""Create a wildcard case block with a given body statement."""

    return hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.py_closed_pattern_to_py_patterns(cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternWildcard())), Nothing(), hydra.ext.python.utils.indented_block(Nothing(), ((stmt,),)))

def with_lambda(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Lambda, v3: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    r"""Execute a computation with lambda context (adds lambda parameter to Graph)."""

    return hydra.schemas.with_lambda_context((lambda x1: python_environment_get_graph(x1)), (lambda x1, x2: python_environment_set_graph(x1, x2)), v1, v2, v3)
