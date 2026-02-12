# Note: this is an automatically generated file. Do not edit.

r"""Python code generator: converts Hydra modules to Python source code."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.arity
import hydra.checking
import hydra.coder_utils
import hydra.coders
import hydra.core
import hydra.ext.python.helpers
import hydra.ext.python.names
import hydra.ext.python.serde
import hydra.ext.python.syntax
import hydra.ext.python.utils
import hydra.graph
import hydra.inference
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
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
import hydra.monads
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

@lru_cache(1)
def python_environment_get_type_context() -> Callable[[hydra.ext.python.helpers.PythonEnvironment], hydra.typing.TypeContext]:
    r"""Get the TypeContext from a PythonEnvironment."""
    
    return (lambda v1: v1.type_context)

def python_environment_set_type_context(tc: hydra.typing.TypeContext, env: hydra.ext.python.helpers.PythonEnvironment) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Set the TypeContext in a PythonEnvironment."""
    
    return hydra.ext.python.helpers.PythonEnvironment(env.namespaces, env.bound_type_variables, tc, env.nullary_bindings, env.version, env.skip_casts, env.inline_variables)

def analyze_python_function(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.typing.FunctionStructure[hydra.ext.python.helpers.PythonEnvironment]]:
    return hydra.coder_utils.analyze_function_term((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), v1, v2)

def analyze_python_function_inline(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.typing.FunctionStructure[hydra.ext.python.helpers.PythonEnvironment]]:
    return hydra.coder_utils.analyze_function_term_inline((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), v1, v2)

def class_variant_pattern_unit(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a class pattern for a unit variant (no value captured)."""
    
    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternClass(hydra.ext.python.syntax.ClassPattern(hydra.ext.python.syntax.NameOrAttribute((hydra.ext.python.names.variant_name(True, env, type_name, field_name),)), Nothing(), Nothing())))

def class_variant_pattern_with_capture(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name, var_name: hydra.core.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a class pattern for a variant with captured value."""
    
    @lru_cache(1)
    def py_var_name() -> hydra.ext.python.syntax.NameOrAttribute:
        return hydra.ext.python.syntax.NameOrAttribute((hydra.ext.python.names.variant_name(True, env, type_name, field_name),))
    @lru_cache(1)
    def capture_pattern() -> hydra.ext.python.syntax.ClosedPattern:
        return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternCapture(hydra.ext.python.syntax.CapturePattern(hydra.ext.python.syntax.PatternCaptureTarget(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, var_name)))))
    @lru_cache(1)
    def keyword_pattern() -> hydra.ext.python.syntax.KeywordPattern:
        return hydra.ext.python.syntax.KeywordPattern(hydra.ext.python.syntax.Name("value"), cast(hydra.ext.python.syntax.Pattern, hydra.ext.python.syntax.PatternOr(hydra.ext.python.syntax.OrPattern((capture_pattern(),)))))
    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternClass(hydra.ext.python.syntax.ClassPattern(py_var_name(), Nothing(), Just(hydra.ext.python.syntax.KeywordPatterns((keyword_pattern(),))))))

def is_type_variable_name(name: hydra.core.Name) -> bool:
    r"""Check if a name is a type variable (unqualified - no dots)."""
    
    return hydra.lib.equality.equal(1, hydra.lib.lists.length(hydra.lib.strings.split_on(".", name.value)))

def collect_type_variables(initial: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Collect type variables from a type."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeForall(value=ft):
            @lru_cache(1)
            def v() -> hydra.core.Name:
                return ft.parameter
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return ft.body
            return collect_type_variables(hydra.lib.sets.insert(v(), initial), body())
        
        case _:
            return (free_vars := hydra.rewriting.free_variables_in_type(typ), (is_type_var := (lambda n: is_type_variable_name(n)), (filtered_list := hydra.lib.lists.filter((lambda x1: is_type_var(x1)), hydra.lib.sets.to_list(free_vars)), hydra.lib.sets.union(initial, hydra.lib.sets.from_list(filtered_list)))[1])[1])[1]

def cond_import_symbol(name: T0, flag: bool) -> Maybe[T0]:
    return hydra.lib.logic.if_else(flag, (lambda : Just(name)), (lambda : Nothing()))

@lru_cache(1)
def dataclass_decorator() -> hydra.ext.python.syntax.NamedExpression:
    r"""Create a @dataclass(frozen=True) decorator."""
    
    return cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_rhs(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("dataclass"))))), cast(hydra.ext.python.syntax.PrimaryRhs, hydra.ext.python.syntax.PrimaryRhsCall(hydra.ext.python.syntax.Args((), (cast(hydra.ext.python.syntax.KwargOrStarred, hydra.ext.python.syntax.KwargOrStarredKwarg(hydra.ext.python.syntax.Kwarg(hydra.ext.python.syntax.Name("frozen"), hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTrue()))))),), ())))))))

def deduplicate_case_variables(cases_: frozenlist[hydra.core.Field]) -> frozenlist[hydra.core.Field]:
    r"""Rewrite case statements to avoid variable name collisions."""
    
    def rewrite_case(state: tuple[FrozenDict[hydra.core.Name, int], frozenlist[hydra.core.Field]], field: hydra.core.Field) -> tuple[FrozenDict[hydra.core.Name, int], frozenlist[hydra.core.Field]]:
        @lru_cache(1)
        def count_by_name() -> FrozenDict[hydra.core.Name, int]:
            return hydra.lib.pairs.first(state)
        @lru_cache(1)
        def done() -> frozenlist[hydra.core.Field]:
            return hydra.lib.pairs.second(state)
        @lru_cache(1)
        def fname() -> hydra.core.Name:
            return field.name
        @lru_cache(1)
        def fterm() -> hydra.core.Term:
            return field.term
        def _hoist_body_1(v1: hydra.core.Function) -> tuple[FrozenDict[hydra.core.Name, int], frozenlist[hydra.core.Field]]:
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    @lru_cache(1)
                    def v() -> hydra.core.Name:
                        return lam.parameter
                    @lru_cache(1)
                    def mdom() -> Maybe[hydra.core.Type]:
                        return lam.domain
                    @lru_cache(1)
                    def body() -> hydra.core.Term:
                        return lam.body
                    return hydra.lib.maybes.maybe((hydra.lib.maps.insert(v(), 1, count_by_name()), hydra.lib.lists.cons(field, done())), (lambda count: (count2 := hydra.lib.math.add(count, 1), v2 := hydra.core.Name(hydra.lib.strings.cat2(v().value, hydra.lib.literals.show_int32(count2))), new_body := hydra.reduction.alpha_convert(v(), v2, body()), new_lam := hydra.core.Lambda(v2, mdom(), new_body), new_term := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(new_lam)))), new_field := hydra.core.Field(fname(), new_term), (hydra.lib.maps.insert(v(), count2, count_by_name()), hydra.lib.lists.cons(new_field, done())))[6]), hydra.lib.maps.lookup(v(), count_by_name()))
                
                case _:
                    return (count_by_name(), hydra.lib.lists.cons(field, done()))
        match hydra.rewriting.deannotate_term(fterm()):
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
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeForall(value=ft):
            return dig_for_wrap(is_term_annot, meta, ft.body)
        
        case hydra.core.TypeWrap():
            return hydra.lib.logic.if_else(is_term_annot, (lambda : meta), (lambda : set_meta_uses_node(meta, True)))
        
        case _:
            return meta

def eliminate_unit_var(v: hydra.core.Name, term0: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute unit for a variable in a term (for unit variant case handling)."""
    
    def rewrite_field(rewrite2: Callable[[hydra.core.Term], hydra.core.Term], fld: hydra.core.Field) -> hydra.core.Field:
        return hydra.core.Field(fld.name, rewrite2(fld.term))
    def rewrite_binding(rewrite2: Callable[[hydra.core.Term], hydra.core.Term], bnd: hydra.core.Binding) -> hydra.core.Binding:
        return hydra.core.Binding(bnd.name, rewrite2(bnd.term), bnd.type)
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def _hoist_rewrite_1(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term, v1: hydra.core.Elimination) -> hydra.core.Term:
            match v1:
                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map((lambda v12: rewrite_field(recurse, v12)), cs.cases))))))))
                
                case _:
                    return term
        def _hoist_rewrite_2(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term, v1: hydra.core.Function) -> hydra.core.Term:
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(lam.parameter, v), (lambda : term), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(lam.parameter, lam.domain, recurse(lam.body))))))))
                
                case hydra.core.FunctionElimination(value=e):
                    return _hoist_rewrite_1(recurse, term, e)
                
                case _:
                    return term
        match hydra.rewriting.deannotate_term(term):
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

@lru_cache(1)
def py_graph_graph() -> Callable[[hydra.ext.python.helpers.PyGraph], hydra.graph.Graph]:
    r"""Accessor for the graph field of PyGraph."""
    
    return (lambda v1: v1.graph)

def encode_variable(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, args: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
    r"""Encode a variable reference to a Python expression."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda pyg: (g := py_graph_graph(pyg), tc := env.type_context, tc_types := tc.types, tc_lambda_vars := tc.lambda_variables, tc_metadata := tc.metadata, inline_vars := env.inline_variables, m_typ := hydra.lib.maps.lookup(name, tc_types), as_variable := hydra.ext.python.names.term_variable_reference(env, name), as_function_call := hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), args), hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(args)), (lambda : hydra.lib.maybes.maybe(hydra.lib.flows.pure(as_function_call), (lambda prim: (prim_arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.equal(prim_arity, hydra.lib.lists.length(args)), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (num_remaining := hydra.lib.math.sub(prim_arity, hydra.lib.lists.length(args)), (remaining_params := hydra.lib.lists.map((lambda i: hydra.ext.python.syntax.Name(hydra.lib.strings.cat2("x", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, num_remaining)), (remaining_exprs := hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(n))))), Nothing()))))))))), ()))),)),))))), remaining_params), (all_args := hydra.lib.lists.concat2(args, remaining_exprs), (full_call := hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), all_args), hydra.lib.flows.pure(make_uncurried_lambda(remaining_params, full_call)))[1])[1])[1])[1])[1])))[1]), hydra.lexical.lookup_primitive(g, name))), (lambda : hydra.lib.maybes.maybe(hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc_lambda_vars), (lambda : hydra.lib.flows.pure(as_variable)), (lambda : hydra.lib.maybes.maybe(hydra.lib.maybes.maybe(hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("Unknown variable: ", name.value)), (lambda _: hydra.lib.flows.pure(as_function_call)), hydra.lib.maps.lookup(name, tc_metadata)), (lambda el: hydra.lib.maybes.maybe(hydra.lib.flows.pure(as_variable), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_scheme_arity(ts), 0), hydra.coder_utils.is_complex_binding(tc, el)), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : make_simple_lambda(hydra.arity.type_arity(ts.type), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1]))), el.type)), hydra.lexical.lookup_element(g, name)), (lambda prim: (prim_arity := hydra.arity.primitive_arity(prim), hydra.lib.logic.if_else(hydra.lib.equality.equal(prim_arity, 0), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (ts := prim.type, (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), (lambda : make_simple_lambda(hydra.arity.type_arity(ts.type), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1])[1])))[1]), hydra.lexical.lookup_primitive(g, name)))), (lambda typ: hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc_lambda_vars), (lambda : hydra.lib.flows.pure(as_variable)), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, inline_vars), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.member(name, tc_metadata)), (lambda : hydra.lib.maybes.maybe((as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1], (lambda el: hydra.lib.maybes.maybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1])), (lambda ts: hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), hydra.coder_utils.is_complex_binding(tc, el)), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1]))), el.type)), hydra.lexical.lookup_element(g, name))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_arity(typ), 0), hydra.coder_utils.is_complex_variable(tc, name)), (lambda : hydra.lib.flows.pure(as_function_call)), (lambda : (as_function_ref := hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ))), (lambda : make_simple_lambda(hydra.arity.type_arity(typ), as_variable)), (lambda : as_variable)), hydra.lib.flows.pure(as_function_ref))[1]))))))))), m_typ))))[9]))

def make_curried_lambda(params: frozenlist[hydra.ext.python.syntax.Name], body: hydra.ext.python.syntax.Expression) -> hydra.ext.python.syntax.Expression:
    r"""Create a curried lambda chain from a list of parameter names and a body."""
    
    return hydra.lib.lists.foldl((lambda acc, p: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionLambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(Nothing(), (hydra.ext.python.syntax.LambdaParamNoDefault(p),), (), Nothing()), acc)))), body, hydra.lib.lists.reverse(params))

def unsupported_expression(msg: str) -> hydra.ext.python.syntax.Expression:
    r"""Create an expression that calls hydra.dsl.python.unsupported(message) at runtime."""
    
    return hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("hydra")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.syntax.Name("dsl")), hydra.ext.python.syntax.Name("python")), hydra.ext.python.syntax.Name("unsupported"))), (hydra.ext.python.utils.string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.DOUBLE, msg),))

def encode_float_value(fv: hydra.core.FloatValue) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    match fv:
        case hydra.core.FloatValueBigfloat(value=f):
            return hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Decimal")), (hydra.ext.python.utils.single_quoted_string(hydra.lib.literals.show_bigfloat(f)),)))
        
        case hydra.core.FloatValueFloat32(value=f2):
            return hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberFloat(hydra.lib.literals.float32_to_bigfloat(f2)))))))
        
        case hydra.core.FloatValueFloat64(value=f3):
            return hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberFloat(hydra.lib.literals.float64_to_bigfloat(f3)))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_integer_value(iv: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    def to_py_int(n: int) -> hydra.compute.Flow[T1, hydra.ext.python.syntax.Expression]:
        return hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(n))))))
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

def encode_literal(lit: hydra.core.Literal) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    match lit:
        case hydra.core.LiteralBinary(value=bs):
            @lru_cache(1)
            def byte_values() -> frozenlist[int]:
                return hydra.lib.literals.binary_to_bytes(bs)
            return hydra.lib.flows.pure(hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("bytes"))))), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomList(hydra.ext.python.utils.py_list(hydra.lib.lists.map((lambda byte_val: hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(byte_val))))))), byte_values()))))),)))
        
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(hydra.lib.logic.if_else(b, (lambda : cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTrue())), (lambda : cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomFalse())))))
        
        case hydra.core.LiteralFloat(value=f):
            return encode_float_value(f)
        
        case hydra.core.LiteralInteger(value=i):
            return encode_integer_value(i)
        
        case hydra.core.LiteralString(value=s):
            return hydra.lib.flows.pure(hydra.ext.python.utils.string_to_py_expression(hydra.ext.python.syntax.QuoteStyle.DOUBLE, s))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal_type(lt: hydra.core.LiteralType) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    @lru_cache(1)
    def find_name() -> str:
        def _hoist_find_name_1(v1: hydra.core.FloatType) -> str:
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
    return hydra.lib.flows.pure(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name(find_name())))))), Nothing()))))))))), ()))),)),)))))

def encode_application_type(env: hydra.ext.python.helpers.PythonEnvironment, at: hydra.core.ApplicationType) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    def gather_params(t: hydra.core.Type, ps: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Type, frozenlist[hydra.core.Type]]:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeApplication(value=app_t):
                return gather_params(app_t.function, hydra.lib.lists.cons(app_t.argument, ps))
            
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
    return hydra.lib.flows.bind(encode_type(env, body()), (lambda py_body: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_type(env, v1)), args()), (lambda py_args: hydra.lib.flows.pure(hydra.ext.python.utils.primary_and_params(hydra.ext.python.utils.py_expression_to_py_primary(py_body), py_args))))))

def encode_forall_type(env: hydra.ext.python.helpers.PythonEnvironment, lt: hydra.core.ForallType) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    def gather_params(t: hydra.core.Type, ps: frozenlist[hydra.core.Name]) -> tuple[hydra.core.Type, frozenlist[hydra.core.Name]]:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeForall(value=forall_t):
                return gather_params(forall_t.body, hydra.lib.lists.cons(forall_t.parameter, ps))
            
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
    return hydra.lib.flows.bind(encode_type(env, body()), (lambda py_body: hydra.lib.flows.pure(hydra.ext.python.utils.primary_and_params(hydra.ext.python.utils.py_expression_to_py_primary(py_body), hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name(n.value)))))), Nothing()))))))))), ()))),)),))))), params())))))

def encode_function_type(env: hydra.ext.python.helpers.PythonEnvironment, ft: hydra.core.FunctionType) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    def gather_params(rdoms: frozenlist[hydra.core.Type], ftype: hydra.core.FunctionType) -> tuple[frozenlist[hydra.core.Type], hydra.core.Type]:
        @lru_cache(1)
        def inner_cod() -> hydra.core.Type:
            return ftype.codomain
        @lru_cache(1)
        def dom() -> hydra.core.Type:
            return ftype.domain
        match hydra.rewriting.deannotate_type(inner_cod()):
            case hydra.core.TypeFunction(value=ft2):
                return gather_params(hydra.lib.lists.cons(dom(), rdoms), ft2)
            
            case hydra.core.TypeAnnotated():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeApplication():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeForall():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeList():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeLiteral():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeMap():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeMaybe():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeEither():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypePair():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeRecord():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeSet():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeUnion():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeUnit():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeVariable():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
            case hydra.core.TypeWrap():
                return (hydra.lib.lists.reverse(hydra.lib.lists.cons(dom(), rdoms)), inner_cod())
            
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
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_type(env, v1)), doms()), (lambda pydoms: hydra.lib.flows.bind(encode_type(env, cod()), (lambda pycod: hydra.lib.flows.pure(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Callable"))))), hydra.ext.python.utils.py_primary_to_py_slice(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomList(hydra.ext.python.utils.py_list(pydoms)))))), (cast(hydra.ext.python.syntax.SliceOrStarredExpression, hydra.ext.python.syntax.SliceOrStarredExpressionSlice(hydra.ext.python.utils.py_expression_to_py_slice(pycod))),))))))))

def encode_type(env: hydra.ext.python.helpers.PythonEnvironment, typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    @lru_cache(1)
    def dflt() -> hydra.compute.Flow[T1, hydra.ext.python.syntax.Expression]:
        return hydra.lib.flows.pure(hydra.ext.python.utils.double_quoted_string(hydra.lib.strings.cat2("type = ", hydra.show.core.type(hydra.rewriting.deannotate_type(typ)))))
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeApplication(value=at):
            return encode_application_type(env, at)
        
        case hydra.core.TypeFunction(value=ft):
            return encode_function_type(env, ft)
        
        case hydra.core.TypeForall(value=lt):
            return encode_forall_type(env, lt)
        
        case hydra.core.TypeList(value=et):
            return hydra.lib.flows.bind(encode_type(env, et), (lambda pyet: hydra.lib.flows.pure(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("frozenlist"), (pyet,)))))
        
        case hydra.core.TypeMap(value=mt):
            return hydra.lib.flows.bind(encode_type(env, mt.keys), (lambda pykt: hydra.lib.flows.bind(encode_type(env, mt.values), (lambda pyvt: hydra.lib.flows.pure(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("FrozenDict"), (pykt, pyvt)))))))
        
        case hydra.core.TypeLiteral(value=lt2):
            return encode_literal_type(lt2)
        
        case hydra.core.TypeMaybe(value=et2):
            return hydra.lib.flows.bind(encode_type(env, et2), (lambda ptype: hydra.lib.flows.pure(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Maybe"))))), (ptype,))))))
        
        case hydra.core.TypeEither(value=either_t):
            return hydra.lib.flows.bind(encode_type(env, either_t.left), (lambda pyleft: hydra.lib.flows.bind(encode_type(env, either_t.right), (lambda pyright: hydra.lib.flows.pure(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Either"))))), (pyleft, pyright))))))))
        
        case hydra.core.TypePair(value=pair_t):
            return hydra.lib.flows.bind(encode_type(env, pair_t.first), (lambda py_first: hydra.lib.flows.bind(encode_type(env, pair_t.second), (lambda py_second: hydra.lib.flows.pure(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("tuple"), (py_first, py_second)))))))
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.flows.pure(hydra.ext.python.names.type_variable_reference(env, rt.type_name))
        
        case hydra.core.TypeSet(value=et3):
            return hydra.lib.flows.bind(encode_type(env, et3), (lambda pyet: hydra.lib.flows.pure(hydra.ext.python.utils.name_and_params(hydra.ext.python.syntax.Name("frozenset"), (pyet,)))))
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.flows.pure(hydra.ext.python.names.type_variable_reference(env, rt2.type_name))
        
        case hydra.core.TypeUnit():
            return hydra.lib.flows.pure(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.utils.py_none))
        
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.flows.pure(hydra.ext.python.names.type_variable_reference(env, name))
        
        case hydra.core.TypeWrap(value=wt):
            return hydra.lib.flows.pure(hydra.ext.python.names.type_variable_reference(env, wt.type_name))
        
        case hydra.core.TypeAnnotated():
            return dflt()
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def make_py_graph(g: hydra.graph.Graph, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PyGraph:
    r"""Constructor for PyGraph record."""
    
    return hydra.ext.python.helpers.PyGraph(g, m)

@lru_cache(1)
def py_graph_metadata() -> Callable[[hydra.ext.python.helpers.PyGraph], hydra.ext.python.helpers.PythonModuleMetadata]:
    r"""Accessor for the metadata field of PyGraph."""
    
    return (lambda v1: v1.metadata)

def in_graph_context(graph_flow: hydra.compute.Flow[hydra.graph.Graph, T0]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, T0]:
    return hydra.coder_utils.in_coder_graph_context((lambda x1: py_graph_graph(x1)), (lambda x1: py_graph_metadata(x1)), (lambda x1, x2: make_py_graph(x1, x2)), graph_flow)

def set_meta_uses_cast(b: bool, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, b, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def update_meta(v1: Callable[[hydra.ext.python.helpers.PythonModuleMetadata], hydra.ext.python.helpers.PythonModuleMetadata]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, None]:
    r"""Update the Python module metadata in the coder state."""
    
    return hydra.coder_utils.update_coder_metadata((lambda x1: py_graph_metadata(x1)), (lambda x1, x2: make_py_graph(x1, x2)), (lambda x1: py_graph_graph(x1)), v1)

def with_let_inline(env: hydra.ext.python.helpers.PythonEnvironment, lt: hydra.core.Let, body: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    @lru_cache(1)
    def binding_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda b: b.name), lt.bindings)
    @lru_cache(1)
    def inline_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(binding_names())
    def no_metadata(tc: T1, b: T2) -> Maybe[T3]:
        return Nothing()
    return hydra.schemas.with_let_context((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), no_metadata, env, lt, (lambda inner_env: (updated_env := hydra.ext.python.helpers.PythonEnvironment(inner_env.namespaces, inner_env.bound_type_variables, inner_env.type_context, inner_env.nullary_bindings, inner_env.version, inner_env.skip_casts, hydra.lib.sets.union(inline_vars(), inner_env.inline_variables)), body(updated_env))[1]))

def with_type_lambda(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.TypeLambda, v3: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    return hydra.schemas.with_type_lambda_context((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), v1, v2, v3)

def wrap_lazy_arguments(name: hydra.core.Name, args: frozenlist[hydra.ext.python.syntax.Expression]) -> frozenlist[hydra.ext.python.syntax.Expression]:
    r"""Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation."""
    
    return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(name, hydra.core.Name("hydra.lib.logic.ifElse")), hydra.lib.equality.equal(hydra.lib.lists.length(args), 3)), (lambda : (hydra.lib.lists.at(0, args), wrap_in_nullary_lambda(hydra.lib.lists.at(1, args)), wrap_in_nullary_lambda(hydra.lib.lists.at(2, args)))), (lambda : args))

def function_arity_with_primitives(graph: hydra.graph.Graph, f: hydra.core.Function) -> int:
    r"""Calculate function arity with proper primitive handling."""
    
    match f:
        case hydra.core.FunctionElimination():
            return 1
        
        case hydra.core.FunctionLambda(value=lam):
            return hydra.lib.math.add(1, term_arity_with_primitives(graph, lam.body))
        
        case hydra.core.FunctionPrimitive(value=name):
            return hydra.lib.maybes.maybe(0, (lambda prim: hydra.arity.primitive_arity(prim)), hydra.lib.maps.lookup(name, graph.primitives))
        
        case _:
            return 0

def term_arity_with_primitives(graph: hydra.graph.Graph, term: hydra.core.Term) -> int:
    r"""Calculate term arity with proper primitive handling."""
    
    match hydra.rewriting.deannotate_and_detype_term(term):
        case hydra.core.TermApplication(value=app):
            return hydra.lib.math.max(0, hydra.lib.math.sub(term_arity_with_primitives(graph, app.function), 1))
        
        case hydra.core.TermFunction(value=f):
            return function_arity_with_primitives(graph, f)
        
        case _:
            return 0

def encode_application(env: hydra.ext.python.helpers.PythonEnvironment, app: hydra.core.Application) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
    r"""Encode a function application to a Python expression."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda pyg: (g := py_graph_graph(pyg), tc := env.type_context, skip_casts := env.skip_casts, term := cast(hydra.core.Term, hydra.core.TermApplication(app)), gathered := hydra.coder_utils.gather_args(term, ()), fun := hydra.lib.pairs.first(gathered), args := hydra.lib.pairs.second(gathered), term_arity := term_arity_with_primitives(g, fun), hydra.lib.flows.bind(hydra.lib.flows.with_default(term_arity, hydra.lib.flows.map((lambda x1: hydra.arity.type_arity(x1)), in_graph_context(hydra.checking.type_of(tc, (), fun)))), (lambda arity: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda t: encode_term_inline(env, False, t)), args), (lambda pargs: (hargs := hydra.lib.lists.take(arity, pargs), rargs := hydra.lib.lists.drop(arity, pargs), hydra.lib.flows.bind(encode_application_inner(env, fun, hargs, rargs), (lambda result: (lhs := hydra.lib.pairs.first(result), remaining_rargs := hydra.lib.pairs.second(result), pyapp := hydra.lib.lists.foldl((lambda t, a: hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(t), (a,))), lhs, remaining_rargs), hydra.lib.flows.pure(pyapp))[3])))[2])))))[8]))

def encode_application_inner(env: hydra.ext.python.helpers.PythonEnvironment, fun: hydra.core.Term, hargs: frozenlist[hydra.ext.python.syntax.Expression], rargs: frozenlist[hydra.ext.python.syntax.Expression]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, tuple[hydra.ext.python.syntax.Expression, frozenlist[hydra.ext.python.syntax.Expression]]]:
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
    def default_case() -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, tuple[hydra.ext.python.syntax.Expression, frozenlist[hydra.ext.python.syntax.Expression]]]:
        return hydra.lib.flows.bind(encode_term_inline(env, False, fun), (lambda pfun: hydra.lib.flows.pure((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(pfun), hargs), rargs))))
    def _hoist_body_1(v1: hydra.core.Elimination) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, tuple[hydra.ext.python.syntax.Expression, frozenlist[hydra.ext.python.syntax.Expression]]]:
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return proj.field
                @lru_cache(1)
                def field_expr() -> hydra.ext.python.syntax.Expression:
                    return hydra.ext.python.utils.project_from_expression(first_arg(), hydra.ext.python.names.encode_field_name(env, fname()))
                return hydra.lib.flows.pure((with_rest(field_expr()), rargs))
            
            case hydra.core.EliminationUnion():
                return hydra.lib.flows.pure((unsupported_expression("inline match expressions are not yet supported"), rargs))
            
            case hydra.core.EliminationWrap():
                @lru_cache(1)
                def value_expr() -> hydra.ext.python.syntax.Expression:
                    return hydra.ext.python.utils.project_from_expression(first_arg(), hydra.ext.python.syntax.Name("value"))
                @lru_cache(1)
                def all_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
                    return hydra.lib.lists.concat2(rest_args(), rargs)
                return hydra.lib.logic.if_else(hydra.lib.lists.null(all_args()), (lambda : hydra.lib.flows.pure((value_expr(), ()))), (lambda : hydra.lib.flows.pure((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(value_expr()), all_args()), ()))))
            
            case _:
                return default_case()
    def _hoist_body_2(v1: hydra.core.Function) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, tuple[hydra.ext.python.syntax.Expression, frozenlist[hydra.ext.python.syntax.Expression]]]:
        match v1:
            case hydra.core.FunctionElimination(value=elm):
                return _hoist_body_1(elm)
            
            case hydra.core.FunctionPrimitive(value=name):
                @lru_cache(1)
                def wrapped_args() -> frozenlist[hydra.ext.python.syntax.Expression]:
                    return wrap_lazy_arguments(name, hargs)
                return hydra.lib.flows.bind(encode_variable(env, name, wrapped_args()), (lambda expr: hydra.lib.flows.pure((expr, rargs))))
            
            case hydra.core.FunctionLambda():
                return hydra.lib.flows.bind(encode_term_inline(env, False, fun), (lambda pfun: hydra.lib.flows.pure((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_expression_to_py_primary(pfun), hargs), rargs))))
            
            case _:
                return default_case()
    match hydra.rewriting.deannotate_term(fun):
        case hydra.core.TermFunction(value=f):
            return _hoist_body_2(f)
        
        case hydra.core.TermVariable(value=name):
            return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda pyg: (g := py_graph_graph(pyg), all_args := hydra.lib.lists.concat2(hargs, rargs), hydra.lib.maybes.maybe(hydra.lib.flows.bind(encode_variable(env, name, hargs), (lambda expr: hydra.lib.flows.pure((expr, rargs)))), (lambda el: hydra.lib.maybes.maybe(hydra.lib.flows.bind(encode_variable(env, name, hargs), (lambda expr: hydra.lib.flows.pure((expr, rargs)))), (lambda ts: (el_arity := hydra.arity.type_scheme_arity(ts), consume_count := hydra.lib.math.min(el_arity, hydra.lib.lists.length(all_args)), consumed_args := hydra.lib.lists.take(consume_count, all_args), remaining_args := hydra.lib.lists.drop(consume_count, all_args), hydra.lib.logic.if_else(hydra.lib.lists.null(consumed_args), (lambda : hydra.lib.flows.bind(encode_variable(env, name, ()), (lambda expr: hydra.lib.flows.pure((expr, rargs))))), (lambda : hydra.lib.flows.pure((hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name)), consumed_args), remaining_args)))))[4]), el.type)), hydra.lexical.lookup_element(g, name)))[2]))
        
        case _:
            return default_case()

def encode_binding_as_assignment(allow_thunking: bool, env: hydra.ext.python.helpers.PythonEnvironment, binding: hydra.core.Binding) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.NamedExpression]:
    r"""Encode a binding as a walrus operator assignment."""
    
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return binding.name
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return binding.term
    @lru_cache(1)
    def mts() -> Maybe[hydra.core.TypeScheme]:
        return binding.type
    @lru_cache(1)
    def py_name() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, name())
    return hydra.lib.flows.bind(encode_term_inline(env, False, term()), (lambda pbody: (tc := env.type_context, is_complex_var := hydra.coder_utils.is_complex_variable(tc, name()), term_is_complex := hydra.coder_utils.is_complex_term(tc, term()), needs_thunk := hydra.lib.maybes.maybe(hydra.lib.logic.and_(allow_thunking, hydra.lib.logic.or_(is_complex_var, term_is_complex)), (lambda ts: hydra.lib.logic.and_(allow_thunking, hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.arity.type_scheme_arity(ts), 0), hydra.lib.logic.or_(is_complex_var, term_is_complex)))), mts()), pterm := hydra.lib.logic.if_else(needs_thunk, (lambda : make_thunk(pbody)), (lambda : pbody)), hydra.lib.flows.pure(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionAssignment(hydra.ext.python.syntax.AssignmentExpression(py_name(), pterm)))))[5]))

def encode_function(env: hydra.ext.python.helpers.PythonEnvironment, f: hydra.core.Function) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
    def _hoist_hydra_ext_python_coder_encode_function_1(env: hydra.ext.python.helpers.PythonEnvironment, v1: hydra.core.Elimination) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return proj.field
                return hydra.lib.flows.pure(make_curried_lambda((hydra.ext.python.syntax.Name("v1"),), hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("v1")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.names.encode_field_name(env, fname()))))
            
            case hydra.core.EliminationWrap():
                return hydra.lib.flows.pure(make_curried_lambda((hydra.ext.python.syntax.Name("v1"),), hydra.ext.python.utils.project_from_expression(cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("v1")))))), Nothing()))))))))), ()))),)),)))), hydra.ext.python.syntax.Name("value"))))
            
            case hydra.core.EliminationUnion():
                return hydra.lib.flows.pure(unsupported_expression("case expressions as values are not yet supported"))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match f:
        case hydra.core.FunctionLambda(value=lam):
            return hydra.lib.flows.bind(analyze_python_function_inline(env, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(lam))))), (lambda fs: (params := fs.params, bindings := fs.bindings, inner_body := fs.body, inner_env := fs.environment, hydra.lib.flows.bind(encode_term_inline(inner_env, False, inner_body), (lambda pbody: (pparams := hydra.lib.lists.map((lambda v1: hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, inner_env, v1)), params), hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : hydra.lib.flows.pure(make_uncurried_lambda(pparams, pbody))), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_binding_as_assignment(False, inner_env, v1)), bindings), (lambda pbinding_exprs: (pbinding_star_exprs := hydra.lib.lists.map((lambda ne: cast(hydra.ext.python.syntax.StarNamedExpression, hydra.ext.python.syntax.StarNamedExpressionSimple(ne))), pbinding_exprs), pbody_star_expr := hydra.ext.python.utils.py_expression_to_py_star_named_expression(pbody), tuple_elements := hydra.lib.lists.concat2(pbinding_star_exprs, (pbody_star_expr,)), tuple_expr := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(tuple_elements)))), index_value := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(hydra.lib.lists.length(bindings))))))), indexed_expr := hydra.ext.python.utils.primary_with_expression_slices(hydra.ext.python.utils.py_expression_to_py_primary(tuple_expr), (index_value,)), hydra.lib.flows.pure(make_uncurried_lambda(pparams, hydra.ext.python.utils.py_primary_to_py_expression(indexed_expr))))[6])))))[1])))[4]))
        
        case hydra.core.FunctionPrimitive(value=name):
            return encode_variable(env, name, ())
        
        case hydra.core.FunctionElimination(value=e):
            return _hoist_hydra_ext_python_coder_encode_function_1(env, e)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_term_inline(env: hydra.ext.python.helpers.PythonEnvironment, no_cast: bool, term: hydra.core.Term) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
    r"""Encode a term to a Python expression (inline form)."""
    
    def encode(t: hydra.core.Term) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
        return encode_term_inline(env, False, t)
    def strip_type_apps(t: hydra.core.Term) -> hydra.core.Term:
        match t:
            case hydra.core.TermAnnotated(value=ann):
                return strip_type_apps(ann.body)
            
            case hydra.core.TermTypeApplication(value=ta):
                return strip_type_apps(ta.body)
            
            case _:
                return t
    def with_cast(pyexp: hydra.ext.python.syntax.Expression) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Expression]:
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(no_cast, env.skip_casts), (lambda : hydra.lib.flows.pure(pyexp)), (lambda : (tc := env.type_context, hydra.lib.flows.with_default(pyexp, hydra.lib.flows.bind(hydra.checking.type_of(tc, (), term), (lambda typ: hydra.lib.flows.bind(encode_type(env, typ), (lambda pytyp: hydra.lib.flows.bind(update_meta((lambda m: hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, True, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var))), (lambda unit_: hydra.lib.flows.pure(hydra.ext.python.utils.cast_to(pytyp, pyexp))))))))))[1]))
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermApplication(value=app):
            return encode_application(env, app)
        
        case hydra.core.TermEither(value=et):
            return hydra.lib.eithers.either((lambda t1: hydra.lib.flows.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Left")), (pyexp,)))))), (lambda t1: hydra.lib.flows.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Right")), (pyexp,)))))), et)
        
        case hydra.core.TermFunction(value=f):
            return encode_function(env, f)
        
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def bindings() -> frozenlist[hydra.core.Binding]:
                return lt.bindings
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return lt.body
            return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings()), (lambda : encode_term_inline(env, False, body())), (lambda : with_let_inline(env, lt, (lambda inner_env: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_binding_as_assignment(False, inner_env, v1)), bindings()), (lambda pbinding_exprs: hydra.lib.flows.bind(encode_term_inline(inner_env, False, body()), (lambda pbody: (pbinding_star_exprs := hydra.lib.lists.map((lambda ne: cast(hydra.ext.python.syntax.StarNamedExpression, hydra.ext.python.syntax.StarNamedExpressionSimple(ne))), pbinding_exprs), pbody_star_expr := hydra.ext.python.utils.py_expression_to_py_star_named_expression(pbody), tuple_elements := hydra.lib.lists.concat2(pbinding_star_exprs, (pbody_star_expr,)), tuple_expr := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(tuple_elements)))), index_value := hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomNumber(cast(hydra.ext.python.syntax.Number, hydra.ext.python.syntax.NumberInteger(hydra.lib.literals.int32_to_bigint(hydra.lib.lists.length(bindings()))))))), indexed_expr := hydra.ext.python.utils.primary_with_expression_slices(hydra.ext.python.utils.py_expression_to_py_primary(tuple_expr), (index_value,)), hydra.lib.flows.pure(hydra.ext.python.utils.py_primary_to_py_expression(indexed_expr)))[6]))))))))
        
        case hydra.core.TermList(value=terms):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), terms), (lambda py_exprs: hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple(hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_expression_to_py_star_named_expression(x1)), py_exprs))))))))
        
        case hydra.core.TermLiteral(value=lit):
            return encode_literal(lit)
        
        case hydra.core.TermMap(value=m):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.flows.bind(encode(k), (lambda py_k: hydra.lib.flows.bind(encode(v), (lambda py_v: hydra.lib.flows.pure(cast(hydra.ext.python.syntax.DoubleStarredKvpair, hydra.ext.python.syntax.DoubleStarredKvpairPair(hydra.ext.python.syntax.Kvpair(py_k, py_v)))))))))[2]), hydra.lib.maps.to_list(m)), (lambda pairs: hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("FrozenDict")), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomDict(hydra.ext.python.syntax.Dict(pairs)))),)))))
        
        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe(hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Nothing")), ())), (lambda t1: hydra.lib.flows.bind(encode(t1), (lambda pyexp: with_cast(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("Just")), (pyexp,)))))), mt)
        
        case hydra.core.TermPair(value=p):
            @lru_cache(1)
            def t1() -> hydra.core.Term:
                return hydra.lib.pairs.first(p)
            @lru_cache(1)
            def t2() -> hydra.core.Term:
                return hydra.lib.pairs.second(p)
            return hydra.lib.flows.bind(encode(t1()), (lambda py_expr1: hydra.lib.flows.bind(encode(t2()), (lambda py_expr2: hydra.lib.flows.pure(hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomTuple(hydra.ext.python.syntax.Tuple((hydra.ext.python.utils.py_expression_to_py_star_named_expression(py_expr1), hydra.ext.python.utils.py_expression_to_py_star_named_expression(py_expr2)))))))))))
        
        case hydra.core.TermRecord(value=r):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return r.type_name
            @lru_cache(1)
            def fields() -> frozenlist[hydra.core.Field]:
                return r.fields
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda fld: encode(fld.term)), fields()), (lambda pargs: hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name_qualified(env, tname())), pargs))))
        
        case hydra.core.TermSet(value=s):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode(x1)), hydra.lib.sets.to_list(s)), (lambda py_els: hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.syntax.Name("frozenset")), (hydra.ext.python.utils.py_atom_to_py_expression(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomSet(hydra.ext.python.syntax.Set(hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_expression_to_py_star_named_expression(x1)), py_els))))),)))))
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return ta.body
            return hydra.lib.flows.bind(encode_term_inline(env, True, strip_type_apps(body())), (lambda pybase: with_cast(pybase)))
        
        case hydra.core.TermTypeLambda(value=tl):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return tl.body
            return with_type_lambda(env, tl, (lambda env2: encode_term_inline(env2, no_cast, body())))
        
        case hydra.core.TermUnion(value=inj):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return inj.type_name
            @lru_cache(1)
            def field() -> hydra.core.Field:
                return inj.field
            return hydra.lib.flows.bind(in_graph_context(hydra.schemas.require_union_type(tname())), (lambda rt: hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(rt), (lambda : hydra.lib.flows.pure(hydra.ext.python.utils.project_from_expression(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.names.encode_name_qualified(env, tname())), hydra.ext.python.names.encode_enum_value(env, field().name)))), (lambda : (fname := field().name, (ftypes := rt.fields, (is_unit_variant := hydra.lib.maybes.maybe(False, (lambda ft: hydra.schemas.is_unit_type(hydra.rewriting.deannotate_type(ft.type))), hydra.lib.lists.find((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), ftypes)), hydra.lib.flows.bind(hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.schemas.is_unit_term(field().term), is_unit_variant), (lambda : hydra.lib.flows.pure(())), (lambda : hydra.lib.flows.bind(encode(field().term), (lambda parg: hydra.lib.flows.pure((parg,)))))), (lambda args: hydra.lib.flows.bind(update_meta((lambda v1: set_meta_uses_cast(True, v1))), (lambda unit_: hydra.lib.flows.pure(hydra.ext.python.utils.cast_to(hydra.ext.python.names.type_variable_reference(env, tname()), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.variant_name(True, env, tname(), fname)), args))))))))[1])[1])[1]))))
        
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.utils.py_none))
        
        case hydra.core.TermVariable(value=name):
            return encode_variable(env, name, ())
        
        case hydra.core.TermWrap(value=wrapped):
            @lru_cache(1)
            def tname() -> hydra.core.Name:
                return wrapped.type_name
            @lru_cache(1)
            def inner() -> hydra.core.Term:
                return wrapped.body
            return hydra.lib.flows.bind(encode(inner()), (lambda parg: hydra.lib.flows.pure(hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name_qualified(env, tname())), (parg,)))))
        
        case _:
            raise TypeError("Unsupported Term")

def is_variant_unit_type(row_type: hydra.core.RowType, field_name: hydra.core.Name) -> bool:
    r"""Check if a variant field has unit type."""
    
    @lru_cache(1)
    def fields() -> frozenlist[hydra.core.FieldType]:
        return row_type.fields
    @lru_cache(1)
    def mfield() -> Maybe[hydra.core.FieldType]:
        return hydra.lib.lists.find((lambda ft: hydra.lib.equality.equal(ft.name, field_name)), fields())
    return hydra.lib.maybes.from_maybe(False, hydra.lib.maybes.map((lambda ft: hydra.schemas.is_unit_type(hydra.rewriting.deannotate_type(ft.type))), mfield()))

def enum_variant_pattern(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name) -> hydra.ext.python.syntax.ClosedPattern:
    r"""Create a value pattern for an enum variant."""
    
    return cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternValue(hydra.ext.python.syntax.ValuePattern(hydra.ext.python.syntax.Attribute((hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, type_name), hydra.ext.python.names.encode_enum_value(env, field_name))))))

def variant_closed_pattern(env: hydra.ext.python.helpers.PythonEnvironment, type_name: hydra.core.Name, field_name: hydra.core.Name, row_type: T0, is_enum: bool, var_name: hydra.core.Name, should_capture: bool) -> hydra.ext.python.syntax.ClosedPattern:
    return hydra.lib.logic.if_else(is_enum, (lambda : enum_variant_pattern(env, type_name, field_name)), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(should_capture), (lambda : class_variant_pattern_unit(env, type_name, field_name)), (lambda : class_variant_pattern_with_capture(env, type_name, field_name, var_name)))))

def encode_case_block(env: hydra.ext.python.helpers.PythonEnvironment, tname: hydra.core.Name, row_type: hydra.core.RowType, is_enum: bool, encode_body: Callable[[hydra.ext.python.helpers.PythonEnvironment, hydra.core.Term], hydra.compute.Flow[T0, frozenlist[hydra.ext.python.syntax.Statement]]], field: hydra.core.Field) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.CaseBlock]:
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field.name
    @lru_cache(1)
    def fterm() -> hydra.core.Term:
        return field.term
    def _hoist_body_1(v1: hydra.core.Function) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.CaseBlock]:
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                @lru_cache(1)
                def v() -> hydra.core.Name:
                    return lam.parameter
                @lru_cache(1)
                def raw_body() -> hydra.core.Term:
                    return lam.body
                @lru_cache(1)
                def is_unit_variant() -> bool:
                    return is_variant_unit_type(row_type, fname())
                @lru_cache(1)
                def effective_body() -> hydra.core.Term:
                    return hydra.lib.logic.if_else(is_unit_variant(), (lambda : eliminate_unit_var(v(), raw_body())), (lambda : raw_body()))
                @lru_cache(1)
                def should_capture() -> bool:
                    return hydra.lib.logic.not_(hydra.lib.logic.or_(is_unit_variant(), hydra.lib.logic.or_(hydra.rewriting.is_free_variable_in_term(v(), raw_body()), hydra.schemas.is_unit_term(raw_body()))))
                return hydra.lib.flows.bind(hydra.lib.flows.pure(python_environment_set_type_context(hydra.schemas.extend_type_context_for_lambda(python_environment_get_type_context(env), lam), env)), (lambda env2: (pattern := variant_closed_pattern(env2, tname, fname(), row_type, is_enum, v(), should_capture()), hydra.lib.flows.bind(encode_body(env2, effective_body()), (lambda stmts: (py_body := hydra.ext.python.utils.indented_block(Nothing(), (stmts,)), hydra.lib.flows.pure(hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.py_closed_pattern_to_py_patterns(pattern), Nothing(), py_body)))[1])))[1]))
            
            case _:
                raise TypeError("Unsupported Function")
    match hydra.rewriting.deannotate_term(fterm()):
        case hydra.core.TermFunction(value=f):
            return _hoist_body_1(f)
        
        case _:
            raise TypeError("Unsupported Term")

def encode_default_case_block(encode_term: Callable[[T0], hydra.compute.Flow[T1, hydra.ext.python.syntax.Expression]], is_full: bool, mdflt: Maybe[T0], tname: hydra.core.Name) -> hydra.compute.Flow[T1, frozenlist[hydra.ext.python.syntax.CaseBlock]]:
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(hydra.lib.logic.if_else(is_full, (lambda : hydra.ext.python.utils.raise_assertion_error("Unreachable: all variants handled")), (lambda : hydra.ext.python.utils.raise_type_error(hydra.lib.strings.cat2("Unsupported ", hydra.names.local_name_of(tname)))))), (lambda d: hydra.lib.flows.bind(encode_term(d), (lambda pyexpr: hydra.lib.flows.pure(hydra.ext.python.utils.return_single(pyexpr))))), mdflt), (lambda stmt: (patterns := hydra.ext.python.utils.py_closed_pattern_to_py_patterns(cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternWildcard())), body := hydra.ext.python.utils.indented_block(Nothing(), ((stmt,),)), hydra.lib.flows.pure((hydra.ext.python.syntax.CaseBlock(patterns, Nothing(), body),)))[2]))

def is_cases_full(row_type: hydra.core.RowType, cases_: frozenlist[T0]) -> bool:
    @lru_cache(1)
    def num_cases() -> int:
        return hydra.lib.lists.length(cases_)
    @lru_cache(1)
    def num_fields() -> int:
        return hydra.lib.lists.length(row_type.fields)
    return hydra.lib.logic.not_(hydra.lib.equality.lt(num_cases(), num_fields()))

def with_bindings(v1: frozenlist[hydra.core.Binding], v2: hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, T0]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, T0]:
    return hydra.coder_utils.with_graph_bindings((lambda x1: py_graph_graph(x1)), (lambda x1, x2: make_py_graph(x1, x2)), (lambda x1: py_graph_metadata(x1)), v1, v2)

@lru_cache(1)
def lru_cache_decorator() -> hydra.ext.python.syntax.NamedExpression:
    r"""Decorator for @lru_cache(1) to memoize zero-argument function results."""
    
    return cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.function_call(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("lru_cache"))))), (py_int(1),))))

def set_meta_uses_lru_cache(b: bool, m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(m.namespaces, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, b, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def use_inline_type_params_for(version: hydra.ext.python.helpers.PythonVersion) -> bool:
    r"""Version-aware inline type parameters."""
    
    return hydra.lib.equality.equal(version, hydra.ext.python.helpers.PythonVersion.PYTHON312)

@lru_cache(1)
def use_inline_type_params() -> bool:
    r"""Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code."""
    
    return use_inline_type_params_for(hydra.ext.python.utils.target_python_version)

def extend_env_with_lambda_params(env: hydra.ext.python.helpers.PythonEnvironment, term: hydra.core.Term) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Extend environment with lambda parameters from a term."""
    
    def go(e: hydra.ext.python.helpers.PythonEnvironment, t: hydra.core.Term) -> hydra.ext.python.helpers.PythonEnvironment:
        def _hoist_go_1(e: hydra.ext.python.helpers.PythonEnvironment, v1: hydra.core.Function) -> hydra.ext.python.helpers.PythonEnvironment:
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    @lru_cache(1)
                    def new_tc() -> hydra.typing.TypeContext:
                        return hydra.schemas.extend_type_context_for_lambda(python_environment_get_type_context(e), lam)
                    @lru_cache(1)
                    def new_env() -> hydra.ext.python.helpers.PythonEnvironment:
                        return python_environment_set_type_context(new_tc(), e)
                    return go(new_env(), lam.body)
                
                case _:
                    return e
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_go_1(e, f)
            
            case _:
                return e
    return go(env, term)

def extract_case_elimination(term: hydra.core.Term) -> Maybe[hydra.core.CaseStatement]:
    def _hoist_hydra_ext_python_coder_extract_case_elimination_1(v1: hydra.core.Elimination) -> Maybe[hydra.core.CaseStatement]:
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                return Just(cs)
            
            case _:
                return Nothing()
    def _hoist_hydra_ext_python_coder_extract_case_elimination_2(v1: hydra.core.Function) -> Maybe[hydra.core.CaseStatement]:
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
    
    def go(params: frozenlist[hydra.core.Name], t: hydra.core.Term) -> tuple[frozenlist[hydra.core.Name], hydra.core.Term]:
        def _hoist_go_1(params: frozenlist[hydra.core.Name], t: hydra.core.Term, v1: hydra.core.Function) -> tuple[frozenlist[hydra.core.Name], hydra.core.Term]:
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

def is_case_statement_application(term: hydra.core.Term) -> Maybe[tuple[hydra.core.Name, tuple[Maybe[hydra.core.Term], tuple[frozenlist[hydra.core.Field], hydra.core.Term]]]]:
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
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(args()), 1)), (lambda : Nothing()), (lambda : (arg := hydra.lib.lists.head(args()), (_hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[2])[1]))

def encode_binding_as(env: hydra.ext.python.helpers.PythonEnvironment, binding: hydra.core.Binding) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a binding as a Python statement (function definition or assignment)."""
    
    @lru_cache(1)
    def name1() -> hydra.core.Name:
        return binding.name
    @lru_cache(1)
    def term1() -> hydra.core.Term:
        return binding.term
    @lru_cache(1)
    def mts() -> Maybe[hydra.core.TypeScheme]:
        return binding.type
    @lru_cache(1)
    def fname() -> hydra.ext.python.syntax.Name:
        return hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.LOWER_SNAKE, env, name1())
    return hydra.lib.maybes.maybe((gathered := gather_lambdas(term1()), (lambda_params := hydra.lib.pairs.first(gathered), (inner_body := hydra.lib.pairs.second(gathered), (mcsa := is_case_statement_application(inner_body), hydra.lib.maybes.maybe((mcs := extract_case_elimination(term1()), hydra.lib.maybes.maybe(hydra.lib.flows.map((lambda stmts: hydra.lib.lists.head(stmts)), encode_term_multiline(env, term1())), (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.flows.bind(in_graph_context(hydra.schemas.require_union_type(tname)), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), inner_param := hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("x"), Nothing()), param := hydra.ext.python.syntax.ParamNoDefault(inner_param, Nothing()), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((param,), (), Nothing()))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_case_block(env, tname, rt, is_enum, (lambda e, t: encode_term_multiline(e, t)), v1)), cases_), (lambda py_cases: hydra.lib.flows.bind(encode_default_case_block((lambda t: encode_term_inline(env, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.syntax.Name("x")))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), hydra.lib.flows.pure(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[5])))[3]), mcs))[1], (lambda csa: hydra.lib.logic.if_else(hydra.lib.lists.null(lambda_params), (lambda : (mcs := extract_case_elimination(term1()), hydra.lib.maybes.maybe(hydra.lib.flows.map((lambda stmts: hydra.lib.lists.head(stmts)), encode_term_multiline(env, term1())), (lambda cs: (tname := cs.type_name, dflt := cs.default, cases_ := cs.cases, hydra.lib.flows.bind(in_graph_context(hydra.schemas.require_union_type(tname)), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), inner_param := hydra.ext.python.syntax.Param(hydra.ext.python.syntax.Name("x"), Nothing()), param := hydra.ext.python.syntax.ParamNoDefault(inner_param, Nothing()), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters((param,), (), Nothing()))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_case_block(env, tname, rt, is_enum, (lambda e, t: encode_term_multiline(e, t)), v1)), cases_), (lambda py_cases: hydra.lib.flows.bind(encode_default_case_block((lambda t: encode_term_inline(env, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(hydra.ext.python.syntax.Name("x")))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), hydra.lib.flows.pure(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[5])))[3]), mcs))[1]), (lambda : (tname := hydra.lib.pairs.first(csa), (rest1 := hydra.lib.pairs.second(csa), (dflt := hydra.lib.pairs.first(rest1), (rest2 := hydra.lib.pairs.second(rest1), (cases_ := hydra.lib.pairs.first(rest2), hydra.lib.flows.bind(in_graph_context(hydra.schemas.require_union_type(tname)), (lambda rt: (is_enum := hydra.schemas.is_enum_row_type(rt), is_full := is_cases_full(rt, cases_), captured_params := hydra.lib.lists.map((lambda n: hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, n), Nothing()), Nothing())), lambda_params), match_arg_name := hydra.ext.python.syntax.Name("x"), match_param := hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(match_arg_name, Nothing()), Nothing()), all_params := hydra.lib.lists.concat2(captured_params, (match_param,)), params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(all_params, (), Nothing()))), env_with_params := extend_env_with_lambda_params(env, term1()), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_case_block(env_with_params, tname, rt, is_enum, (lambda e, t: encode_term_multiline(e, t)), v1)), cases_), (lambda py_cases: hydra.lib.flows.bind(encode_default_case_block((lambda t: encode_term_inline(env_with_params, False, t)), is_full, dflt, tname), (lambda py_dflt: (subj := cast(hydra.ext.python.syntax.SubjectExpression, hydra.ext.python.syntax.SubjectExpressionSimple(cast(hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.NamedExpressionSimple(hydra.ext.python.utils.py_name_to_py_expression(match_arg_name))))), all_cases := hydra.lib.lists.concat2(py_cases, py_dflt), match_stmt := cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementMatch(hydra.ext.python.syntax.MatchStatement(subj, all_cases))))), body := hydra.ext.python.utils.indented_block(Nothing(), ((match_stmt,),)), func_def_raw := hydra.ext.python.syntax.FunctionDefRaw(False, fname(), (), Just(params), Nothing(), Nothing(), body), hydra.lib.flows.pure(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(Nothing(), func_def_raw)))))))[5])))))[8])))[1])[1])[1])[1])[1]))), mcsa))[1])[1])[1])[1], (lambda ts: hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_term_description(term1())), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), encode_term_assignment(env, name1(), term1(), ts, norm_comment))[1]))), mts())

def encode_function_definition(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], body: hydra.core.Term, doms: frozenlist[hydra.core.Type], mcod: Maybe[hydra.core.Type], comment: Maybe[str], prefixes: frozenlist[hydra.ext.python.syntax.Statement]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a function definition with parameters and body."""
    
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda pair: (arg_name := hydra.lib.pairs.first(pair), typ := hydra.lib.pairs.second(pair), hydra.lib.flows.bind(encode_type(env, typ), (lambda py_typ: hydra.lib.flows.pure(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, arg_name), Just(hydra.ext.python.syntax.Annotation(py_typ))), Nothing())))))[2]), hydra.lib.lists.zip(args, doms)), (lambda py_args: (py_params := cast(hydra.ext.python.syntax.Parameters, hydra.ext.python.syntax.ParametersParamNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(py_args, (), Nothing()))), hydra.lib.flows.bind(encode_term_multiline(env, body), (lambda stmts: (block := hydra.ext.python.utils.indented_block(comment, (hydra.lib.lists.concat2(prefixes, stmts),)), hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(Nothing()), (lambda cod: hydra.lib.flows.bind(encode_type(env, cod), (lambda pytyp: hydra.lib.flows.pure(Just(pytyp))))), mcod), (lambda mreturn_type: (py_tparams := hydra.lib.logic.if_else(use_inline_type_params(), (lambda : hydra.lib.lists.map((lambda arg_: hydra.ext.python.utils.py_name_to_py_type_parameter(hydra.ext.python.names.encode_type_variable(arg_))), tparams)), (lambda : ())), is_thunk := hydra.lib.lists.null(args), m_decorators := hydra.lib.logic.if_else(is_thunk, (lambda : Just(hydra.ext.python.syntax.Decorators((lru_cache_decorator(),)))), (lambda : Nothing())), hydra.lib.flows.bind(hydra.lib.logic.if_else(is_thunk, (lambda : update_meta((lambda v1: set_meta_uses_lru_cache(True, v1)))), (lambda : hydra.lib.flows.pure(None))), (lambda unit1: (py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env, name), hydra.lib.flows.pure(cast(hydra.ext.python.syntax.Statement, hydra.ext.python.syntax.StatementCompound(cast(hydra.ext.python.syntax.CompoundStatement, hydra.ext.python.syntax.CompoundStatementFunction(hydra.ext.python.syntax.FunctionDefinition(m_decorators, hydra.ext.python.syntax.FunctionDefRaw(False, py_name, py_tparams, Just(py_params), mreturn_type, Nothing(), block))))))))[1])))[3])))[1])))[1]))

def encode_term_assignment(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, term: hydra.core.Term, ts: hydra.core.TypeScheme, comment: Maybe[str]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a term assignment to a Python statement."""
    
    return hydra.lib.flows.bind(analyze_python_function(env, term), (lambda fs: (tparams := fs.type_params, params := fs.params, bindings := fs.bindings, body := fs.body, doms := fs.domains, mcod := fs.codomain, env2 := fs.environment, tc := env2.type_context, binding := hydra.core.Binding(name, term, Just(ts)), is_complex := hydra.coder_utils.is_complex_binding(tc, binding), hydra.lib.logic.if_else(is_complex, (lambda : with_bindings(bindings, hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_binding_as(env2, v1)), bindings), (lambda binding_stmts: encode_function_definition(env2, name, tparams, params, body, doms, mcod, comment, binding_stmts))))), (lambda : hydra.lib.flows.bind(encode_term_inline(env2, False, body), (lambda body_expr: (py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.LOWER_SNAKE, env2, name), hydra.lib.flows.pure(hydra.ext.python.utils.annotated_statement(comment, hydra.ext.python.utils.assignment_statement(py_name, body_expr))))[1])))))[10]))

def encode_term_multiline(env: hydra.ext.python.helpers.PythonEnvironment, term: hydra.core.Term) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode a term to a list of statements with return as final statement."""
    
    @lru_cache(1)
    def dflt_logic() -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[hydra.ext.python.syntax.Statement]]:
        return hydra.lib.flows.bind(analyze_python_function(env, term), (lambda fs: (params := fs.params, bindings := fs.bindings, inner_body := fs.body, env2 := fs.environment, hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(params)), (lambda : hydra.lib.flows.fail("Functions currently unsupported in this context")), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : hydra.lib.flows.bind(encode_term_inline(env, False, term), (lambda expr: hydra.lib.flows.pure((hydra.ext.python.utils.return_single(expr),))))), (lambda : with_bindings(bindings, hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_binding_as(env2, v1)), bindings), (lambda binding_stmts: hydra.lib.flows.bind(encode_term_multiline(env2, inner_body), (lambda body_stmts: hydra.lib.flows.pure(hydra.lib.lists.concat2(binding_stmts, body_stmts))))))))))))[4]))
    @lru_cache(1)
    def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        return hydra.coder_utils.gather_applications(term)
    @lru_cache(1)
    def args() -> frozenlist[hydra.core.Term]:
        return hydra.lib.pairs.first(gathered())
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.pairs.second(gathered())
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args()), 1), (lambda : (arg := hydra.lib.lists.head(args()), (_hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[2])[1]), (lambda : dflt_logic()))

def encode_bindings_as_defs(env: T0, encode_binding: Callable[[T0, T1], hydra.compute.Flow[T2, T3]], bindings: frozenlist[T1]) -> hydra.compute.Flow[T2, frozenlist[T3]]:
    return hydra.lib.flows.map_list((lambda v1: encode_binding(env, v1)), bindings)

def encode_name_constants(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Generate name constants for a type."""
    
    def to_stmt(pair: tuple[hydra.ext.python.syntax.Name, hydra.core.Name]) -> hydra.ext.python.syntax.Statement:
        return hydra.ext.python.utils.assignment_statement(hydra.lib.pairs.first(pair), hydra.ext.python.utils.function_call(hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.encode_name(True, hydra.util.CaseConvention.PASCAL, env, hydra.core.Name("hydra.core.Name"))), (hydra.ext.python.utils.double_quoted_string(hydra.lib.pairs.second(pair).value),)))
    @lru_cache(1)
    def name_pair() -> tuple[hydra.ext.python.syntax.Name, hydra.core.Name]:
        return (hydra.ext.python.names.encode_constant_for_type_name(env, name), name)
    def field_pair(field: hydra.core.FieldType) -> tuple[hydra.ext.python.syntax.Name, hydra.core.Name]:
        return (hydra.ext.python.names.encode_constant_for_field_name(env, name, field.name), field.name)
    def field_pairs(t: hydra.core.Type) -> frozenlist[tuple[hydra.ext.python.syntax.Name, hydra.core.Name]]:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeForall(value=ft):
                return field_pairs(ft.body)
            
            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.lists.map((lambda x1: field_pair(x1)), rt.fields)
            
            case hydra.core.TypeUnion(value=rt2):
                return hydra.lib.lists.map((lambda x1: field_pair(x1)), rt2.fields)
            
            case hydra.core.TypeAnnotated():
                return ()
            
            case hydra.core.TypeApplication():
                return ()
            
            case hydra.core.TypeFunction():
                return ()
            
            case hydra.core.TypeList():
                return ()
            
            case hydra.core.TypeLiteral():
                return ()
            
            case hydra.core.TypeMap():
                return ()
            
            case hydra.core.TypeMaybe():
                return ()
            
            case hydra.core.TypeEither():
                return ()
            
            case hydra.core.TypePair():
                return ()
            
            case hydra.core.TypeSet():
                return ()
            
            case hydra.core.TypeUnit():
                return ()
            
            case hydra.core.TypeVariable():
                return ()
            
            case hydra.core.TypeWrap():
                return ()
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.lists.map((lambda x1: to_stmt(x1)), hydra.lib.lists.cons(name_pair(), field_pairs(typ)))

def encode_field_type(env: hydra.ext.python.helpers.PythonEnvironment, field_type: hydra.core.FieldType) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a field type for record definitions (field: type annotation)."""
    
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field_type.name
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return field_type.type
    return hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_type_description(ftype())), (lambda comment: (py_name := cast(hydra.ext.python.syntax.SingleTarget, hydra.ext.python.syntax.SingleTargetName(hydra.ext.python.names.encode_field_name(env, fname()))), hydra.lib.flows.bind(encode_type(env, ftype()), (lambda py_type: (annotated_py_type := hydra.ext.python.utils.annotated_expression(comment, py_type), hydra.lib.flows.pure(hydra.ext.python.utils.py_assignment_to_py_statement(cast(hydra.ext.python.syntax.Assignment, hydra.ext.python.syntax.AssignmentTyped(hydra.ext.python.syntax.TypedAssignment(py_name, annotated_py_type, Nothing()))))))[1])))[1]))

def generic_arg(tparam_list: frozenlist[hydra.core.Name]) -> Maybe[hydra.ext.python.syntax.Expression]:
    r"""Create Generic[...] argument expression for class definition."""
    
    return hydra.lib.logic.if_else(hydra.lib.lists.null(tparam_list), (lambda : Nothing()), (lambda : Just(hydra.ext.python.utils.py_primary_to_py_expression(hydra.ext.python.utils.primary_with_expression_slices(cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.syntax.Name("Generic"))))), hydra.lib.lists.map((lambda n: cast(hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ExpressionSimple(hydra.ext.python.syntax.Disjunction((hydra.ext.python.syntax.Conjunction((cast(hydra.ext.python.syntax.Inversion, hydra.ext.python.syntax.InversionSimple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(Nothing(), hydra.ext.python.syntax.BitwiseXor(Nothing(), hydra.ext.python.syntax.BitwiseAnd(Nothing(), hydra.ext.python.syntax.ShiftExpression(Nothing(), hydra.ext.python.syntax.Sum(Nothing(), hydra.ext.python.syntax.Term(Nothing(), cast(hydra.ext.python.syntax.Factor, hydra.ext.python.syntax.FactorSimple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(False, cast(hydra.ext.python.syntax.Primary, hydra.ext.python.syntax.PrimarySimple(cast(hydra.ext.python.syntax.Atom, hydra.ext.python.syntax.AtomName(hydra.ext.python.names.encode_type_variable(n)))))), Nothing()))))))))), ()))),)),))))), tparam_list))))))

def encode_record_type(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, row_type: hydra.core.RowType, comment: Maybe[str]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a record type as a Python dataclass."""
    
    @lru_cache(1)
    def tfields() -> frozenlist[hydra.core.FieldType]:
        return row_type.fields
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_field_type(env, v1)), tfields()), (lambda py_fields: (body := hydra.ext.python.utils.indented_block(comment, (py_fields,)), bound_vars := env.bound_type_variables, tparam_list := hydra.lib.pairs.first(bound_vars), m_generic_arg := generic_arg(tparam_list), args := hydra.lib.maybes.maybe(Nothing(), (lambda a: Just(hydra.ext.python.utils.py_expressions_to_py_args((a,)))), m_generic_arg), decs := Just(hydra.ext.python.syntax.Decorators((dataclass_decorator(),))), py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), no_type_params := (), hydra.lib.flows.pure(hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(decs, py_name, no_type_params, args, body))))[8]))

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

def encode_enum_value_assignment(env: hydra.ext.python.helpers.PythonEnvironment, field_type: hydra.core.FieldType) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode an enum value assignment statement with optional comment."""
    
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field_type.name
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return field_type.type
    return hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_type_description(ftype())), (lambda mcomment: (py_name := hydra.ext.python.names.encode_enum_value(env, fname()), fname_str := fname().value, py_value := hydra.ext.python.utils.double_quoted_string(fname_str), assign_stmt := hydra.ext.python.utils.assignment_statement(py_name, py_value), hydra.lib.flows.pure(hydra.lib.maybes.maybe((assign_stmt,), (lambda c: (assign_stmt, hydra.ext.python.utils.py_expression_to_py_statement(hydra.ext.python.utils.triple_quoted_string(c)))), mcomment)))[4]))

def encode_type_quoted(env: hydra.ext.python.helpers.PythonEnvironment, typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Expression]:
    return hydra.lib.flows.bind(encode_type(env, typ), (lambda pytype: hydra.lib.flows.pure(hydra.lib.logic.if_else(hydra.lib.sets.null(hydra.rewriting.free_variables_in_type(typ)), (lambda : pytype), (lambda : hydra.ext.python.utils.double_quoted_string(hydra.serialization.print_expr(hydra.ext.python.serde.encode_expression(pytype))))))))

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

def encode_union_field(env: hydra.ext.python.helpers.PythonEnvironment, union_name: hydra.core.Name, field_type: hydra.core.FieldType) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, hydra.ext.python.syntax.Statement]:
    r"""Encode a union field as a variant class."""
    
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field_type.name
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return field_type.type
    return hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_type_description(ftype())), (lambda fcomment: (is_unit := hydra.lib.equality.equal(hydra.rewriting.deannotate_type(ftype()), cast(hydra.core.Type, hydra.core.TypeUnit())), var_name := hydra.ext.python.names.variant_name(False, env, union_name, fname()), tparam_names := find_type_params(env, ftype()), tparam_py_names := hydra.lib.lists.map((lambda x1: hydra.ext.python.names.encode_type_variable(x1)), tparam_names), field_params := hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_name_to_py_type_parameter(x1)), tparam_py_names), body := hydra.lib.logic.if_else(is_unit, (lambda : hydra.ext.python.utils.indented_block(fcomment, (hydra.ext.python.utils.unit_variant_methods(var_name),))), (lambda : hydra.ext.python.utils.indented_block(fcomment, ()))), hydra.lib.flows.bind(hydra.lib.logic.if_else(is_unit, (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(encode_type_quoted(env, ftype()), (lambda quoted_type: hydra.lib.flows.pure(Just(variant_args(quoted_type, ()))))))), (lambda margs: hydra.lib.flows.pure(hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), var_name, field_params, margs, body))))))[6]))

def encode_union_field_alt(env: hydra.ext.python.helpers.PythonEnvironment, union_name: hydra.core.Name, field_type: hydra.core.FieldType) -> hydra.ext.python.syntax.Primary:
    r"""Encode a union field as a primary expression for | alternatives."""
    
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field_type.name
    @lru_cache(1)
    def ftype() -> hydra.core.Type:
        return field_type.type
    @lru_cache(1)
    def tparam_names() -> frozenlist[hydra.core.Name]:
        return find_type_params(env, ftype())
    @lru_cache(1)
    def tparams() -> frozenlist[hydra.ext.python.syntax.Name]:
        return hydra.lib.lists.map((lambda x1: hydra.ext.python.names.encode_type_variable(x1)), tparam_names())
    @lru_cache(1)
    def name_prim() -> hydra.ext.python.syntax.Primary:
        return hydra.ext.python.utils.py_name_to_py_primary(hydra.ext.python.names.variant_name(False, env, union_name, fname()))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(tparams()), (lambda : name_prim()), (lambda : (tparam_exprs := hydra.lib.lists.map((lambda x1: hydra.ext.python.utils.py_name_to_py_expression(x1)), tparams()), hydra.ext.python.utils.primary_with_expression_slices(name_prim(), tparam_exprs))[1]))

def union_type_statements_for(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.ext.python.syntax.Name, tparams: frozenlist[hydra.ext.python.syntax.TypeParameter], mcomment: Maybe[str], tyexpr: hydra.ext.python.syntax.Expression) -> frozenlist[hydra.ext.python.syntax.Statement]:
    r"""Version-aware union type statement generation."""
    
    return hydra.lib.logic.if_else(use_inline_type_params_for(env.version), (lambda : (hydra.ext.python.utils.type_alias_statement(name, tparams, mcomment, tyexpr),)), (lambda : hydra.ext.python.utils.union_type_class_statements310(name, mcomment, tyexpr)))

def encode_union_type(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, row_type: hydra.core.RowType, comment: Maybe[str]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode a union type as an enum (for unit-only fields) or variant classes."""
    
    @lru_cache(1)
    def tfields() -> frozenlist[hydra.core.FieldType]:
        return row_type.fields
    return hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(row_type), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_enum_value_assignment(env, v1)), tfields()), (lambda vals: (body := hydra.ext.python.utils.indented_block(comment, vals), enum_name := hydra.ext.python.syntax.Name("Enum"), args := Just(hydra.ext.python.utils.py_expressions_to_py_args((hydra.ext.python.utils.py_name_to_py_expression(enum_name),))), py_name := hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), hydra.lib.flows.pure((hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), py_name, (), args, body)),)))[4]))), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: encode_union_field(env, name, v1)), tfields()), (lambda field_stmts: (tparams := environment_type_parameters(env), union_alts := hydra.lib.lists.map((lambda v1: encode_union_field_alt(env, name, v1)), tfields()), union_stmts := union_type_statements_for(env, hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), tparams, comment, hydra.ext.python.utils.or_expression(union_alts)), hydra.lib.flows.pure(hydra.lib.lists.concat2(field_stmts, union_stmts)))[3]))))

def encode_wrapped_type(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> hydra.compute.Flow[T0, hydra.ext.python.syntax.Statement]:
    @lru_cache(1)
    def tparam_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(env.bound_type_variables)
    return hydra.lib.flows.bind(encode_type_quoted(env, typ), (lambda ptype_quoted: (body := hydra.ext.python.utils.indented_block(comment, ()), hydra.lib.flows.pure(hydra.ext.python.utils.py_class_definition_to_py_statement(hydra.ext.python.syntax.ClassDefinition(Nothing(), hydra.ext.python.names.encode_name(False, hydra.util.CaseConvention.PASCAL, env, name), hydra.lib.lists.map((lambda arg_: hydra.ext.python.utils.py_name_to_py_type_parameter(hydra.ext.python.names.encode_type_variable(arg_))), find_type_params(env, typ)), Just(variant_args(ptype_quoted, tparam_list())), body))))[1]))

def extend_env_with_type_var(env: hydra.ext.python.helpers.PythonEnvironment, var_: hydra.core.Name) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Extend a PythonEnvironment with a new bound type variable."""
    
    @lru_cache(1)
    def old_bound() -> tuple[frozenlist[hydra.core.Name], FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]]:
        return env.bound_type_variables
    @lru_cache(1)
    def tparam_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(old_bound())
    @lru_cache(1)
    def tparam_map() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.pairs.second(old_bound())
    @lru_cache(1)
    def new_list() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.concat2(tparam_list(), (var_,))
    @lru_cache(1)
    def new_map() -> FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]:
        return hydra.lib.maps.insert(var_, hydra.ext.python.names.encode_type_variable(var_), tparam_map())
    return hydra.ext.python.helpers.PythonEnvironment(env.namespaces, (new_list(), new_map()), env.type_context, env.nullary_bindings, env.version, env.skip_casts, env.inline_variables)

def encode_type_assignment_inner(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[hydra.ext.python.syntax.Statement]]:
    r"""Encode the inner type definition, unwrapping forall types."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    @lru_cache(1)
    def dflt() -> hydra.compute.Flow[T0, frozenlist[hydra.ext.python.syntax.Statement]]:
        return hydra.lib.flows.bind(encode_type(env, typ), (lambda type_expr: hydra.lib.flows.pure(encode_type_def_single(env, name, comment, type_expr))))
    match stripped():
        case hydra.core.TypeForall(value=ft):
            @lru_cache(1)
            def tvar() -> hydra.core.Name:
                return ft.parameter
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return ft.body
            @lru_cache(1)
            def new_env() -> hydra.ext.python.helpers.PythonEnvironment:
                return extend_env_with_type_var(env, tvar())
            return encode_type_assignment_inner(new_env(), name, body(), comment)
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.flows.map((lambda s: (s,)), encode_record_type(env, name, rt, comment))
        
        case hydra.core.TypeUnion(value=rt2):
            return encode_union_type(env, name, rt2, comment)
        
        case hydra.core.TypeWrap(value=wt):
            @lru_cache(1)
            def inner_type() -> hydra.core.Type:
                return wt.body
            return hydra.lib.flows.map((lambda s: (s,)), encode_wrapped_type(env, name, inner_type(), comment))
        
        case _:
            return dflt()

def encode_type_assignment(env: hydra.ext.python.helpers.PythonEnvironment, name: hydra.core.Name, typ: hydra.core.Type, comment: Maybe[str]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]]:
    r"""Encode a type definition, dispatching based on type structure."""
    
    return hydra.lib.flows.bind(encode_type_assignment_inner(env, name, typ, comment), (lambda def_stmts: (const_stmts := encode_name_constants(env, name, typ), hydra.lib.flows.pure(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda s: (s,)), def_stmts), (const_stmts,))))[1]))

def encode_definition(env: hydra.ext.python.helpers.PythonEnvironment, def_: hydra.module.Definition) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, frozenlist[frozenlist[hydra.ext.python.syntax.Statement]]]:
    r"""Encode a definition (term or type) to Python statements."""
    
    match def_:
        case hydra.module.DefinitionTerm(value=td):
            @lru_cache(1)
            def name() -> hydra.core.Name:
                return td.name
            @lru_cache(1)
            def term() -> hydra.core.Term:
                return td.term
            @lru_cache(1)
            def typ() -> hydra.core.TypeScheme:
                return td.type
            return hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_term_description(term())), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), hydra.lib.flows.bind(encode_term_assignment(env, name(), term(), typ(), norm_comment), (lambda stmt: hydra.lib.flows.pure(((stmt,),)))))[1]))
        
        case hydra.module.DefinitionType(value=td2):
            @lru_cache(1)
            def name() -> hydra.core.Name:
                return td2.name
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return td2.type
            return hydra.lib.flows.bind(in_graph_context(hydra.annotations.get_type_description(typ())), (lambda comment: (norm_comment := hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, comment), encode_type_assignment(env, name(), typ(), norm_comment))[1]))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_field(env: hydra.ext.python.helpers.PythonEnvironment, field: hydra.core.Field, encode_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, T1]]) -> hydra.compute.Flow[T0, tuple[hydra.ext.python.syntax.Name, T1]]:
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field.name
    @lru_cache(1)
    def fterm() -> hydra.core.Term:
        return field.term
    return hydra.lib.flows.bind(encode_term(fterm()), (lambda pterm: hydra.lib.flows.pure((hydra.ext.python.names.encode_field_name(env, fname()), pterm))))

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

def extend_meta_for_type(top_level: bool, is_term_annot: bool, typ: hydra.core.Type, meta: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Extend metadata based on a type (used during module encoding)."""
    
    @lru_cache(1)
    def current_tvars() -> frozenset[hydra.core.Name]:
        return meta.type_variables
    @lru_cache(1)
    def new_tvars() -> frozenset[hydra.core.Name]:
        return collect_type_variables(current_tvars(), typ)
    @lru_cache(1)
    def meta_with_tvars() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return set_meta_type_variables(meta, new_tvars())
    @lru_cache(1)
    def meta_with_subtypes() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return hydra.lib.lists.foldl((lambda m, t: extend_meta_for_type(False, is_term_annot, t, m)), meta_with_tvars(), hydra.rewriting.subtypes(typ))
    def _hoist_body_1(v1: hydra.core.FloatType) -> hydra.ext.python.helpers.PythonModuleMetadata:
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return set_meta_uses_decimal(meta_with_subtypes(), True)
            
            case _:
                return meta_with_subtypes()
    def _hoist_body_2(v1: hydra.core.LiteralType) -> hydra.ext.python.helpers.PythonModuleMetadata:
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return _hoist_body_1(ft)
            
            case _:
                return meta_with_subtypes()
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeFunction(value=ft):
            @lru_cache(1)
            def cod() -> hydra.core.Type:
                return ft.codomain
            @lru_cache(1)
            def dom() -> hydra.core.Type:
                return ft.domain
            @lru_cache(1)
            def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return extend_meta_for_type(top_level, is_term_annot, cod(), meta_with_subtypes())
            @lru_cache(1)
            def meta3() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return extend_meta_for_type(False, is_term_annot, dom(), meta2())
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
            return hydra.lib.logic.if_else(hydra.schemas.is_enum_row_type(rt), (lambda : set_meta_uses_enum(meta_with_subtypes(), True)), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(rt.fields)), (lambda : set_meta_uses_node(meta_with_subtypes(), True)), (lambda : meta_with_subtypes()))))
        
        case hydra.core.TypeForall(value=ft2):
            @lru_cache(1)
            def body() -> hydra.core.Type:
                return ft2.body
            @lru_cache(1)
            def meta_for_wrap() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return dig_for_wrap(is_term_annot, meta_with_subtypes(), body())
            match hydra.rewriting.deannotate_type(body()):
                case hydra.core.TypeRecord():
                    return set_meta_uses_generic(meta_for_wrap(), True)
                
                case _:
                    return meta_for_wrap()
        
        case hydra.core.TypeRecord(value=rt2):
            @lru_cache(1)
            def fields() -> frozenlist[hydra.core.FieldType]:
                return rt2.fields
            @lru_cache(1)
            def has_annotated() -> bool:
                return hydra.lib.lists.foldl((lambda b, ft: hydra.lib.logic.or_(b, hydra.annotations.has_type_description(ft.type))), False, fields())
            @lru_cache(1)
            def meta1() -> hydra.ext.python.helpers.PythonModuleMetadata:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(fields()), (lambda : meta_with_subtypes()), (lambda : set_meta_uses_dataclass(meta_with_subtypes(), True)))
            return hydra.lib.logic.if_else(has_annotated(), (lambda : set_meta_uses_annotated(meta1(), True)), (lambda : meta1()))
        
        case hydra.core.TypeWrap():
            return hydra.lib.logic.if_else(is_term_annot, (lambda : meta_with_subtypes()), (lambda : set_meta_uses_node(meta_with_subtypes(), True)))
        
        case _:
            return meta_with_subtypes()

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
    
    def step(meta: hydra.ext.python.helpers.PythonModuleMetadata, t: hydra.core.Term) -> hydra.ext.python.helpers.PythonModuleMetadata:
        def _hoist_step_1(meta: hydra.ext.python.helpers.PythonModuleMetadata, v1: hydra.core.Function) -> hydra.ext.python.helpers.PythonModuleMetadata:
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.maybes.maybe(meta, (lambda dom: hydra.lib.logic.if_else(top_level, (lambda : extend_meta_for_type(True, False, dom, meta)), (lambda : meta))), lam.domain)
                
                case _:
                    return meta
        def _hoist_step_2(meta: hydra.ext.python.helpers.PythonModuleMetadata, v1: hydra.core.FloatValue) -> hydra.ext.python.helpers.PythonModuleMetadata:
            match v1:
                case hydra.core.FloatValueBigfloat():
                    return set_meta_uses_decimal(meta, True)
                
                case _:
                    return meta
        def _hoist_step_3(meta: hydra.ext.python.helpers.PythonModuleMetadata, v1: hydra.core.Literal) -> hydra.ext.python.helpers.PythonModuleMetadata:
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
                @lru_cache(1)
                def bindings() -> frozenlist[hydra.core.Binding]:
                    return lt.bindings
                return hydra.lib.lists.foldl((for_binding := (lambda m, b: hydra.lib.maybes.maybe(m, (lambda ts: (term1 := b.term, hydra.lib.logic.if_else(hydra.coder_utils.is_simple_assignment(term1), (lambda : m), (lambda : extend_meta_for_type(True, True, ts.type, m))))[1]), b.type)), (lambda x1, x2: for_binding(x1, x2)))[1], meta, bindings())
            
            case hydra.core.TermLiteral(value=l):
                return _hoist_step_3(meta, l)
            
            case hydra.core.TermMap():
                return set_meta_uses_frozen_dict(meta, True)
            
            case hydra.core.TermMaybe(value=m):
                return hydra.lib.maybes.maybe(set_meta_uses_nothing(meta, True), (lambda _: set_meta_uses_just(meta, True)), m)
            
            case hydra.core.TermUnion():
                return set_meta_uses_cast(True, meta)
            
            case _:
                return meta
    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: step(x1, x2)), meta0, term)

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
                @lru_cache(1)
                def term() -> hydra.core.Term:
                    return term_def.term
                @lru_cache(1)
                def typ_scheme() -> hydra.core.TypeScheme:
                    return term_def.type
                @lru_cache(1)
                def typ() -> hydra.core.Type:
                    return typ_scheme().type
                @lru_cache(1)
                def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                    return extend_meta_for_type(True, True, typ(), meta)
                return extend_meta_for_term(True, meta2(), term())
            
            case hydra.module.DefinitionType(value=type_def):
                @lru_cache(1)
                def typ() -> hydra.core.Type:
                    return type_def.type
                @lru_cache(1)
                def meta2() -> hydra.ext.python.helpers.PythonModuleMetadata:
                    return set_meta_uses_name(meta, True)
                return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: extend_meta_for_type(True, False, t, m)), meta2(), typ())
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def result() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return hydra.lib.lists.foldl((lambda x1, x2: add_def(x1, x2)), start(), defs)
    @lru_cache(1)
    def tvars() -> frozenset[hydra.core.Name]:
        return result().type_variables
    return set_meta_uses_type_var(result(), hydra.lib.logic.not_(hydra.lib.sets.null(tvars())))

# The target Python version for code generation.
target_python_version = hydra.ext.python.utils.target_python_version

def initial_environment(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], tcontext: hydra.typing.TypeContext) -> hydra.ext.python.helpers.PythonEnvironment:
    r"""Create an initial Python environment for code generation."""
    
    return hydra.ext.python.helpers.PythonEnvironment(namespaces, ((), hydra.lib.maps.empty()), tcontext, hydra.lib.sets.empty(), target_python_version, True, hydra.lib.sets.empty())

def is_type_module_check(defs: frozenlist[hydra.module.Definition]) -> bool:
    def _hoist_hydra_ext_python_coder_is_type_module_check_1(v1: hydra.module.Definition) -> bool:
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
    def sorted_term_defs() -> frozenlist[hydra.module.Definition]:
        def _hoist_sorted_term_defs_1(v1: hydra.module.Definition) -> hydra.core.Name:
            match v1:
                case hydra.module.DefinitionTerm(value=td):
                    return td.name
                
                case _:
                    raise TypeError("Unsupported Definition")
        def _hoist_sorted_term_defs_2(v1: hydra.module.Definition) -> frozenlist[hydra.core.Name]:
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
    return hydra.schemas.with_let_context((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), (lambda x1, x2: hydra.coder_utils.binding_metadata(x1, x2)), v1, v2, v3)

def with_definitions(env: hydra.ext.python.helpers.PythonEnvironment, defs: frozenlist[hydra.module.Definition], body: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    @lru_cache(1)
    def bindings() -> frozenlist[hydra.core.Binding]:
        def _hoist_bindings_1(v1: hydra.module.Definition) -> Maybe[hydra.core.Binding]:
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

def encode_python_module(mod: hydra.module.Module, defs0: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.ext.python.syntax.Module]:
    r"""Encode a Hydra module to a Python module AST."""
    
    @lru_cache(1)
    def defs() -> frozenlist[hydra.module.Definition]:
        return reorder_defs(defs0)
    @lru_cache(1)
    def meta0() -> hydra.ext.python.helpers.PythonModuleMetadata:
        return gather_metadata(mod.namespace, defs())
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.monads.with_state(make_py_graph(g, meta0()), (namespaces0 := meta0().namespaces, hydra.lib.flows.bind(hydra.inference.initial_type_context(g), (lambda tcontext: (env0 := initial_environment(namespaces0, tcontext), is_type_mod := is_type_module_check(defs0), with_definitions(env0, defs(), (lambda env: hydra.lib.flows.bind(hydra.lib.flows.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.flows.map_list((lambda d: encode_definition(env, d)), defs())), (lambda def_stmts: hydra.lib.flows.bind(hydra.monads.get_state(), (lambda pyg1: (meta1 := py_graph_metadata(pyg1), meta2 := hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.logic.not_(is_type_mod), use_inline_type_params()), (lambda : set_meta_uses_type_var(meta1, False)), (lambda : meta1)), meta := hydra.lib.logic.if_else(hydra.lib.logic.and_(is_type_mod, hydra.lib.equality.equal(target_python_version, hydra.ext.python.helpers.PythonVersion.PYTHON310)), (lambda : set_meta_uses_type_alias(meta2, True)), (lambda : meta2)), namespaces := meta1.namespaces, comment_stmts := hydra.lib.maybes.maybe((), (lambda c: (hydra.ext.python.utils.comment_statement(c),)), hydra.lib.maybes.map(hydra.coder_utils.normalize_comment, mod.description)), import_stmts := module_imports(namespaces, meta), tvars := hydra.lib.logic.if_else(hydra.lib.logic.or_(is_type_mod, hydra.lib.logic.not_(use_inline_type_params())), (lambda : meta.type_variables), (lambda : hydra.lib.sets.empty())), tvar_stmts := hydra.lib.lists.map((lambda tv: tvar_statement(hydra.ext.python.names.encode_type_variable(tv))), hydra.lib.sets.to_list(tvars)), body := hydra.lib.lists.filter((lambda group: hydra.lib.logic.not_(hydra.lib.lists.null(group))), hydra.lib.lists.concat(((comment_stmts, import_stmts, tvar_stmts), def_stmts))), hydra.lib.flows.pure(hydra.ext.python.syntax.Module(body)))[9])))))))[2])))[1])))

def set_meta_namespaces(ns: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], m: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    return hydra.ext.python.helpers.PythonModuleMetadata(ns, m.type_variables, m.uses_annotated, m.uses_callable, m.uses_cast, m.uses_lru_cache, m.uses_type_alias, m.uses_dataclass, m.uses_decimal, m.uses_either, m.uses_enum, m.uses_frozen_dict, m.uses_frozen_list, m.uses_generic, m.uses_just, m.uses_left, m.uses_maybe, m.uses_name, m.uses_node, m.uses_nothing, m.uses_right, m.uses_type_var)

def extend_meta_for_types(types: frozenlist[hydra.core.Type], meta: hydra.ext.python.helpers.PythonModuleMetadata) -> hydra.ext.python.helpers.PythonModuleMetadata:
    r"""Extend metadata for a list of types."""
    
    @lru_cache(1)
    def names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda t: hydra.rewriting.type_dependency_names(False, t)), types))
    @lru_cache(1)
    def current_ns() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return meta.namespaces
    @lru_cache(1)
    def updated_ns() -> hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]:
        return hydra.schemas.add_names_to_namespaces((lambda x1: hydra.ext.python.names.encode_namespace(x1)), names(), current_ns())
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

def module_to_python(mod: hydra.module.Module, defs: frozenlist[hydra.module.Definition]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[str, str]]:
    r"""Convert a Hydra module to Python source files."""
    
    return hydra.lib.flows.bind(encode_python_module(mod, defs), (lambda file: (s := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.ext.python.serde.encode_module(file))), path := hydra.names.namespace_to_file_path(hydra.util.CaseConvention.LOWER_SNAKE, hydra.module.FileExtension("py"), mod.namespace), hydra.lib.flows.pure(hydra.lib.maps.singleton(path, s)))[2]))

def wildcard_case_block(stmt: hydra.ext.python.syntax.Statement) -> hydra.ext.python.syntax.CaseBlock:
    r"""Create a wildcard case block with a given body statement."""
    
    return hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.py_closed_pattern_to_py_patterns(cast(hydra.ext.python.syntax.ClosedPattern, hydra.ext.python.syntax.ClosedPatternWildcard())), Nothing(), hydra.ext.python.utils.indented_block(Nothing(), ((stmt,),)))

def with_lambda(v1: hydra.ext.python.helpers.PythonEnvironment, v2: hydra.core.Lambda, v3: Callable[[hydra.ext.python.helpers.PythonEnvironment], T0]) -> T0:
    return hydra.schemas.with_lambda_context((lambda x1: python_environment_get_type_context(x1)), (lambda x1, x2: python_environment_set_type_context(x1, x2)), v1, v2, v3)

def with_updated_graph(v1: Callable[[hydra.graph.Graph], hydra.graph.Graph], v2: hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, T0]) -> hydra.compute.Flow[hydra.ext.python.helpers.PyGraph, T0]:
    return hydra.coder_utils.with_updated_coder_graph((lambda x1: py_graph_graph(x1)), (lambda x1: py_graph_metadata(x1)), (lambda x1, x2: make_py_graph(x1, x2)), v1, v2)
