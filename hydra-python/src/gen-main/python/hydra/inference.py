# Note: this is an automatically generated file. Do not edit.

r"""Type inference following Algorithm W, extended for nominal terms and types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.checking
import hydra.context
import hydra.core
import hydra.errors
import hydra.extract.core
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
import hydra.names
import hydra.reflect
import hydra.resolution
import hydra.rewriting
import hydra.show.core
import hydra.show.typing
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.unification
import hydra.variables

T0 = TypeVar("T0")

def bind_constraints(flow_cx: hydra.context.Context, cx: hydra.graph.Graph, constraints: frozenlist[hydra.typing.TypeConstraint]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.TypeSubst]:
    r"""Unify type constraints and check the substitution."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_ic.object.message))), _ic.context)), (lambda _a: _a), hydra.unification.unify_type_constraints(flow_cx, cx.schema_types, constraints)), (lambda s: hydra.lib.eithers.bind(hydra.checking.check_type_subst(flow_cx, cx, s), (lambda _: Right(s)))))

def bind_unbound_type_variables(cx: hydra.graph.Graph, term0: hydra.core.Term) -> hydra.core.Term:
    r"""Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding. These variables may appear in the binding type scheme itself or in that of a subterm, in domain types attached to functions, and in type abstraction and type application terms. This process attempts to capture type variables which have escaped unification, e.g. due to unused code. However, unbound type variables not appearing beneath any typed let binding remain unbound."""

    @lru_cache(1)
    def svars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(cx.schema_types))
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        match term:
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    bname = b.name
                    bterm = b.term
                    return hydra.lib.maybes.maybe((lambda : hydra.core.Binding(bname, bind_unbound_type_variables(cx, bterm), Nothing())), (lambda ts: (bvars := hydra.lib.sets.from_list(ts.variables), unbound_in_type := hydra.variables.free_variables_in_type(ts.type), unbound_in_term := hydra.variables.free_type_variables_in_term(bterm), unbound := hydra.lib.sets.to_list(hydra.lib.sets.difference(hydra.lib.sets.union(unbound_in_type, unbound_in_term), hydra.lib.sets.union(svars(), bvars))), ts2 := hydra.core.TypeScheme(hydra.lib.lists.concat2(ts.variables, unbound), ts.type, ts.constraints), bterm2 := hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, t)))), bterm, unbound), hydra.core.Binding(bname, bterm2, Just(ts2)))[6]), b.type)
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: for_binding(x1)), l.bindings), bind_unbound_type_variables(cx, l.body))))

            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)

def build_type_application_term(tvars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Term:
    r"""Fold a list of type variables over a term to build a type application term."""

    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, cast(hydra.core.Type, hydra.core.TypeVariable(v)))))), body, tvars)

def extend_context(pairs: frozenlist[tuple[hydra.core.Name, hydra.core.TypeScheme]], cx: hydra.graph.Graph) -> hydra.graph.Graph:
    r"""Add (term variable, type scheme) pairs to the graph's bound types."""

    return hydra.graph.Graph(cx.bound_terms, hydra.lib.maps.union(hydra.lib.maps.from_list(pairs), cx.bound_types), cx.class_constraints, cx.lambda_variables, cx.metadata, cx.primitives, cx.schema_types, cx.type_variables)

def finalize_inferred_term(flow_cx: hydra.context.Context, cx: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term]:
    r"""Finalize an inferred term by checking for unbound type variables, then normalizing type variables."""

    @lru_cache(1)
    def term2() -> hydra.core.Term:
        return bind_unbound_type_variables(cx, term)
    return hydra.lib.eithers.bind(hydra.checking.check_for_unbound_type_variables(flow_cx, cx, term2()), (lambda _: Right(hydra.variables.normalize_type_variables_in_term(term2()))))

def merge_class_constraints(m1: FrozenDict[T0, hydra.core.TypeVariableMetadata], m2: FrozenDict[T0, hydra.core.TypeVariableMetadata]) -> FrozenDict[T0, hydra.core.TypeVariableMetadata]:
    r"""Merge two maps of class constraints. When both maps have constraints for the same variable, union the class sets."""

    return hydra.lib.lists.foldl((lambda acc, pair: (k := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.maybes.maybe((lambda : hydra.lib.maps.insert(k, v, acc)), (lambda existing: (merged := hydra.core.TypeVariableMetadata(hydra.lib.sets.union(existing.classes, v.classes)), hydra.lib.maps.insert(k, merged, acc))[1]), hydra.lib.maps.lookup(k, acc)))[2]), m1, hydra.lib.maps.to_list(m2))

def fresh_variable_type(cx: hydra.context.Context) -> tuple[hydra.core.Type, hydra.context.Context]:
    r"""Generate a fresh type variable."""

    @lru_cache(1)
    def result() -> tuple[hydra.core.Name, hydra.context.Context]:
        return hydra.names.fresh_name(cx)
    @lru_cache(1)
    def name() -> hydra.core.Name:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def cx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(result())
    return (cast(hydra.core.Type, hydra.core.TypeVariable(name())), cx2())

def yield_checked(fcx: hydra.context.Context, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.typing.InferenceResult:
    r"""Create a checked inference result."""

    @lru_cache(1)
    def iterm() -> hydra.core.Term:
        return hydra.substitution.subst_types_in_term(subst, term)
    @lru_cache(1)
    def itype() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    return hydra.typing.InferenceResult(iterm(), itype(), subst, hydra.lib.maps.empty(), fcx)

def map_constraints(flow_cx: hydra.context.Context, cx: hydra.graph.Graph, f: Callable[[hydra.typing.TypeSubst], T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Map over type constraints after unification."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_ic.object.message))), _ic.context)), (lambda _a: _a), hydra.unification.unify_type_constraints(flow_cx, cx.schema_types, constraints)), (lambda s: hydra.lib.eithers.bind(hydra.checking.check_type_subst(flow_cx, cx, s), (lambda _: Right(f(s))))))

def yield_with_constraints(fcx: hydra.context.Context, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst, constraints: FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]) -> hydra.typing.InferenceResult:
    r"""Create an inference result with class constraints."""

    return hydra.typing.InferenceResult(hydra.substitution.subst_types_in_term(subst, term), hydra.substitution.subst_in_type(subst, typ), subst, constraints, fcx)

def yield_(fcx: hydra.context.Context, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.typing.InferenceResult:
    r"""Create an inference result with no class constraints."""

    return hydra.typing.InferenceResult(hydra.substitution.subst_types_in_term(subst, term), hydra.substitution.subst_in_type(subst, typ), subst, hydra.lib.maps.empty(), fcx)

def infer_type_of_projection(fcx: hydra.context.Context, cx: hydra.graph.Graph, proj: hydra.core.Projection) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a record projection (Either version)."""

    tname = proj.type_name
    fname = proj.field
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx2 := hydra.lib.pairs.second(st_rp), svars := schema_type.variables, stype := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.record_type(fcx2, tname, stype), (lambda sfields: hydra.lib.eithers.bind(hydra.resolution.find_field_type(fcx2, fname, sfields), (lambda ftyp: Right(yield_(fcx2, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(tname, fname)))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), ftyp))), hydra.substitution.id_type_subst())))))))[4]))

def infer_type_of_unwrap(fcx: hydra.context.Context, cx: hydra.graph.Graph, tname: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of an unwrap operation (Either version)."""

    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx2 := hydra.lib.pairs.second(st_rp), svars := schema_type.variables, stype := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.wrapped_type(fcx2, tname, stype), (lambda wtyp: Right(yield_(fcx2, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(tname))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), wtyp))), hydra.substitution.id_type_subst())))))[4]))

def free_variables_in_context(cx: hydra.graph.Graph) -> frozenset[hydra.core.Name]:
    r"""Get all free variables in a graph's bound types."""

    return hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), hydra.lib.lists.map((lambda x1: hydra.variables.free_variables_in_type_scheme_simple(x1)), hydra.lib.maps.elems(cx.bound_types)))

def is_unbound(cx: hydra.graph.Graph, v: hydra.core.Name) -> bool:
    r"""Check if a variable is unbound in context."""

    return hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_context(cx))), hydra.lib.logic.not_(hydra.lib.maps.member(v, cx.schema_types)))

def generalize(cx: hydra.graph.Graph, typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Generalize a type to a type scheme."""

    def is_type_var_name(name: hydra.core.Name) -> bool:
        @lru_cache(1)
        def parts() -> frozenlist[str]:
            return hydra.lib.strings.split_on(".", name.value)
        return hydra.lib.equality.lte(hydra.lib.lists.length(parts()), 1)
    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(is_unbound(cx, v), is_type_var_name(v))), hydra.variables.free_variables_in_type_ordered(typ)))
    all_constraints = cx.class_constraints
    @lru_cache(1)
    def relevant_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda v: hydra.lib.maybes.map((lambda meta: (v, meta)), hydra.lib.maps.lookup(v, all_constraints))), vars())))
    @lru_cache(1)
    def constraints_maybe() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(relevant_constraints()), (lambda : Nothing()), (lambda : Just(relevant_constraints())))
    return hydra.core.TypeScheme(vars(), typ, constraints_maybe())

def infer_type_of_literal(fcx: hydra.context.Context, lit: hydra.core.Literal) -> hydra.typing.InferenceResult:
    r"""Infer the type of a literal."""

    return hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLiteral(lit)), cast(hydra.core.Type, hydra.core.TypeLiteral(hydra.reflect.literal_type(lit))), hydra.substitution.id_type_subst(), hydra.lib.maps.empty(), fcx)

def infer_type_of_unit(fcx: hydra.context.Context) -> hydra.typing.InferenceResult:
    r"""The trivial inference rule for the unit term."""

    return hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermUnit()), cast(hydra.core.Type, hydra.core.TypeUnit()), hydra.substitution.id_type_subst(), hydra.lib.maps.empty(), fcx)

def yield_checked_with_constraints(fcx: hydra.context.Context, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst, constraints: FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]) -> hydra.typing.InferenceResult:
    r"""Create a checked inference result with class constraints."""

    @lru_cache(1)
    def iterm() -> hydra.core.Term:
        return hydra.substitution.subst_types_in_term(subst, term)
    @lru_cache(1)
    def itype() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    @lru_cache(1)
    def iconstraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.substitution.subst_in_class_constraints(subst, constraints)
    return hydra.typing.InferenceResult(iterm(), itype(), subst, iconstraints(), fcx)

def infer_type_of_variable(fcx: hydra.context.Context, cx: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a variable (Either version)."""

    return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Variable not bound to type: ", name.value)))), fcx))), (lambda scheme: (ts_result := hydra.resolution.instantiate_type_scheme(fcx, scheme), ts := hydra.lib.pairs.first(ts_result), fcx2 := hydra.lib.pairs.second(ts_result), constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), ts.constraints), Right(yield_checked_with_constraints(fcx2, build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name))), ts.type, hydra.substitution.id_type_subst(), constraints)))[4]), hydra.lib.maybes.map((lambda v1: v1.type), hydra.lib.maps.lookup(name, cx.primitives)))), (lambda scheme: (ts_result := hydra.resolution.instantiate_type_scheme(fcx, scheme), ts := hydra.lib.pairs.first(ts_result), fcx2 := hydra.lib.pairs.second(ts_result), constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), ts.constraints), Right(hydra.typing.InferenceResult(build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name))), ts.type, hydra.substitution.id_type_subst(), constraints, fcx2)))[4]), hydra.lib.maps.lookup(name, cx.bound_types))

def infer_many(fcx: hydra.context.Context, cx: hydra.graph.Graph, pairs: frozenlist[tuple[hydra.core.Term, str]]) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], tuple[hydra.typing.TypeSubst, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]]:
    r"""Infer types for multiple terms, propagating class constraints from sub-expressions."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(pairs), (lambda : Right((((), ((), (hydra.substitution.id_type_subst(), hydra.lib.maps.empty()))), fcx))), (lambda : (dflt := (e := hydra.lib.pairs.first(hydra.lib.lists.head(pairs)), (desc := hydra.lib.pairs.second(hydra.lib.lists.head(pairs)), (tl := hydra.lib.lists.tail(pairs), hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, e, desc), (lambda result1: (fcx2 := result1.context, e1 := result1.term, t1 := result1.type, s1 := result1.subst, c1 := result1.class_constraints, hydra.lib.eithers.bind(infer_many(fcx2, hydra.substitution.subst_in_context(s1, cx), tl), (lambda rp2: (result2 := hydra.lib.pairs.first(rp2), fcx3 := hydra.lib.pairs.second(rp2), e2 := hydra.lib.pairs.first(result2), t2 := hydra.lib.pairs.first(hydra.lib.pairs.second(result2)), s2 := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(result2))), c2 := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(result2))), c1_subst := hydra.substitution.subst_in_class_constraints(s2, c1), merged_constraints := merge_class_constraints(c1_subst, c2), Right(((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(s2, e1), e2), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(s2, t1), t2), (hydra.substitution.compose_type_subst(s1, s2), merged_constraints))), fcx3)))[8])))[5])))[1])[1])[1], dflt)[1]))

def infer_type_of_annotated_term(fcx: hydra.context.Context, cx: hydra.graph.Graph, at: hydra.core.AnnotatedTerm) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of an annotated term (Either version)."""

    term = at.body
    ann = at.annotation
    return hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, term, "annotated term"), (lambda result: (fcx2 := result.context, iterm := result.term, itype := result.type, isubst := result.subst, iconstraints := result.class_constraints, Right(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(iterm, ann))), itype, isubst, iconstraints, fcx2)))[5]))

def infer_type_of_application(fcx0: hydra.context.Context, cx: hydra.graph.Graph, app: hydra.core.Application) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a function application (Either version)."""

    @lru_cache(1)
    def fcx() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons("application", fcx0.trace), fcx0.messages, fcx0.other)
    e0 = app.function
    e1 = app.argument
    return hydra.lib.eithers.bind(infer_type_of_term(fcx(), cx, e0, "lhs"), (lambda lhs_result: (fcx2 := lhs_result.context, a := lhs_result.term, t0 := lhs_result.type, s0 := lhs_result.subst, c0 := lhs_result.class_constraints, hydra.lib.eithers.bind(infer_type_of_term(fcx2, hydra.substitution.subst_in_context(s0, cx), e1, "rhs"), (lambda rhs_result: (fcx3 := rhs_result.context, b := rhs_result.term, t1 := rhs_result.type, s1 := rhs_result.subst, c1 := rhs_result.class_constraints, v_result := hydra.names.fresh_name(fcx3), v := hydra.lib.pairs.first(v_result), fcx4 := hydra.lib.pairs.second(v_result), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_ic.object.message))), _ic.context)), (lambda _a: _a), hydra.unification.unify_types(fcx4, cx.schema_types, hydra.substitution.subst_in_type(s1, t0), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(t1, cast(hydra.core.Type, hydra.core.TypeVariable(v))))), "application lhs")), (lambda s2: hydra.lib.eithers.bind(hydra.checking.check_type_subst(fcx4, cx, s2), (lambda _: (r_expr := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(s1, s2), a), hydra.substitution.subst_types_in_term(s2, b)))), r_type := hydra.substitution.subst_in_type(s2, cast(hydra.core.Type, hydra.core.TypeVariable(v))), r_subst := hydra.substitution.compose_type_subst_list((s0, s1, s2)), c0_subst := hydra.substitution.subst_in_class_constraints(s2, hydra.substitution.subst_in_class_constraints(s1, c0)), c1_subst := hydra.substitution.subst_in_class_constraints(s2, c1), r_constraints := merge_class_constraints(c0_subst, c1_subst), Right(hydra.typing.InferenceResult(r_expr, r_type, r_subst, r_constraints, fcx4)))[6])))))[8])))[5]))

def infer_type_of_case_statement(fcx: hydra.context.Context, cx: hydra.graph.Graph, case_stmt: hydra.core.CaseStatement) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a case statement (Either version)."""

    tname = case_stmt.type_name
    dflt = case_stmt.default
    cases = case_stmt.cases
    @lru_cache(1)
    def fnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), cases)
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx2 := hydra.lib.pairs.second(st_rp), svars := schema_type.variables, stype := schema_type.type, hydra.lib.eithers.bind(hydra.extract.core.union_type(fcx2, tname, stype), (lambda sfields: hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda t: infer_type_of_term(fcx2, cx, t, hydra.lib.strings.cat(("case ", tname.value, ".<default>")))), dflt), (lambda dflt_rp: (dflt_result := dflt_rp, fcx3 := hydra.lib.maybes.from_maybe((lambda : fcx2), hydra.lib.maybes.map((lambda v1: v1.context), dflt_rp)), hydra.lib.eithers.bind(infer_many(fcx3, cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat(("case ", tname.value, ".", f.name.value)))), cases)), (lambda case_rp: (case_results := hydra.lib.pairs.first(case_rp), fcx4 := hydra.lib.pairs.second(case_rp), iterms := hydra.lib.pairs.first(case_results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(case_results)), isubst := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(case_results))), case_elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(case_results))), codv_result := hydra.names.fresh_name(fcx4), codv := hydra.lib.pairs.first(codv_result), fcx5 := hydra.lib.pairs.second(codv_result), cod := cast(hydra.core.Type, hydra.core.TypeVariable(codv)), case_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), sfields)), dflt_constraints := hydra.lib.maybes.to_list(hydra.lib.maybes.map((lambda r: hydra.typing.TypeConstraint(cod, hydra.substitution.subst_in_type(isubst, r.type), "match default")), dflt_result)), case_constraints := hydra.lib.maybes.cat(hydra.lib.lists.zip_with((lambda fname, itype: hydra.lib.maybes.map((lambda ftype: hydra.typing.TypeConstraint(itype, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(ftype, cod))), "case type")), hydra.lib.maps.lookup(fname, case_map))), fnames(), itypes)), dflt_class_constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), hydra.lib.maybes.map((lambda v1: v1.class_constraints), dflt_result)), all_elem_constraints := merge_class_constraints(case_elem_constraints, dflt_class_constraints), hydra.lib.eithers.bind(map_constraints(fcx5, cx, (lambda subst: yield_with_constraints(fcx5, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(tname, hydra.lib.maybes.map((lambda v1: v1.term), dflt_result), hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames(), iterms))))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), cod))), hydra.substitution.compose_type_subst_list(hydra.lib.lists.concat((hydra.lib.maybes.to_list(hydra.lib.maybes.map((lambda v1: v1.subst), dflt_result)), (isubst, subst)))), hydra.substitution.subst_in_class_constraints(subst, all_elem_constraints))), hydra.lib.lists.concat((dflt_constraints, case_constraints))), (lambda mc_result: Right(mc_result))))[15])))[2])))))[4]))

def infer_type_of_collection(fcx: hydra.context.Context, cx: hydra.graph.Graph, typ_cons: Callable[[hydra.core.Type], hydra.core.Type], trm_cons: Callable[[frozenlist[hydra.core.Term]], hydra.core.Term], desc: str, class_names: frozenset[hydra.core.Name], els: frozenlist[hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a collection. The classNames parameter specifies type classes (e.g. ordering) that the element type variable must satisfy."""

    @lru_cache(1)
    def var_result() -> tuple[hydra.core.Name, hydra.context.Context]:
        return hydra.names.fresh_name(fcx)
    @lru_cache(1)
    def var() -> hydra.core.Name:
        return hydra.lib.pairs.first(var_result())
    @lru_cache(1)
    def fcx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(var_result())
    @lru_cache(1)
    def class_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.lib.logic.if_else(hydra.lib.sets.null(class_names), (lambda : hydra.lib.maps.empty()), (lambda : hydra.lib.maps.singleton(var(), hydra.core.TypeVariableMetadata(class_names))))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : Right(yield_with_constraints(fcx2(), build_type_application_term((var(),), trm_cons(())), typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var()))), hydra.substitution.id_type_subst(), class_constraints()))), (lambda : hydra.lib.eithers.bind(infer_many(fcx2(), cx, hydra.lib.lists.zip(els, hydra.lib.lists.map((lambda i: hydra.lib.strings.cat(("#", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, hydra.lib.math.add(hydra.lib.lists.length(els), 1))))), (lambda results_rp: (results := hydra.lib.pairs.first(results_rp), fcx3 := hydra.lib.pairs.second(results_rp), terms := hydra.lib.pairs.first(results), types := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), subst1 := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), constraints := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(var())), t, desc)), types), all_constraints := merge_class_constraints(class_constraints(), elem_constraints), hydra.lib.eithers.bind(map_constraints(fcx3, cx, (lambda subst2: (iterm := trm_cons(terms), itype := typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var()))), isubst := hydra.substitution.compose_type_subst(subst1, subst2), yield_with_constraints(fcx3, iterm, itype, isubst, hydra.substitution.subst_in_class_constraints(subst2, all_constraints)))[3]), constraints), (lambda mc_result: Right(mc_result))))[8]))))

def infer_type_of_either(fcx: hydra.context.Context, cx: hydra.graph.Graph, e: Either[hydra.core.Term, hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of an either value (Either version)."""

    return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, l, "either left value"), (lambda r1: (fcx2 := r1.context, iterm := r1.term, left_type := r1.type, subst := r1.subst, fv_result := fresh_variable_type(fcx2), right_type := hydra.lib.pairs.first(fv_result), fcx3 := hydra.lib.pairs.second(fv_result), either_term := cast(hydra.core.Term, hydra.core.TermEither(Left(iterm))), term_with_left_type := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(either_term, left_type))), term_with_both_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(term_with_left_type, right_type))), either_type := cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, right_type))), Right(yield_checked(fcx3, term_with_both_types, either_type, subst)))[11]))), (lambda r: hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, r, "either right value"), (lambda r1: (fcx2 := r1.context, iterm := r1.term, right_type := r1.type, subst := r1.subst, fv_result := fresh_variable_type(fcx2), left_type := hydra.lib.pairs.first(fv_result), fcx3 := hydra.lib.pairs.second(fv_result), either_term := cast(hydra.core.Term, hydra.core.TermEither(Right(iterm))), term_with_left_type := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(either_term, left_type))), term_with_both_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(term_with_left_type, right_type))), either_type := cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, right_type))), Right(yield_checked(fcx3, term_with_both_types, either_type, subst)))[11]))), e)

def infer_type_of_elimination(fcx: hydra.context.Context, cx: hydra.graph.Graph, elm: hydra.core.Elimination) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of an elimination (Either version)."""

    match elm:
        case hydra.core.EliminationRecord(value=p):
            return infer_type_of_projection(fcx, cx, p)

        case hydra.core.EliminationUnion(value=c):
            return infer_type_of_case_statement(fcx, cx, c)

        case hydra.core.EliminationWrap(value=tname):
            return infer_type_of_unwrap(fcx, cx, tname)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def infer_type_of_function(fcx: hydra.context.Context, cx: hydra.graph.Graph, f: hydra.core.Function) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a function (Either version)."""

    match f:
        case hydra.core.FunctionElimination(value=elm):
            return infer_type_of_elimination(fcx, cx, elm)

        case hydra.core.FunctionLambda(value=l):
            return infer_type_of_lambda(fcx, cx, l)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def infer_type_of_injection(fcx: hydra.context.Context, cx: hydra.graph.Graph, injection: hydra.core.Injection) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a union injection (Either version)."""

    tname = injection.type_name
    field = injection.field
    fname = field.name
    term = field.term
    return hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, term, "injected term"), (lambda result: (fcx2 := result.context, hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx2, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx3 := hydra.lib.pairs.second(st_rp), svars := schema_type.variables, stype := schema_type.type, iterm := result.term, ityp := result.type, isubst := result.subst, hydra.lib.eithers.bind(hydra.extract.core.union_type(fcx3, tname, stype), (lambda sfields: hydra.lib.eithers.bind(hydra.resolution.find_field_type(fcx3, fname, sfields), (lambda ftyp: hydra.lib.eithers.bind(map_constraints(fcx3, cx, (lambda subst: yield_(fcx3, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(tname, hydra.core.Field(fname, iterm))))), hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field"),)), (lambda mc_result: Right(mc_result))))))))[7])))[1]))

def infer_type_of_lambda(fcx: hydra.context.Context, cx: hydra.graph.Graph, lambda_: hydra.core.Lambda) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a lambda function (Either version)."""

    var = lambda_.parameter
    body = lambda_.body
    @lru_cache(1)
    def vdom_result() -> tuple[hydra.core.Name, hydra.context.Context]:
        return hydra.names.fresh_name(fcx)
    @lru_cache(1)
    def vdom() -> hydra.core.Name:
        return hydra.lib.pairs.first(vdom_result())
    @lru_cache(1)
    def fcx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(vdom_result())
    @lru_cache(1)
    def dom() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeVariable(vdom()))
    @lru_cache(1)
    def cx2() -> hydra.graph.Graph:
        return extend_context(((var, hydra.core.TypeScheme((), dom(), Nothing())),), cx)
    return hydra.lib.eithers.bind(infer_type_of_term(fcx2(), cx2(), body, "lambda body"), (lambda result: (fcx3 := result.context, iterm := result.term, icod := result.type, isubst := result.subst, rdom := hydra.substitution.subst_in_type(isubst, dom()), rterm := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var, Just(rdom), iterm))))), rtype := cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(rdom, icod))), vars := hydra.lib.sets.unions((hydra.variables.free_variables_in_type(rdom), hydra.variables.free_variables_in_type(icod), free_variables_in_context(hydra.substitution.subst_in_context(isubst, cx2())))), cx3 := hydra.substitution.subst_in_context(isubst, cx), iconstraints := hydra.substitution.subst_in_class_constraints(isubst, result.class_constraints), Right(hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints, fcx3)))[10]))

def infer_type_of_let(fcx0: hydra.context.Context, cx: hydra.graph.Graph, let0: hydra.core.Let) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Normalize a let term before inferring its type (Either version)."""

    @lru_cache(1)
    def fcx() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons("let", fcx0.trace), fcx0.messages, fcx0.other)
    bindings0 = let0.bindings
    body0 = let0.body
    @lru_cache(1)
    def names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bindings0)
    @lru_cache(1)
    def name_set() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(names())
    def to_pair(binding: hydra.core.Binding) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        name = binding.name
        term = binding.term
        return (name, hydra.lib.lists.filter((lambda n: hydra.lib.sets.member(n, name_set())), hydra.lib.sets.to_list(hydra.variables.free_variables_in_term(term))))
    @lru_cache(1)
    def adj_list() -> frozenlist[tuple[hydra.core.Name, frozenlist[hydra.core.Name]]]:
        return hydra.lib.lists.map((lambda x1: to_pair(x1)), bindings0)
    @lru_cache(1)
    def groups() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(adj_list())
    @lru_cache(1)
    def binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
        return hydra.lib.maps.from_list(hydra.lib.lists.zip(names(), bindings0))
    def create_let(e: hydra.core.Term, group: frozenlist[hydra.core.Name]) -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map())), group)), e)))
    @lru_cache(1)
    def rewritten_let() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda x1, x2: create_let(x1, x2)), body0, hydra.lib.lists.reverse(groups()))
    def restore_let(iterm: hydra.core.Term) -> hydra.core.Term:
        def helper(level: int, bins: frozenlist[hydra.core.Binding], term: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
            def nonzero(term2: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                match term2:
                    case hydra.core.TermLet(value=l):
                        bs = l.bindings
                        let_body = l.body
                        return helper(hydra.lib.math.sub(level, 1), hydra.lib.lists.concat((bs, bins)), let_body)

                    case _:
                        raise TypeError("Unsupported Term")
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(level, 0), (lambda : (bins, term)), (lambda : nonzero(term)))
        @lru_cache(1)
        def result() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
            return helper(hydra.lib.lists.length(groups()), (), iterm)
        @lru_cache(1)
        def binding_list() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.first(result())
        @lru_cache(1)
        def e() -> hydra.core.Term:
            return hydra.lib.pairs.second(result())
        @lru_cache(1)
        def binding_map2() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b)), binding_list()))
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map2())), names())), e())))
    def rewrite_result(iresult: hydra.typing.InferenceResult) -> hydra.typing.InferenceResult:
        fcx_r = iresult.context
        iterm = iresult.term
        itype = iresult.type
        isubst = iresult.subst
        iconstraints = iresult.class_constraints
        return hydra.typing.InferenceResult(restore_let(iterm), itype, isubst, iconstraints, fcx_r)
    @lru_cache(1)
    def res() -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
        match rewritten_let():
            case hydra.core.TermLet(value=l):
                return infer_type_of_let_normalized(fcx(), cx, l)

            case _:
                return infer_type_of_term(fcx(), cx, rewritten_let(), "empty let term")
    return hydra.lib.eithers.map((lambda x1: rewrite_result(x1)), res())

def infer_type_of_let_normalized(fcx0: hydra.context.Context, cx0: hydra.graph.Graph, let_term: hydra.core.Let) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a let (letrec) term which is already in a normal form (Either version)."""

    @lru_cache(1)
    def fcx() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons("let-normalized", fcx0.trace), fcx0.messages, fcx0.other)
    bins0 = let_term.bindings
    body0 = let_term.body
    @lru_cache(1)
    def bnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bins0)
    @lru_cache(1)
    def bvars_result() -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
        return hydra.names.fresh_names(hydra.lib.lists.length(bins0), fcx())
    @lru_cache(1)
    def bvars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(bvars_result())
    @lru_cache(1)
    def fcx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(bvars_result())
    @lru_cache(1)
    def tbins0() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), bvars())
    @lru_cache(1)
    def cx1() -> hydra.graph.Graph:
        return extend_context(hydra.lib.lists.zip(bnames(), hydra.lib.lists.map((lambda t: hydra.core.TypeScheme((), t, Nothing())), tbins0())), cx0)
    return hydra.lib.eithers.bind(infer_types_of_temporary_bindings(fcx2(), cx1(), bins0), (lambda ir_rp: (inferred_result := hydra.lib.pairs.first(ir_rp), fcx3 := hydra.lib.pairs.second(ir_rp), bterms1 := hydra.lib.pairs.first(inferred_result), tbins1 := hydra.lib.pairs.first(hydra.lib.pairs.second(inferred_result)), subst_and_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(inferred_result)), s1 := hydra.lib.pairs.first(subst_and_constraints), inferred_constraints := hydra.lib.pairs.second(subst_and_constraints), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_ic.object.message))), _ic.context)), (lambda _a: _a), hydra.unification.unify_type_lists(fcx3, cx0.schema_types, hydra.lib.lists.map((lambda v1: hydra.substitution.subst_in_type(s1, v1)), tbins0()), tbins1, "temporary type bindings")), (lambda s2: hydra.lib.eithers.bind(hydra.checking.check_type_subst(fcx3, cx0, s2), (lambda _: (g2base := hydra.substitution.subst_in_context(hydra.substitution.compose_type_subst(s1, s2), cx0), constraints_with_s2 := hydra.substitution.subst_in_class_constraints(s2, inferred_constraints), composed_subst := hydra.substitution.compose_type_subst(s1, s2), original_binding_constraints := hydra.lib.lists.foldl((lambda acc, b: hydra.lib.maybes.maybe((lambda : acc), (lambda ts: hydra.lib.maybes.maybe((lambda : acc), (lambda c: merge_class_constraints(acc, c)), ts.constraints)), b.type)), hydra.lib.maps.empty(), bins0), original_constraints_subst := hydra.substitution.subst_in_class_constraints(composed_subst, original_binding_constraints), all_inferred_constraints := merge_class_constraints(constraints_with_s2, original_constraints_subst), merged_constraints := merge_class_constraints(g2base.class_constraints, all_inferred_constraints), g2 := hydra.graph.Graph(g2base.bound_terms, g2base.bound_types, merged_constraints, g2base.lambda_variables, g2base.metadata, g2base.primitives, g2base.schema_types, g2base.type_variables), bterms1_subst := hydra.lib.lists.map((lambda v1: hydra.substitution.subst_types_in_term(s2, v1)), bterms1), tsbins1 := hydra.lib.lists.zip(bnames(), hydra.lib.lists.map((lambda t: generalize(g2, hydra.substitution.subst_in_type(s2, t))), tbins1)), hydra.lib.eithers.bind(infer_type_of_term(fcx3, extend_context(tsbins1, g2), body0, "let body"), (lambda body_result: (fcx4 := body_result.context, body1 := body_result.term, tbody := body_result.type, sbody := body_result.subst, st1 := hydra.typing.TermSubst(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda pair: (name := hydra.lib.pairs.first(pair), ts := hydra.lib.pairs.second(pair), (name, build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name)))))[2]), tsbins1))), create_binding := (lambda binding_pair: (name_ts_pair := hydra.lib.pairs.first(binding_pair), term := hydra.lib.pairs.second(binding_pair), name := hydra.lib.pairs.first(name_ts_pair), ts := hydra.lib.pairs.second(name_ts_pair), final_ts := hydra.substitution.subst_in_type_scheme(sbody, ts), type_lambda_term := hydra.lib.lists.foldl((lambda b, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, b)))), hydra.substitution.substitute_in_term(st1, term), hydra.lib.lists.reverse(final_ts.variables)), hydra.core.Binding(name, hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(sbody, s2), type_lambda_term), Just(final_ts)))[6]), bins1 := hydra.lib.lists.map((lambda x1: create_binding(x1)), hydra.lib.lists.zip(tsbins1, bterms1_subst)), body_constraints := hydra.substitution.subst_in_class_constraints(sbody, body_result.class_constraints), binding_constraints_subst := hydra.substitution.subst_in_class_constraints(sbody, constraints_with_s2), all_constraints := merge_class_constraints(binding_constraints_subst, body_constraints), Right(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins1, body1))), tbody, hydra.substitution.compose_type_subst_list((s1, s2, sbody)), all_constraints, fcx4)))[10])))[10])))))[7]))

def infer_type_of_list(fcx: hydra.context.Context, cx: hydra.graph.Graph, v1: frozenlist[hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a list (Either version)."""

    return infer_type_of_collection(fcx, cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), "list element", hydra.lib.sets.empty(), v1)

def infer_type_of_map(fcx: hydra.context.Context, cx: hydra.graph.Graph, m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a map (Either version)."""

    @lru_cache(1)
    def kvar_result() -> tuple[hydra.core.Name, hydra.context.Context]:
        return hydra.names.fresh_name(fcx)
    @lru_cache(1)
    def kvar() -> hydra.core.Name:
        return hydra.lib.pairs.first(kvar_result())
    @lru_cache(1)
    def fcx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(kvar_result())
    @lru_cache(1)
    def vvar_result() -> tuple[hydra.core.Name, hydra.context.Context]:
        return hydra.names.fresh_name(fcx2())
    @lru_cache(1)
    def vvar() -> hydra.core.Name:
        return hydra.lib.pairs.first(vvar_result())
    @lru_cache(1)
    def fcx3() -> hydra.context.Context:
        return hydra.lib.pairs.second(vvar_result())
    @lru_cache(1)
    def key_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.lib.maps.singleton(kvar(), hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton(hydra.core.Name("ordering"))))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Right(yield_with_constraints(fcx3(), build_type_application_term((kvar(), vvar()), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.empty()))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar())), cast(hydra.core.Type, hydra.core.TypeVariable(vvar()))))), hydra.substitution.id_type_subst(), key_constraints()))), (lambda : hydra.lib.eithers.bind(infer_many(fcx3(), cx, hydra.lib.lists.map((lambda k: (k, "map key")), hydra.lib.maps.keys(m))), (lambda k_rp: (k_results := hydra.lib.pairs.first(k_rp), fcx4 := hydra.lib.pairs.second(k_rp), kterms := hydra.lib.pairs.first(k_results), ktypes := hydra.lib.pairs.first(hydra.lib.pairs.second(k_results)), ksubst := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(k_results))), k_elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(k_results))), hydra.lib.eithers.bind(infer_many(fcx4, hydra.substitution.subst_in_context(ksubst, cx), hydra.lib.lists.map((lambda v: (v, "map value")), hydra.lib.maps.elems(m))), (lambda v_rp: (v_results := hydra.lib.pairs.first(v_rp), fcx5 := hydra.lib.pairs.second(v_rp), vterms := hydra.lib.pairs.first(v_results), vtypes := hydra.lib.pairs.first(hydra.lib.pairs.second(v_results)), vsubst := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(v_results))), v_elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(v_results))), kcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(kvar())), t, "map key")), ktypes), vcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(vvar())), t, "map value")), vtypes), all_map_constraints := merge_class_constraints(key_constraints(), merge_class_constraints(k_elem_constraints, v_elem_constraints)), hydra.lib.eithers.bind(map_constraints(fcx5, cx, (lambda subst: yield_with_constraints(fcx5, cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.zip(kterms, vterms)))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar())), cast(hydra.core.Type, hydra.core.TypeVariable(vvar()))))), hydra.substitution.compose_type_subst_list((ksubst, vsubst, subst)), hydra.substitution.subst_in_class_constraints(subst, all_map_constraints))), hydra.lib.lists.concat((kcons, vcons))), (lambda mc_result: Right(mc_result))))[9])))[6]))))

def infer_type_of_optional(fcx: hydra.context.Context, cx: hydra.graph.Graph, m: Maybe[hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a Maybe value."""

    def trm_cons(terms: frozenlist[hydra.core.Term]) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(terms), (lambda : cast(hydra.core.Term, hydra.core.TermMaybe(Nothing()))), (lambda : cast(hydra.core.Term, hydra.core.TermMaybe(Just(hydra.lib.lists.head(terms))))))
    return infer_type_of_collection(fcx, cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), (lambda x1: trm_cons(x1)), "optional element", hydra.lib.sets.empty(), hydra.lib.maybes.maybe((lambda : ()), (lambda x1: hydra.lib.lists.singleton(x1)), m))

def infer_type_of_pair(fcx: hydra.context.Context, cx: hydra.graph.Graph, p: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a pair (Either version)."""

    return hydra.lib.eithers.bind(infer_many(fcx, cx, ((hydra.lib.pairs.first(p), "pair first element"), (hydra.lib.pairs.second(p), "pair second element"))), (lambda rp: (results := hydra.lib.pairs.first(rp), fcx2 := hydra.lib.pairs.second(rp), iterms := hydra.lib.pairs.first(results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), isubst := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), pair_elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), ifst := hydra.lib.lists.head(iterms), isnd := hydra.lib.lists.head(hydra.lib.lists.tail(iterms)), ty_fst := hydra.lib.lists.head(itypes), ty_snd := hydra.lib.lists.head(hydra.lib.lists.tail(itypes)), pair_term := cast(hydra.core.Term, hydra.core.TermPair((ifst, isnd))), term_with_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(pair_term, ty_fst))), ty_snd))), Right(yield_with_constraints(fcx2, term_with_types, cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(ty_fst, ty_snd))), isubst, pair_elem_constraints)))[12]))

def infer_type_of_record(fcx: hydra.context.Context, cx: hydra.graph.Graph, record: hydra.core.Record) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a record (Either version)."""

    tname = record.type_name
    fields = record.fields
    @lru_cache(1)
    def fnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), fields)
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx2 := hydra.lib.pairs.second(st_rp), hydra.lib.eithers.bind(infer_many(fcx2, cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat2("field ", f.name.value))), fields)), (lambda rp: (results := hydra.lib.pairs.first(rp), fcx3 := hydra.lib.pairs.second(rp), svars := schema_type.variables, stype := schema_type.type, iterms := hydra.lib.pairs.first(results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), isubst := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), rec_elem_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(results))), ityp := cast(hydra.core.Type, hydra.core.TypeRecord(hydra.lib.lists.zip_with((lambda n, t: hydra.core.FieldType(n, t)), fnames(), itypes))), hydra.lib.eithers.bind(map_constraints(fcx3, cx, (lambda subst: yield_with_constraints(fcx3, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames(), iterms))))), hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst), hydra.substitution.subst_in_class_constraints(subst, rec_elem_constraints))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of record"),)), (lambda mc_result: Right(mc_result))))[9])))[2]))

def infer_type_of_set(fcx: hydra.context.Context, cx: hydra.graph.Graph, s: frozenset[hydra.core.Term]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a set (Either version)."""

    return infer_type_of_collection(fcx, cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeSet(x))), (lambda terms: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(terms)))), "set element", hydra.lib.sets.singleton(hydra.core.Name("ordering")), hydra.lib.sets.to_list(s))

def infer_type_of_term(fcx: hydra.context.Context, cx: hydra.graph.Graph, term: hydra.core.Term, desc: str) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a given term (Either version)."""

    @lru_cache(1)
    def fcx2() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons(desc, fcx.trace), fcx.messages, fcx.other)
    match term:
        case hydra.core.TermAnnotated(value=a):
            return infer_type_of_annotated_term(fcx2(), cx, a)

        case hydra.core.TermApplication(value=a2):
            return infer_type_of_application(fcx2(), cx, a2)

        case hydra.core.TermEither(value=e):
            return infer_type_of_either(fcx2(), cx, e)

        case hydra.core.TermFunction(value=f):
            return infer_type_of_function(fcx2(), cx, f)

        case hydra.core.TermLet(value=l):
            return infer_type_of_let(fcx2(), cx, l)

        case hydra.core.TermList(value=els):
            return infer_type_of_list(fcx2(), cx, els)

        case hydra.core.TermLiteral(value=l2):
            return Right(infer_type_of_literal(fcx2(), l2))

        case hydra.core.TermMap(value=m):
            return infer_type_of_map(fcx2(), cx, m)

        case hydra.core.TermMaybe(value=m2):
            return infer_type_of_optional(fcx2(), cx, m2)

        case hydra.core.TermPair(value=p):
            return infer_type_of_pair(fcx2(), cx, p)

        case hydra.core.TermRecord(value=r):
            return infer_type_of_record(fcx2(), cx, r)

        case hydra.core.TermSet(value=s):
            return infer_type_of_set(fcx2(), cx, s)

        case hydra.core.TermTypeApplication(value=tt):
            return infer_type_of_type_application(fcx2(), cx, tt)

        case hydra.core.TermTypeLambda(value=ta):
            return infer_type_of_type_lambda(fcx2(), cx, ta)

        case hydra.core.TermUnion(value=i):
            return infer_type_of_injection(fcx2(), cx, i)

        case hydra.core.TermUnit():
            return Right(infer_type_of_unit(fcx2()))

        case hydra.core.TermVariable(value=name):
            return infer_type_of_variable(fcx2(), cx, name)

        case hydra.core.TermWrap(value=w):
            return infer_type_of_wrapped_term(fcx2(), cx, w)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def infer_type_of_type_application(fcx: hydra.context.Context, cx: hydra.graph.Graph, tt: hydra.core.TypeApplicationTerm) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a type application (Either version)."""

    return infer_type_of_term(fcx, cx, tt.body, "type application term")

def infer_type_of_type_lambda(fcx: hydra.context.Context, cx: hydra.graph.Graph, ta: hydra.core.TypeLambda) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a type abstraction (Either version)."""

    return infer_type_of_term(fcx, cx, ta.body, "type abstraction")

def infer_type_of_wrapped_term(fcx: hydra.context.Context, cx: hydra.graph.Graph, wt: hydra.core.WrappedTerm) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a wrapped term (Either version)."""

    tname = wt.type_name
    term = wt.body
    return hydra.lib.eithers.bind(hydra.resolution.require_schema_type(fcx, cx.schema_types, tname), (lambda st_rp: (schema_type := hydra.lib.pairs.first(st_rp), fcx2 := hydra.lib.pairs.second(st_rp), hydra.lib.eithers.bind(infer_type_of_term(fcx2, cx, term, "wrapped term"), (lambda result: (fcx3 := result.context, svars := schema_type.variables, stype := schema_type.type, iterm := result.term, itype := result.type, isubst := result.subst, ityp := cast(hydra.core.Type, hydra.core.TypeWrap(itype)), hydra.lib.eithers.bind(map_constraints(fcx3, cx, (lambda subst: yield_(fcx3, build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, iterm)))), hydra.resolution.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper"),)), (lambda mc_result: Right(mc_result))))[7])))[2]))

def infer_types_of_temporary_bindings(fcx: hydra.context.Context, cx: hydra.graph.Graph, bins: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], tuple[hydra.typing.TypeSubst, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]], hydra.context.Context]]:
    r"""Infer types for temporary let bindings (Either version)."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(bins), (lambda : Right((((), ((), (hydra.substitution.id_type_subst(), hydra.lib.maps.empty()))), fcx))), (lambda : (dflt := (binding := hydra.lib.lists.head(bins), (k := binding.name, (v := binding.term, (tl := hydra.lib.lists.tail(bins), hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, v, hydra.lib.strings.cat(("temporary let binding '", k.value, "'"))), (lambda result1: (fcx2 := result1.context, j := result1.term, u_prime := result1.type, u := result1.subst, c1_inferred := result1.class_constraints, hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(hydra.lib.maps.empty())), (lambda ts: (ts_result := hydra.resolution.instantiate_type_scheme(fcx2, ts), instantiated_ts := hydra.lib.pairs.first(ts_result), fresh_constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), instantiated_ts.constraints), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_ic.object.message))), _ic.context)), (lambda _a: _a), hydra.unification.unify_types(fcx2, cx.schema_types, instantiated_ts.type, u_prime, "original binding type")), (lambda unify_subst: Right(hydra.substitution.subst_in_class_constraints(unify_subst, fresh_constraints)))))[3]), binding.type), (lambda original_binding_constraints: (c1 := merge_class_constraints(c1_inferred, original_binding_constraints), hydra.lib.eithers.bind(infer_types_of_temporary_bindings(fcx2, hydra.substitution.subst_in_context(u, cx), tl), (lambda rp2: (result2 := hydra.lib.pairs.first(rp2), fcx3 := hydra.lib.pairs.second(rp2), h := hydra.lib.pairs.first(result2), r_prime := hydra.lib.pairs.first(hydra.lib.pairs.second(result2)), rest_pair := hydra.lib.pairs.second(hydra.lib.pairs.second(result2)), r := hydra.lib.pairs.first(rest_pair), c2 := hydra.lib.pairs.second(rest_pair), c1_subst := hydra.substitution.subst_in_class_constraints(r, c1), merged_constraints := merge_class_constraints(c1_subst, c2), Right(((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(r, j), h), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(r, u_prime), r_prime), (hydra.substitution.compose_type_subst(u, r), merged_constraints))), fcx3)))[9])))[1])))[5])))[1])[1])[1])[1], dflt)[1]))

def for_inferred_term(fcx: hydra.context.Context, cx: hydra.graph.Graph, term: hydra.core.Term, desc: str, f: Callable[[hydra.typing.InferenceResult], T0]) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[T0, hydra.context.Context]]:
    r"""Infer a term's type and map over the result."""

    return hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, term, desc), (lambda rp: Right((f(rp), rp.context))))

def infer_graph_types(fcx0: hydra.context.Context, bindings0: frozenlist[hydra.core.Binding], g0: hydra.graph.Graph):
    r"""Infer types for all elements in a graph, using the provided ordered bindings. Returns both the inferred graph and the ordered inferred bindings."""

    @lru_cache(1)
    def fcx() -> hydra.context.Context:
        return hydra.context.Context(hydra.lib.lists.cons("graph inference", fcx0.trace), fcx0.messages, fcx0.other)
    @lru_cache(1)
    def let0() -> hydra.core.Let:
        return hydra.core.Let(bindings0, cast(hydra.core.Term, hydra.core.TermUnit()))
    def from_let_term(l: hydra.core.Let) -> tuple[hydra.graph.Graph, frozenlist[hydra.core.Binding]]:
        bindings = l.bindings
        prims = g0.primitives
        schema_types = g0.schema_types
        @lru_cache(1)
        def raw_g() -> hydra.graph.Graph:
            return hydra.lexical.build_graph(bindings, hydra.lib.maps.empty(), prims)
        g = hydra.graph.Graph(raw_g().bound_terms, raw_g().bound_types, raw_g().class_constraints, raw_g().lambda_variables, raw_g().metadata, raw_g().primitives, schema_types, raw_g().type_variables)
        return (g, bindings)
    return hydra.lib.eithers.bind(infer_type_of_term(fcx(), g0, cast(hydra.core.Term, hydra.core.TermLet(let0())), "graph term"), (lambda result: (fcx2 := result.context, term := result.term, _hoist_term_body_1 := (lambda v1: (lambda l: Right((from_let_term(l), fcx2)))(v1.value) if isinstance(v1, hydra.core.TermLet) else (lambda _: Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("Expected inferred graph as let term"))), fcx2)))(v1.value) if isinstance(v1, hydra.core.TermVariable) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), hydra.lib.eithers.bind(finalize_inferred_term(fcx2, g0, term), (lambda finalized: _hoist_term_body_1(finalized))))[3]))

def infer_in_graph_context(fcx: hydra.context.Context, cx: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a term in a given inference context."""

    return infer_type_of_term(fcx, cx, term, "single term")

def infer_type_of(fcx: hydra.context.Context, cx: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[tuple[hydra.core.Term, hydra.core.TypeScheme], hydra.context.Context]]:
    r"""Map a possibly untyped term to a fully typed term and its type."""

    @lru_cache(1)
    def let_term() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((hydra.core.Binding(hydra.core.Name("ignoredVariableName"), term, Nothing()),), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("ignoredBody")))))))
    return hydra.lib.eithers.bind(infer_type_of_term(fcx, cx, let_term(), "infer type of term"), (lambda result: (fcx2 := result.context, hydra.lib.eithers.bind(finalize_inferred_term(fcx2, cx, result.term), (lambda finalized: hydra.lib.eithers.bind(hydra.extract.core.let(fcx2, cx, finalized), (lambda let_result: (bindings := let_result.bindings, hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(bindings)), (lambda : (binding := hydra.lib.lists.head(bindings), (term1 := binding.term, (mts := binding.type, hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("Expected a type scheme"))), fcx2))), (lambda ts: Right(((term1, ts), fcx2))), mts))[1])[1])[1]), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("Expected a single binding with a type scheme, but got: ", hydra.lib.literals.show_int32(hydra.lib.lists.length(bindings)), " bindings"))))), fcx2)))))[1])))))[1]))

def infer_type_of_primitive(fcx: hydra.context.Context, cx: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Infer the type of a primitive function (Either version)."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("No such primitive: ", name.value)))), fcx))), (lambda scheme: (ts_result := hydra.resolution.instantiate_type_scheme(fcx, scheme), ts := hydra.lib.pairs.first(ts_result), fcx2 := hydra.lib.pairs.second(ts_result), constraints := hydra.lib.maybes.from_maybe((lambda : hydra.lib.maps.empty()), ts.constraints), Right(yield_checked_with_constraints(fcx2, build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name))), ts.type, hydra.substitution.id_type_subst(), constraints)))[4]), hydra.lib.maybes.map((lambda v1: v1.type), hydra.lib.maps.lookup(name, cx.primitives)))

def show_inference_result(result: hydra.typing.InferenceResult) -> str:
    r"""Show an inference result for debugging."""

    term = result.term
    typ = result.type
    subst = result.subst
    return hydra.lib.strings.cat(("{term=", hydra.show.core.term(term), ", type=", hydra.show.core.type(typ), ", subst=", hydra.show.typing.type_subst(subst), "}"))

def yield_debug(fcx: hydra.context.Context, cx: T0, debug_id: str, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult]:
    r"""Create an inference result with debug output."""

    @lru_cache(1)
    def rterm() -> hydra.core.Term:
        return hydra.substitution.subst_types_in_term(subst, term)
    @lru_cache(1)
    def rtyp() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    return hydra.lib.eithers.bind(hydra.annotations.debug_if(fcx, debug_id, hydra.lib.strings.cat(("\n\tterm: ", hydra.show.core.term(term), "\n\ttyp: ", hydra.show.core.type(typ), "\n\tsubst: ", hydra.show.typing.type_subst(subst), "\n\trterm: ", hydra.show.core.term(rterm()), "\n\trtyp: ", hydra.show.core.type(rtyp())))), (lambda result: Right(hydra.typing.InferenceResult(rterm(), rtyp(), subst, hydra.lib.maps.empty(), fcx))))
