# Note: this is an automatically generated file. Do not edit.

r"""Type inference following Algorithm W, extended for nominal terms and types."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.checking
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
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
import hydra.monads
import hydra.reflect
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.show.typing
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.unification

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def bind_constraints(cx: hydra.typing.InferenceContext, f: Callable[[hydra.typing.TypeSubst], hydra.compute.Flow[T0, T1]], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T0, T1]:
    return hydra.lib.flows.bind(hydra.unification.unify_type_constraints(cx.schema_types, constraints), (lambda s: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s), (lambda _: f(s)))))

def bind_unbound_type_variables(cx: hydra.typing.InferenceContext, term0: hydra.core.Term) -> hydra.core.Type:
    r"""Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding. These variables may appear in the binding type scheme itself or in that of a subterm, in domain types attached to functions, and in type abstraction and type application terms. This process attempts to capture type variables which have escaped unification, e.g. due to unused code. However, unbound type variables not appearing beneath any typed let binding remain unbound."""
    
    def svars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(cx.schema_types))
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
        match term:
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> hydra.core.Type:
                    def bname() -> hydra.core.Type:
                        return b.name
                    def bterm() -> hydra.core.Type:
                        return b.term
                    return hydra.lib.maybes.maybe(hydra.core.Binding(bname(), bind_unbound_type_variables(cx, bterm()), Nothing()), (lambda ts: (bvars := hydra.lib.sets.from_list(ts.variables), unbound_in_type := hydra.rewriting.free_variables_in_type(ts.type), unbound_in_term := hydra.rewriting.free_type_variables_in_term(bterm()), unbound := hydra.lib.sets.to_list(hydra.lib.sets.difference(hydra.lib.sets.union(unbound_in_type, unbound_in_term), hydra.lib.sets.union(svars(), bvars))), ts2 := hydra.core.TypeScheme(hydra.lib.lists.concat2(ts.variables, unbound), ts.type, ts.constraints), bterm2 := hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, t)))), bterm(), unbound), hydra.core.Binding(bname(), bterm2, Just(ts2)))[6]), b.type)
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(for_binding, l.bindings), bind_unbound_type_variables(cx, l.body))))
            
            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term(rewrite, term0)

def build_type_application_term(tvars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Type:
    r"""Fold a list of type variables over a term to build a type application term."""
    
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, cast(hydra.core.Type, hydra.core.TypeVariable(v)))))), body, tvars)

def empty_inference_context() -> hydra.core.Type:
    r"""An empty inference context."""
    
    return hydra.typing.InferenceContext(FrozenDict({}), FrozenDict({}), FrozenDict({}), FrozenDict({}), False)

def extend_context(pairs: frozenlist[tuple[hydra.core.Name, hydra.core.TypeScheme]], cx: hydra.typing.InferenceContext) -> hydra.core.Type:
    r"""Add (term variable, type scheme) pairs to the typing environment."""
    
    return hydra.typing.InferenceContext(cx.schema_types, cx.primitive_types, hydra.lib.maps.union(hydra.lib.maps.from_list(pairs), cx.data_types), cx.class_constraints, cx.debug)

def finalize_inferred_term(cx: hydra.typing.InferenceContext, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def term2() -> hydra.core.Type:
        return bind_unbound_type_variables(cx, term)
    return hydra.lib.flows.bind(hydra.checking.check_for_unbound_type_variables(cx, term2()), (lambda _: hydra.lib.flows.pure(hydra.rewriting.normalize_type_variables_in_term(term2()))))

def merge_class_constraints(m1: FrozenDict[T0, hydra.core.TypeVariableMetadata], m2: FrozenDict[T0, hydra.core.TypeVariableMetadata]) -> FrozenDict[T0, hydra.core.TypeVariableMetadata]:
    return hydra.lib.lists.foldl((lambda acc, pair: (k := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.maybes.maybe(hydra.lib.maps.insert(k, v, acc), (lambda existing: (merged := hydra.core.TypeVariableMetadata(hydra.lib.sets.union(existing.classes, v.classes)), hydra.lib.maps.insert(k, merged, acc))[1]), hydra.lib.maps.lookup(k, acc)))[2]), m1, hydra.lib.maps.to_list(m2))

def fresh_variable_type() -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), hydra.schemas.fresh_name())

def yield_checked(term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def iterm() -> hydra.core.Type:
        return hydra.substitution.subst_types_in_term(subst, term)
    def itype() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    return hydra.lib.flows.pure(hydra.typing.InferenceResult(iterm(), itype(), subst, hydra.lib.maps.empty()))

def map_constraints(cx: hydra.typing.InferenceContext, f: Callable[[hydra.typing.TypeSubst], T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, T0]:
    return hydra.lib.flows.bind(hydra.unification.unify_type_constraints(cx.schema_types, constraints), (lambda s: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s), (lambda _: hydra.lib.flows.pure(f(s))))))

def yield_(term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.core.Type:
    r"""Create an inference result with no class constraints."""
    
    return hydra.typing.InferenceResult(hydra.substitution.subst_types_in_term(subst, term), hydra.substitution.subst_in_type(subst, typ), subst, hydra.lib.maps.empty())

def infer_type_of_projection(cx: hydra.typing.InferenceContext, proj: hydra.core.Projection) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def tname() -> hydra.core.Type:
        return proj.type_name
    def fname() -> hydra.core.Type:
        return proj.field
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname()), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.record_type(tname(), stype), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname(), sfields), (lambda ftyp: hydra.lib.flows.pure(yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(tname(), fname())))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), ftyp))), hydra.substitution.id_type_subst())))))))[2]))

def infer_type_of_unwrap(cx: hydra.typing.InferenceContext, tname: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.wrapped_type(tname, stype), (lambda wtyp: hydra.lib.flows.pure(yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(tname))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), wtyp))), hydra.substitution.id_type_subst())))))[2]))

def free_variables_in_context(cx: hydra.typing.InferenceContext) -> frozenset[hydra.core.Name]:
    r"""Get all free variables in an inference context."""
    
    return hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), hydra.lib.lists.map(hydra.rewriting.free_variables_in_type_scheme_simple, hydra.lib.maps.elems(cx.data_types)))

def yield_checked_with_constraints(term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst, constraints: FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def iterm() -> hydra.core.Type:
        return hydra.substitution.subst_types_in_term(subst, term)
    def itype() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    def iconstraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.substitution.subst_in_class_constraints(subst, constraints)
    return hydra.lib.flows.pure(hydra.typing.InferenceResult(iterm(), itype(), subst, iconstraints()))

def infer_type_of_primitive(cx: hydra.typing.InferenceContext, name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("No such primitive: ", name.value)), (lambda scheme: hydra.lib.flows.bind(hydra.schemas.instantiate_type_scheme(scheme), (lambda ts: (constraints := hydra.lib.maybes.from_maybe(hydra.lib.maps.empty(), ts.constraints), yield_checked_with_constraints(build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))))), ts.type, hydra.substitution.id_type_subst(), constraints))[1]))), hydra.lib.maps.lookup(name, cx.primitive_types))

def is_unbound(cx: hydra.typing.InferenceContext, v: hydra.core.Name) -> bool:
    r"""Check if a variable is unbound in context."""
    
    return hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_context(cx))), hydra.lib.logic.not_(hydra.lib.maps.member(v, cx.schema_types)))

def generalize(cx: hydra.typing.InferenceContext, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Generalize a type to a type scheme."""
    
    def vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.filter((lambda v1: is_unbound(cx, v1)), hydra.rewriting.free_variables_in_type_ordered(typ)))
    def all_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return cx.class_constraints
    def relevant_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda v: hydra.lib.maybes.map((lambda meta: (v, meta)), hydra.lib.maps.lookup(v, all_constraints()))), vars())))
    def constraints_maybe() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(relevant_constraints()), (lambda : Nothing()), (lambda : Just(relevant_constraints())))
    return hydra.core.TypeScheme(vars(), typ, constraints_maybe())

def infer_type_of_literal(_: T0, lit: hydra.core.Literal) -> hydra.compute.Flow[T1, hydra.typing.InferenceResult]:
    return hydra.lib.flows.pure(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLiteral(lit)), cast(hydra.core.Type, hydra.core.TypeLiteral(hydra.reflect.literal_type(lit))), hydra.substitution.id_type_subst(), hydra.lib.maps.empty()))

def infer_type_of_unit() -> hydra.core.Type:
    r"""The trivial inference rule for the unit term."""
    
    return hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermUnit()), cast(hydra.core.Type, hydra.core.TypeUnit()), hydra.substitution.id_type_subst(), hydra.lib.maps.empty())

def infer_type_of_variable(cx: hydra.typing.InferenceContext, name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("Variable not bound to type: ", name.value)), (lambda scheme: hydra.lib.flows.bind(hydra.schemas.instantiate_type_scheme(scheme), (lambda ts: (constraints := hydra.lib.maybes.from_maybe(hydra.lib.maps.empty(), ts.constraints), hydra.lib.flows.pure(hydra.typing.InferenceResult(build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name))), ts.type, hydra.substitution.id_type_subst(), constraints)))[1]))), hydra.lib.maps.lookup(name, cx.data_types))

def infer_many(cx: hydra.typing.InferenceContext, pairs: frozenlist[tuple[hydra.core.Term, str]]) -> hydra.compute.Flow[T0, tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
    def dflt() -> hydra.compute.Flow[T0, tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
        def e() -> hydra.core.Type:
            return hydra.lib.pairs.first(hydra.lib.lists.head(pairs))
        def desc() -> str:
            return hydra.lib.pairs.second(hydra.lib.lists.head(pairs))
        def tl() -> frozenlist[tuple[hydra.core.Term, str]]:
            return hydra.lib.lists.tail(pairs)
        return hydra.lib.flows.bind(infer_type_of_term(cx, e(), desc()), (lambda result1: (e1 := result1.term, t1 := result1.type, s1 := result1.subst, hydra.lib.flows.bind(infer_many(hydra.substitution.subst_in_context(s1, cx), tl()), (lambda result2: (e2 := hydra.lib.pairs.first(result2), t2 := hydra.lib.pairs.first(hydra.lib.pairs.second(result2)), s2 := hydra.lib.pairs.second(hydra.lib.pairs.second(result2)), hydra.lib.flows.pure((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(s2, e1), e2), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(s2, t1), t2), hydra.substitution.compose_type_subst(s1, s2)))))[3])))[3]))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(pairs), (lambda : hydra.lib.flows.pure(((), ((), hydra.substitution.id_type_subst())))), (lambda : dflt()))

def infer_type_of_annotated_term(cx: hydra.typing.InferenceContext, at: hydra.core.AnnotatedTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def term() -> hydra.core.Type:
        return at.body
    def ann() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return at.annotation
    return hydra.lib.flows.bind(infer_type_of_term(cx, term(), "annotated term"), (lambda result: (iterm := result.term, itype := result.type, isubst := result.subst, iconstraints := result.class_constraints, hydra.lib.flows.pure(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(iterm, ann()))), itype, isubst, iconstraints)))[4]))

def infer_type_of_application(cx: hydra.typing.InferenceContext, app: hydra.core.Application) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def e0() -> hydra.core.Type:
        return app.function
    def e1() -> hydra.core.Type:
        return app.argument
    return hydra.lib.flows.bind(infer_type_of_term(cx, e0(), "lhs"), (lambda lhs_result: (a := lhs_result.term, t0 := lhs_result.type, s0 := lhs_result.subst, c0 := lhs_result.class_constraints, hydra.lib.flows.bind(infer_type_of_term(hydra.substitution.subst_in_context(s0, cx), e1(), "rhs"), (lambda rhs_result: (b := rhs_result.term, t1 := rhs_result.type, s1 := rhs_result.subst, c1 := rhs_result.class_constraints, hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda v: hydra.lib.flows.bind(hydra.unification.unify_types(cx.schema_types, hydra.substitution.subst_in_type(s1, t0), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(t1, cast(hydra.core.Type, hydra.core.TypeVariable(v))))), "application lhs"), (lambda s2: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s2), (lambda _: (r_expr := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(s1, s2), a), hydra.substitution.subst_types_in_term(s2, b)))), r_type := hydra.substitution.subst_in_type(s2, cast(hydra.core.Type, hydra.core.TypeVariable(v))), r_subst := hydra.substitution.compose_type_subst_list((s0, s1, s2)), c0_subst := hydra.substitution.subst_in_class_constraints(s2, hydra.substitution.subst_in_class_constraints(s1, c0)), c1_subst := hydra.substitution.subst_in_class_constraints(s2, c1), r_constraints := merge_class_constraints(c0_subst, c1_subst), hydra.lib.flows.pure(hydra.typing.InferenceResult(r_expr, r_type, r_subst, r_constraints)))[6])))))))[4])))[4]))

def infer_type_of_case_statement(cx: hydra.typing.InferenceContext, case_stmt: hydra.core.CaseStatement) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def tname() -> hydra.core.Type:
        return case_stmt.type_name
    def dflt() -> Maybe[hydra.core.Term]:
        return case_stmt.default
    def cases() -> frozenlist[hydra.core.Field]:
        return case_stmt.cases
    def fnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), cases())
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname()), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.union_type(tname(), stype), (lambda sfields: hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda t: infer_type_of_term(cx, t, hydra.lib.strings.cat(("case ", tname().value, ".<default>")))), dflt()), (lambda dflt_result: hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat(("case ", tname().value, ".", f.name.value)))), cases())), (lambda case_results: (iterms := hydra.lib.pairs.first(case_results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(case_results)), isubst := hydra.lib.pairs.second(hydra.lib.pairs.second(case_results)), hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda codv: (cod := cast(hydra.core.Type, hydra.core.TypeVariable(codv)), case_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), sfields)), dflt_constraints := hydra.monads.maybe_to_list(hydra.lib.maybes.map((lambda r: hydra.typing.TypeConstraint(cod, r.type, "match default")), dflt_result)), case_constraints := hydra.lib.maybes.cat(hydra.lib.lists.zip_with((lambda fname, itype: hydra.lib.maybes.map((lambda ftype: hydra.typing.TypeConstraint(itype, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(ftype, cod))), "case type")), hydra.lib.maps.lookup(fname, case_map))), fnames(), itypes)), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(tname(), hydra.lib.maybes.map((lambda v1: v1.term), dflt_result), hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames(), iterms))))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), cod))), hydra.substitution.compose_type_subst_list(hydra.lib.lists.concat((hydra.monads.maybe_to_list(hydra.lib.maybes.map((lambda v1: v1.subst), dflt_result)), (isubst, subst)))))), hydra.lib.lists.concat((dflt_constraints, case_constraints))))[4])))[3])))))))[2]))

def infer_type_of_collection(cx: hydra.typing.InferenceContext, typ_cons: Callable[[hydra.core.Type], hydra.core.Type], trm_cons: Callable[[frozenlist[hydra.core.Term]], hydra.core.Term], desc: str, els: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda var: hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : hydra.lib.flows.pure(yield_(build_type_application_term((var,), trm_cons(())), typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var))), hydra.substitution.id_type_subst()))), (lambda : hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.zip(els, hydra.lib.lists.map((lambda i: hydra.lib.strings.cat(("#", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, hydra.lib.math.add(hydra.lib.lists.length(els), 1))))), (lambda results: (terms := hydra.lib.pairs.first(results), types := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), subst1 := hydra.lib.pairs.second(hydra.lib.pairs.second(results)), constraints := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(var)), t, desc)), types), map_constraints(cx, (lambda subst2: (iterm := trm_cons(terms), itype := typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var))), isubst := hydra.substitution.compose_type_subst(subst1, subst2), yield_(iterm, itype, isubst))[3]), constraints))[4]))))))

def infer_type_of_either(cx: hydra.typing.InferenceContext, e: Either[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.eithers.either((lambda l: hydra.lib.flows.bind(infer_type_of_term(cx, l, "either left value"), (lambda r1: (iterm := r1.term, left_type := r1.type, subst := r1.subst, hydra.lib.flows.bind(fresh_variable_type(), (lambda right_type: (either_term := cast(hydra.core.Term, hydra.core.TermEither(Left(iterm))), term_with_left_type := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(either_term, left_type))), term_with_both_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(term_with_left_type, right_type))), either_type := cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, right_type))), yield_checked(term_with_both_types, either_type, subst))[4])))[3]))), (lambda r: hydra.lib.flows.bind(infer_type_of_term(cx, r, "either right value"), (lambda r1: (iterm := r1.term, right_type := r1.type, subst := r1.subst, hydra.lib.flows.bind(fresh_variable_type(), (lambda left_type: (either_term := cast(hydra.core.Term, hydra.core.TermEither(Right(iterm))), term_with_left_type := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(either_term, left_type))), term_with_both_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(term_with_left_type, right_type))), either_type := cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, right_type))), yield_checked(term_with_both_types, either_type, subst))[4])))[3]))), e)

def infer_type_of_elimination(cx: hydra.typing.InferenceContext, elm: hydra.core.Elimination) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    match elm:
        case hydra.core.EliminationRecord(value=p):
            return infer_type_of_projection(cx, p)
        
        case hydra.core.EliminationUnion(value=c):
            return infer_type_of_case_statement(cx, c)
        
        case hydra.core.EliminationWrap(value=tname):
            return infer_type_of_unwrap(cx, tname)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def infer_type_of_function(cx: hydra.typing.InferenceContext, f: hydra.core.Function) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    match f:
        case hydra.core.FunctionElimination(value=elm):
            return infer_type_of_elimination(cx, elm)
        
        case hydra.core.FunctionLambda(value=l):
            return infer_type_of_lambda(cx, l)
        
        case hydra.core.FunctionPrimitive(value=name):
            return infer_type_of_primitive(cx, name)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def infer_type_of_injection(cx: hydra.typing.InferenceContext, injection: hydra.core.Injection) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def tname() -> hydra.core.Type:
        return injection.type_name
    def field() -> hydra.core.Type:
        return injection.field
    def fname() -> hydra.core.Type:
        return field().name
    def term() -> hydra.core.Type:
        return field().term
    return hydra.lib.flows.bind(infer_type_of_term(cx, term(), "injected term"), (lambda result: hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname()), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, iterm := result.term, ityp := result.type, isubst := result.subst, hydra.lib.flows.bind(hydra.extract.core.union_type(tname(), stype), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname(), sfields), (lambda ftyp: map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(tname(), hydra.core.Field(fname(), iterm))))), hydra.schemas.nominal_application(tname(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field"),)))))))[5]))))

def infer_type_of_lambda(cx: hydra.typing.InferenceContext, lambda_: hydra.core.Lambda) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def var() -> hydra.core.Type:
        return lambda_.parameter
    def body() -> hydra.core.Type:
        return lambda_.body
    return hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda vdom: (dom := cast(hydra.core.Type, hydra.core.TypeVariable(vdom)), cx2 := extend_context(((var(), hydra.core.TypeScheme((), dom, Nothing())),), cx), hydra.lib.flows.bind(infer_type_of_term(cx2, body(), "lambda body"), (lambda result: (iterm := result.term, icod := result.type, isubst := result.subst, rdom := hydra.substitution.subst_in_type(isubst, dom), rterm := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var(), Just(rdom), iterm))))), rtype := cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(rdom, icod))), vars := hydra.lib.sets.unions((hydra.rewriting.free_variables_in_type(rdom), hydra.rewriting.free_variables_in_type(icod), free_variables_in_context(hydra.substitution.subst_in_context(isubst, cx2)))), cx3 := hydra.substitution.subst_in_context(isubst, cx), iconstraints := hydra.substitution.subst_in_class_constraints(isubst, result.class_constraints), hydra.lib.flows.pure(hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints)))[9])))[2]))

def infer_type_of_let(cx: hydra.typing.InferenceContext, let0: hydra.core.Let) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def bindings0() -> frozenlist[hydra.core.Binding]:
        return let0.bindings
    def body0() -> hydra.core.Type:
        return let0.body
    def names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bindings0())
    def name_set() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(names())
    def to_pair(binding: hydra.core.Binding) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        def name() -> hydra.core.Type:
            return binding.name
        def term() -> hydra.core.Type:
            return binding.term
        return (name(), hydra.lib.lists.filter((lambda n: hydra.lib.sets.member(n, name_set())), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_term(term()))))
    def adj_list() -> frozenlist[tuple[hydra.core.Name, frozenlist[hydra.core.Name]]]:
        return hydra.lib.lists.map(to_pair, bindings0())
    def groups() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(adj_list())
    def binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
        return hydra.lib.maps.from_list(hydra.lib.lists.zip(names(), bindings0()))
    def create_let(e: hydra.core.Term, group: frozenlist[hydra.core.Name]) -> hydra.core.Type:
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map())), group)), e)))
    def rewritten_let() -> hydra.core.Type:
        return hydra.lib.lists.foldl(create_let, body0(), hydra.lib.lists.reverse(groups()))
    def restore_let(iterm: hydra.core.Term) -> hydra.core.Type:
        def helper(level: int, bins: frozenlist[hydra.core.Binding], term: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
            def nonzero(term2: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                match term2:
                    case hydra.core.TermLet(value=l):
                        def bs() -> frozenlist[hydra.core.Binding]:
                            return l.bindings
                        def e() -> hydra.core.Type:
                            return l.body
                        return helper(hydra.lib.math.sub(level, 1), hydra.lib.lists.concat((bs(), bins)), e())
                    
                    case _:
                        raise TypeError("Unsupported Term")
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(level, 0), (lambda : (bins, term)), (lambda : nonzero(term)))
        def result() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
            return helper(hydra.lib.lists.length(groups()), (), iterm)
        def binding_list() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.first(result())
        def e() -> hydra.core.Type:
            return hydra.lib.pairs.second(result())
        def binding_map2() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b)), binding_list()))
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map2())), names())), e())))
    def rewrite_result(result: hydra.typing.InferenceResult) -> hydra.core.Type:
        def iterm() -> hydra.core.Type:
            return result.term
        def itype() -> hydra.core.Type:
            return result.type
        def isubst() -> hydra.core.Type:
            return result.subst
        def iconstraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
            return result.class_constraints
        return hydra.typing.InferenceResult(restore_let(iterm()), itype(), isubst(), iconstraints())
    def res() -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
        match rewritten_let():
            case hydra.core.TermLet(value=l):
                return infer_type_of_let_normalized(cx, l)
            
            case _:
                return infer_type_of_term(cx, rewritten_let(), "empty let term")
    return hydra.lib.flows.map(rewrite_result, res())

def infer_type_of_let_normalized(cx0: hydra.typing.InferenceContext, let_term: hydra.core.Let) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def bins0() -> frozenlist[hydra.core.Binding]:
        return let_term.bindings
    def body0() -> hydra.core.Type:
        return let_term.body
    def bnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bins0())
    return hydra.lib.flows.bind(hydra.schemas.fresh_names(hydra.lib.lists.length(bins0())), (lambda bvars: (tbins0 := hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), bvars), cx1 := extend_context(hydra.lib.lists.zip(bnames(), hydra.lib.lists.map((lambda t: hydra.core.TypeScheme((), t, Nothing())), tbins0)), cx0), hydra.lib.flows.bind(infer_types_of_temporary_bindings(cx1, bins0()), (lambda inferred_result: (bterms1 := hydra.lib.pairs.first(inferred_result), tbins1 := hydra.lib.pairs.first(hydra.lib.pairs.second(inferred_result)), subst_and_constraints := hydra.lib.pairs.second(hydra.lib.pairs.second(inferred_result)), s1 := hydra.lib.pairs.first(subst_and_constraints), inferred_constraints := hydra.lib.pairs.second(subst_and_constraints), hydra.lib.flows.bind(hydra.unification.unify_type_lists(cx0.schema_types, hydra.lib.lists.map((lambda v1: hydra.substitution.subst_in_type(s1, v1)), tbins0), tbins1, "temporary type bindings"), (lambda s2: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx0, s2), (lambda _: (g2base := hydra.substitution.subst_in_context(hydra.substitution.compose_type_subst(s1, s2), cx0), constraints_with_s2 := hydra.substitution.subst_in_class_constraints(s2, inferred_constraints), composed_subst := hydra.substitution.compose_type_subst(s1, s2), original_binding_constraints := hydra.lib.lists.foldl((lambda acc, b: hydra.lib.maybes.maybe(acc, (lambda ts: hydra.lib.maybes.maybe(acc, (lambda c: merge_class_constraints(acc, c)), ts.constraints)), b.type)), hydra.lib.maps.empty(), bins0()), original_constraints_subst := hydra.substitution.subst_in_class_constraints(composed_subst, original_binding_constraints), all_inferred_constraints := merge_class_constraints(constraints_with_s2, original_constraints_subst), merged_constraints := merge_class_constraints(g2base.class_constraints, all_inferred_constraints), g2 := hydra.typing.InferenceContext(g2base.schema_types, g2base.primitive_types, g2base.data_types, merged_constraints, g2base.debug), bterms1_subst := hydra.lib.lists.map((lambda v1: hydra.substitution.subst_types_in_term(s2, v1)), bterms1), tsbins1 := hydra.lib.lists.zip(bnames(), hydra.lib.lists.map((lambda t: generalize(g2, hydra.substitution.subst_in_type(s2, t))), tbins1)), hydra.lib.flows.bind(infer_type_of_term(extend_context(tsbins1, g2), body0(), "let body"), (lambda body_result: (body1 := body_result.term, tbody := body_result.type, sbody := body_result.subst, st1 := hydra.typing.TermSubst(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda pair: (name := hydra.lib.pairs.first(pair), ts := hydra.lib.pairs.second(pair), (name, build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name)))))[2]), tsbins1))), create_binding := (lambda binding_pair: (name_ts_pair := hydra.lib.pairs.first(binding_pair), term := hydra.lib.pairs.second(binding_pair), name := hydra.lib.pairs.first(name_ts_pair), ts := hydra.lib.pairs.second(name_ts_pair), type_lambda_term := hydra.lib.lists.foldl((lambda b, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, b)))), hydra.substitution.substitute_in_term(st1, term), hydra.lib.lists.reverse(ts.variables)), final_ts := hydra.substitution.subst_in_type_scheme(sbody, ts), hydra.core.Binding(name, hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(sbody, s2), type_lambda_term), Just(final_ts)))[6]), bins1 := hydra.lib.lists.map(create_binding, hydra.lib.lists.zip(tsbins1, bterms1_subst)), body_constraints := hydra.substitution.subst_in_class_constraints(sbody, body_result.class_constraints), binding_constraints_subst := hydra.substitution.subst_in_class_constraints(sbody, constraints_with_s2), all_constraints := merge_class_constraints(binding_constraints_subst, body_constraints), ret := hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins1, body1))), tbody, hydra.substitution.compose_type_subst_list((s1, s2, sbody)), all_constraints), hydra.lib.flows.pure(ret))[10])))[10])))))[5])))[2]))

def infer_type_of_list(cx: hydra.typing.InferenceContext, v1: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), "list element", v1)

def infer_type_of_map(cx: hydra.typing.InferenceContext, m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda kvar: hydra.lib.flows.bind(hydra.schemas.fresh_name(), (lambda vvar: hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : hydra.lib.flows.pure(yield_(build_type_application_term((kvar, vvar), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.empty()))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), cast(hydra.core.Type, hydra.core.TypeVariable(vvar))))), hydra.substitution.id_type_subst()))), (lambda : hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda k: (k, "map key")), hydra.lib.maps.keys(m))), (lambda kresults: (kterms := hydra.lib.pairs.first(kresults), ktypes := hydra.lib.pairs.first(hydra.lib.pairs.second(kresults)), ksubst := hydra.lib.pairs.second(hydra.lib.pairs.second(kresults)), hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda v: (v, "map value")), hydra.lib.maps.elems(m))), (lambda vresults: (vterms := hydra.lib.pairs.first(vresults), vtypes := hydra.lib.pairs.first(hydra.lib.pairs.second(vresults)), vsubst := hydra.lib.pairs.second(hydra.lib.pairs.second(vresults)), kcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), t, "map key")), ktypes), vcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(vvar)), t, "map value")), vtypes), map_constraints(cx, (lambda subst: yield_(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.zip(kterms, vterms)))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), cast(hydra.core.Type, hydra.core.TypeVariable(vvar))))), hydra.substitution.compose_type_subst_list((ksubst, vsubst, subst)))), hydra.lib.lists.concat((kcons, vcons))))[5])))[3]))))))))

def infer_type_of_optional(cx: hydra.typing.InferenceContext, m: Maybe[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def trm_cons(terms: frozenlist[hydra.core.Term]) -> hydra.core.Type:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(terms), (lambda : cast(hydra.core.Term, hydra.core.TermMaybe(Nothing()))), (lambda : cast(hydra.core.Term, hydra.core.TermMaybe(Just(hydra.lib.lists.head(terms))))))
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), trm_cons, "optional element", hydra.lib.maybes.maybe((), (lambda x1: hydra.lib.lists.singleton(x1)), m))

def infer_type_of_pair(cx: hydra.typing.InferenceContext, p: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.map((lambda results: (iterms := hydra.lib.pairs.first(results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), isubst := hydra.lib.pairs.second(hydra.lib.pairs.second(results)), ifst := hydra.lib.lists.head(iterms), isnd := hydra.lib.lists.head(hydra.lib.lists.tail(iterms)), ty_fst := hydra.lib.lists.head(itypes), ty_snd := hydra.lib.lists.head(hydra.lib.lists.tail(itypes)), pair_term := cast(hydra.core.Term, hydra.core.TermPair((ifst, isnd))), term_with_types := cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(pair_term, ty_fst))), ty_snd))), yield_(term_with_types, cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(ty_fst, ty_snd))), isubst))[9]), infer_many(cx, ((hydra.lib.pairs.first(p), "pair first element"), (hydra.lib.pairs.second(p), "pair second element"))))

def infer_type_of_record(cx: hydra.typing.InferenceContext, record: hydra.core.Record) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def tname() -> hydra.core.Type:
        return record.type_name
    def fields() -> frozenlist[hydra.core.Field]:
        return record.fields
    def fnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), fields())
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname()), (lambda schema_type: hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat2("field ", f.name.value))), fields())), (lambda results: (svars := schema_type.variables, stype := schema_type.type, iterms := hydra.lib.pairs.first(results), itypes := hydra.lib.pairs.first(hydra.lib.pairs.second(results)), isubst := hydra.lib.pairs.second(hydra.lib.pairs.second(results)), ityp := cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(tname(), hydra.lib.lists.zip_with((lambda n, t: hydra.core.FieldType(n, t)), fnames(), itypes)))), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname(), hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames(), iterms))))), hydra.schemas.nominal_application(tname(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of record"),)))[6]))))

def infer_type_of_set(cx: hydra.typing.InferenceContext, s: frozenset[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeSet(x))), (lambda terms: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(terms)))), "set element", hydra.lib.sets.to_list(s))

def infer_type_of_term(cx: hydra.typing.InferenceContext, term: hydra.core.Term, desc: str) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def match_term() -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
        match term:
            case hydra.core.TermAnnotated(value=a):
                return infer_type_of_annotated_term(cx, a)
            
            case hydra.core.TermApplication(value=a2):
                return infer_type_of_application(cx, a2)
            
            case hydra.core.TermEither(value=e):
                return infer_type_of_either(cx, e)
            
            case hydra.core.TermFunction(value=f):
                return infer_type_of_function(cx, f)
            
            case hydra.core.TermLet(value=l):
                return infer_type_of_let(cx, l)
            
            case hydra.core.TermList(value=els):
                return infer_type_of_list(cx, els)
            
            case hydra.core.TermLiteral(value=l2):
                return infer_type_of_literal(cx, l2)
            
            case hydra.core.TermMap(value=m):
                return infer_type_of_map(cx, m)
            
            case hydra.core.TermMaybe(value=m2):
                return infer_type_of_optional(cx, m2)
            
            case hydra.core.TermPair(value=p):
                return infer_type_of_pair(cx, p)
            
            case hydra.core.TermRecord(value=r):
                return infer_type_of_record(cx, r)
            
            case hydra.core.TermSet(value=s):
                return infer_type_of_set(cx, s)
            
            case hydra.core.TermTypeApplication(value=tt):
                return infer_type_of_type_application(cx, tt)
            
            case hydra.core.TermTypeLambda(value=ta):
                return infer_type_of_type_lambda(cx, ta)
            
            case hydra.core.TermUnion(value=i):
                return infer_type_of_injection(cx, i)
            
            case hydra.core.TermUnit():
                return hydra.lib.flows.pure(infer_type_of_unit())
            
            case hydra.core.TermVariable(value=name):
                return infer_type_of_variable(cx, name)
            
            case hydra.core.TermWrap(value=w):
                return infer_type_of_wrapped_term(cx, w)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.monads.with_trace(desc, match_term())

def infer_type_of_type_application(cx: hydra.typing.InferenceContext, tt: hydra.core.TypeApplicationTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_term(cx, tt.body, "type application term")

def infer_type_of_type_lambda(cx: hydra.typing.InferenceContext, ta: hydra.core.TypeLambda) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_term(cx, ta.body, "type abstraction")

def infer_type_of_wrapped_term(cx: hydra.typing.InferenceContext, wt: hydra.core.WrappedTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def tname() -> hydra.core.Type:
        return wt.type_name
    def term() -> hydra.core.Type:
        return wt.body
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname()), (lambda schema_type: hydra.lib.flows.bind(infer_type_of_term(cx, term(), "wrapped term"), (lambda result: (svars := schema_type.variables, stype := schema_type.type, iterm := result.term, itype := result.type, isubst := result.subst, ityp := cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(tname(), itype))), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname(), iterm)))), hydra.schemas.nominal_application(tname(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper"),)))[6]))))

def infer_types_of_temporary_bindings(cx: hydra.typing.InferenceContext, bins: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[T0, tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], tuple[hydra.typing.TypeSubst, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]]]:
    def dflt() -> hydra.compute.Flow[T0, tuple[frozenlist[hydra.core.Term], tuple[frozenlist[hydra.core.Type], tuple[hydra.typing.TypeSubst, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]]]:
        def binding() -> hydra.core.Type:
            return hydra.lib.lists.head(bins)
        def k() -> hydra.core.Type:
            return binding().name
        def v() -> hydra.core.Type:
            return binding().term
        def tl() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.tail(bins)
        return hydra.lib.flows.bind(infer_type_of_term(cx, v(), hydra.lib.strings.cat(("temporary let binding '", k().value, "'"))), (lambda result1: (j := result1.term, u_prime := result1.type, u := result1.subst, c1_inferred := result1.class_constraints, hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(hydra.lib.maps.empty()), (lambda ts: hydra.lib.flows.bind(hydra.schemas.instantiate_type_scheme(ts), (lambda instantiated_ts: (fresh_constraints := hydra.lib.maybes.from_maybe(hydra.lib.maps.empty(), instantiated_ts.constraints), hydra.lib.flows.bind(hydra.unification.unify_types(cx.schema_types, instantiated_ts.type, u_prime, "original binding type"), (lambda unify_subst: hydra.lib.flows.pure(hydra.substitution.subst_in_class_constraints(unify_subst, fresh_constraints)))))[1]))), binding().type), (lambda original_binding_constraints: (c1 := merge_class_constraints(c1_inferred, original_binding_constraints), hydra.lib.flows.bind(infer_types_of_temporary_bindings(hydra.substitution.subst_in_context(u, cx), tl()), (lambda result2: (h := hydra.lib.pairs.first(result2), r_prime := hydra.lib.pairs.first(hydra.lib.pairs.second(result2)), rest_pair := hydra.lib.pairs.second(hydra.lib.pairs.second(result2)), r := hydra.lib.pairs.first(rest_pair), c2 := hydra.lib.pairs.second(rest_pair), c1_subst := hydra.substitution.subst_in_class_constraints(r, c1), merged_constraints := merge_class_constraints(c1_subst, c2), hydra.lib.flows.pure((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(r, j), h), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(r, u_prime), r_prime), (hydra.substitution.compose_type_subst(u, r), merged_constraints)))))[7])))[1])))[4]))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bins), (lambda : hydra.lib.flows.pure(((), ((), (hydra.substitution.id_type_subst(), hydra.lib.maps.empty()))))), (lambda : dflt()))

def for_inferred_term(cx: hydra.typing.InferenceContext, term: hydra.core.Term, desc: str, f: Callable[[hydra.typing.InferenceResult], T0]) -> hydra.compute.Flow[T1, T0]:
    return hydra.lib.flows.map(f, infer_type_of_term(cx, term, desc))

def infer_graph_types(g0: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.graph.Graph]:
    def from_let_term(l: hydra.core.Let) -> hydra.core.Type:
        def bindings() -> frozenlist[hydra.core.Binding]:
            return l.bindings
        def body() -> hydra.core.Type:
            return l.body
        def from_binding(b: hydra.core.Binding) -> tuple[hydra.core.Name, hydra.core.Binding]:
            return (b.name, b)
        return hydra.graph.Graph(hydra.lib.maps.from_list(hydra.lib.lists.map(from_binding, bindings())), hydra.lib.maps.empty(), hydra.lib.maps.empty(), body(), g0.primitives, g0.schema)
    def to_let_term(g: hydra.graph.Graph) -> hydra.core.Type:
        def to_binding(el: hydra.core.Binding) -> hydra.core.Type:
            return hydra.core.Binding(el.name, el.term, el.type)
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(to_binding, hydra.lib.maps.elems(g.elements)), g.body)))
    def for_final(finalized: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.graph.Graph]:
        match finalized:
            case hydra.core.TermLet(value=l):
                return hydra.lib.flows.pure(from_let_term(l))
            
            case hydra.core.TermVariable():
                return hydra.lib.flows.fail("Expected inferred graph as let term")
            
            case _:
                raise TypeError("Unsupported Term")
    return hydra.monads.with_trace("graph inference", hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g0), (lambda cx: hydra.lib.flows.bind(infer_type_of_term(cx, to_let_term(g0), "graph term"), (lambda result: (term := result.term, ts := result.type, hydra.lib.flows.bind(finalize_inferred_term(cx, term), (lambda finalized: for_final(finalized))))[2])))))

def infer_in_graph_context(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.typing.InferenceResult]:
    r"""Infer the type of a term in graph context."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda cx: infer_type_of_term(cx, term, "single term")))))

def infer_type_of(cx: hydra.typing.InferenceContext, term: hydra.core.Term) -> hydra.compute.Flow[T0, tuple[hydra.core.Term, hydra.core.TypeScheme]]:
    def let_term() -> hydra.core.Type:
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((hydra.core.Binding(hydra.core.Name("ignoredVariableName"), term, Nothing()),), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("ignoredBody")))))))
    def for_bindings(bindings: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[T1, tuple[hydra.core.Term, hydra.core.TypeScheme]]:
        def binding() -> hydra.core.Type:
            return hydra.lib.lists.head(bindings)
        def term1() -> hydra.core.Type:
            return binding().term
        def mts() -> Maybe[hydra.core.TypeScheme]:
            return binding().type
        return hydra.lib.maybes.maybe(hydra.lib.flows.fail("Expected a type scheme"), (lambda ts: hydra.lib.flows.pure((term1(), ts))), mts())
    def unify_and_subst(result: hydra.typing.InferenceResult) -> hydra.compute.Flow[T1, tuple[hydra.core.Term, hydra.core.TypeScheme]]:
        def subst() -> hydra.core.Type:
            return result.subst
        return hydra.lib.flows.bind(finalize_inferred_term(cx, result.term), (lambda finalized: hydra.lib.flows.bind(hydra.lexical.with_empty_graph(hydra.extract.core.let(finalized)), (lambda let_result: (bindings := let_result.bindings, hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(bindings)), (lambda : for_bindings(bindings)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("Expected a single binding with a type scheme, but got: ", hydra.lib.literals.show_int32(hydra.lib.lists.length(bindings)), " bindings"))))))[1]))))
    return hydra.lib.flows.bind(infer_type_of_term(cx, let_term(), "infer type of term"), (lambda result: unify_and_subst(result)))

def initial_type_context(g: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.TypeContext]:
    def to_pair(pair: tuple[hydra.core.Name, hydra.core.Binding]) -> hydra.compute.Flow[T1, tuple[hydra.core.Name, hydra.core.Type]]:
        def name() -> hydra.core.Type:
            return hydra.lib.pairs.first(pair)
        def el() -> hydra.core.Type:
            return hydra.lib.pairs.second(pair)
        return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("untyped element: ", name().value)), (lambda ts: hydra.lib.flows.pure((name(), hydra.schemas.type_scheme_to_f_type(ts)))), el().type)
    return hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda ix: hydra.lib.flows.bind(hydra.lib.flows.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.flows.map_list((lambda x1: to_pair(x1)), hydra.lib.maps.to_list(g.elements))), (lambda types: hydra.lib.flows.pure(hydra.typing.TypeContext(types, hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), ix))))))

def show_inference_result(result: hydra.typing.InferenceResult) -> str:
    r"""Show an inference result for debugging."""
    
    def term() -> hydra.core.Type:
        return result.term
    def typ() -> hydra.core.Type:
        return result.type
    def subst() -> hydra.core.Type:
        return result.subst
    return hydra.lib.strings.cat(("{term=", hydra.show.core.term(term()), ", type=", hydra.show.core.type(typ()), ", subst=", hydra.show.typing.type_subst(subst()), "}"))

def yield_debug(cx: T0, debug_id: T1, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.compute.Flow[T2, hydra.typing.InferenceResult]:
    def rterm() -> hydra.core.Type:
        return hydra.substitution.subst_types_in_term(subst, term)
    def rtyp() -> hydra.core.Type:
        return hydra.substitution.subst_in_type(subst, typ)
    return hydra.lib.flows.bind(hydra.annotations.debug_if(debug_id, hydra.lib.strings.cat(("\n\tterm: ", hydra.show.core.term(term), "\n\ttyp: ", hydra.show.core.type(typ), "\n\tsubst: ", hydra.show.typing.type_subst(subst), "\n\trterm: ", hydra.show.core.term(rterm()), "\n\trtyp: ", hydra.show.core.type(rtyp())))), (lambda result: hydra.lib.flows.pure(hydra.typing.InferenceResult(rterm(), rtyp(), subst, hydra.lib.maps.empty()))))

def yield_with_constraints(term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst, constraints: FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]) -> hydra.core.Type:
    r"""Create an inference result with class constraints."""
    
    return hydra.typing.InferenceResult(hydra.substitution.subst_types_in_term(subst, term), hydra.substitution.subst_in_type(subst, typ), subst, constraints)
