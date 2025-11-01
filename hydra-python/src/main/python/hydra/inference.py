# Note: this is an automatically generated file. Do not edit.

r"""Type inference following Algorithm W, extended for nominal terms and types."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
import hydra.annotations
import hydra.checking
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lexical
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.optionals
import hydra.lib.sets
import hydra.lib.strings
import hydra.mantle
import hydra.monads
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.show.typing
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.unification
import hydra.variants

def bind_constraints[T0, T1](cx: hydra.typing.InferenceContext, f: Callable[[hydra.typing.TypeSubst], hydra.compute.Flow[T0, T1]], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T0, T1]:
    return hydra.lib.flows.bind(hydra.unification.unify_type_constraints(cx.schema_types, constraints), (lambda s: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s), (lambda _: f(s)))))

def bind_unbound_type_variables(cx: hydra.typing.InferenceContext, term0: hydra.core.Term) -> hydra.core.Term:
    r"""Place unbound type variables appearing anywhere under a typed let binding in the type scheme of that binding. These variables may appear in the binding type scheme itself or in that of a subterm, in domain types attached to functions, and in type abstraction and type application terms. This process attempts to capture type variables which have escaped unification, e.g. due to unused code. However, unbound type variables not appearing beneath any typed let binding remain unbound."""
    
    svars = hydra.lib.sets.from_list(hydra.lib.maps.keys(cx.schema_types))
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        match term:
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    bname = b.name
                    bterm = b.term
                    return hydra.lib.optionals.maybe(hydra.core.Binding(bname, bind_unbound_type_variables(cx, bterm), cast(Maybe[hydra.core.TypeScheme], Nothing())), (lambda ts: (bvars := hydra.lib.sets.from_list(ts.variables), unbound_in_type := hydra.rewriting.free_variables_in_type(ts.type), unbound_in_term := hydra.rewriting.free_type_variables_in_term(bterm), unbound := hydra.lib.sets.to_list(hydra.lib.sets.difference(hydra.lib.sets.union(unbound_in_type, unbound_in_term), hydra.lib.sets.union(svars, bvars))), ts2 := hydra.core.TypeScheme(hydra.lib.lists.concat2(ts.variables, unbound), ts.type), bterm2 := hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, t)))), bterm, unbound), hydra.core.Binding(bname, bterm2, cast(Maybe[hydra.core.TypeScheme], Just(ts2))))[6]), b.type)
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(for_binding, l.bindings), bind_unbound_type_variables(cx, l.body))))
            
            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term(rewrite, term0)

def build_type_application_term(tvars: frozenlist[hydra.core.Name], body: hydra.core.Term) -> hydra.core.Term:
    r"""Fold a list of type variables over a term to build a type application term."""
    
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, cast(hydra.core.Type, hydra.core.TypeVariable(v)))))), body, tvars)

# An empty inference context.
empty_inference_context = hydra.typing.InferenceContext(FrozenDict({}), FrozenDict({}), FrozenDict({}), False)

def extend_context(pairs: frozenlist[Tuple[hydra.core.Name, hydra.core.TypeScheme]], cx: hydra.typing.InferenceContext) -> hydra.typing.InferenceContext:
    r"""Add (term variable, type scheme) pairs to the typing environment."""
    
    return hydra.typing.InferenceContext(cx.schema_types, cx.primitive_types, hydra.lib.maps.union(hydra.lib.maps.from_list(pairs), cx.data_types), cx.debug)

def finalize_inferred_term[T0](cx: hydra.typing.InferenceContext, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    term2 = bind_unbound_type_variables(cx, term)
    return hydra.lib.flows.bind(hydra.checking.check_for_unbound_type_variables(cx, term2), (lambda _: hydra.lib.flows.pure(hydra.rewriting.normalize_type_variables_in_term(term2))))

def map_constraints[T0, T1](cx: hydra.typing.InferenceContext, f: Callable[[hydra.typing.TypeSubst], T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, T0]:
    return hydra.lib.flows.bind(hydra.unification.unify_type_constraints(cx.schema_types, constraints), (lambda s: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s), (lambda _: hydra.lib.flows.pure(f(s))))))

def yield_(term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.typing.InferenceResult:
    r"""Create an inference result."""
    
    return hydra.typing.InferenceResult(hydra.substitution.subst_types_in_term(subst, term), hydra.substitution.subst_in_type(subst, typ), subst)

def infer_type_of_projection[T0](cx: hydra.typing.InferenceContext, proj: hydra.core.Projection) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    tname = proj.type_name
    fname = proj.field
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.record_type(tname, stype), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname, sfields), (lambda ftyp: hydra.lib.flows.pure(yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(tname, fname)))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), ftyp))), hydra.substitution.id_type_subst)))))))[2]))

def infer_type_of_tuple_projection[T0, T1](cx: T0, tp: hydra.core.TupleProjection) -> hydra.compute.Flow[T1, hydra.typing.InferenceResult]:
    arity = tp.arity
    idx = tp.index
    return hydra.lib.flows.bind(hydra.schemas.fresh_names(arity), (lambda vars: (types := hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), vars), cod := hydra.lib.lists.at(idx, types), hydra.lib.flows.pure(yield_(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(hydra.core.TupleProjection(arity, idx, cast(Maybe[frozenlist[hydra.core.Type]], Just(types))))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeProduct(types)), cod))), hydra.substitution.id_type_subst)))[2]))

def infer_type_of_unwrap[T0](cx: hydra.typing.InferenceContext, tname: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.wrapped_type(tname, stype), (lambda wtyp: hydra.lib.flows.pure(yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(tname))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), wtyp))), hydra.substitution.id_type_subst)))))[2]))

def free_variables_in_context(cx: hydra.typing.InferenceContext) -> frozenset[hydra.core.Name]:
    r"""Get all free variables in an inference context."""
    
    return hydra.lib.lists.foldl(hydra.lib.sets.union, hydra.lib.sets.empty(), hydra.lib.lists.map(hydra.rewriting.free_variables_in_type_scheme_simple, hydra.lib.maps.elems(cx.data_types)))

def yield_checked[T0](term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    iterm = hydra.substitution.subst_types_in_term(subst, term)
    itype = hydra.substitution.subst_in_type(subst, typ)
    return hydra.lib.flows.pure(hydra.typing.InferenceResult(iterm, itype, subst))

def infer_type_of_primitive[T0](cx: hydra.typing.InferenceContext, name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.optionals.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("No such primitive: ", name.value)), (lambda scheme: hydra.lib.flows.bind(hydra.schemas.instantiate_type_scheme(scheme), (lambda ts: yield_checked(build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))))), ts.type, hydra.substitution.id_type_subst)))), hydra.lib.maps.lookup(name, cx.primitive_types))

def is_unbound(cx: hydra.typing.InferenceContext, v: hydra.core.Name) -> bool:
    r"""Check if a variable is unbound in context."""
    
    return hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_context(cx))), hydra.lib.logic.not_(hydra.lib.maps.member(v, cx.schema_types)))

def generalize(cx: hydra.typing.InferenceContext, typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Generalize a type to a type scheme."""
    
    vars = hydra.lib.lists.nub(hydra.lib.lists.filter((lambda v1: is_unbound(cx, v1)), hydra.rewriting.free_variables_in_type_ordered(typ)))
    return hydra.core.TypeScheme(vars, typ)

def infer_type_of_literal[T0, T1](_: T0, lit: hydra.core.Literal) -> hydra.compute.Flow[T1, hydra.typing.InferenceResult]:
    return hydra.lib.flows.pure(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLiteral(lit)), cast(hydra.core.Type, hydra.core.TypeLiteral(hydra.variants.literal_type(lit))), hydra.substitution.id_type_subst))

# The trivial inference rule for the unit term.
infer_type_of_unit = hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermUnit(None)), cast(hydra.core.Type, hydra.core.TypeUnit(None)), hydra.substitution.id_type_subst)

def infer_type_of_variable[T0](cx: hydra.typing.InferenceContext, name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.optionals.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("Variable not bound to type: ", name.value)), (lambda scheme: hydra.lib.flows.bind(hydra.schemas.instantiate_type_scheme(scheme), (lambda ts: hydra.lib.flows.pure(hydra.typing.InferenceResult(build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name))), ts.type, hydra.substitution.id_type_subst))))), hydra.lib.maps.lookup(name, cx.data_types))

def infer_many[T0](cx: hydra.typing.InferenceContext, pairs: frozenlist[Tuple[hydra.core.Term, str]]) -> hydra.compute.Flow[T0, Tuple[frozenlist[hydra.core.Term], Tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
    def dflt() -> hydra.compute.Flow[T0, Tuple[frozenlist[hydra.core.Term], Tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
        e = hydra.lib.lists.head(pairs)[0]
        desc = hydra.lib.lists.head(pairs)[1]
        tl = hydra.lib.lists.tail(pairs)
        return hydra.lib.flows.bind(infer_type_of_term(cx, e, desc), (lambda result1: (e1 := result1.term, t1 := result1.type, s1 := result1.subst, hydra.lib.flows.bind(infer_many(hydra.substitution.subst_in_context(s1, cx), tl), (lambda result2: (e2 := result2[0], t2 := result2[1][0], s2 := result2[1][1], hydra.lib.flows.pure((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(s2, e1), e2), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(s2, t1), t2), hydra.substitution.compose_type_subst(s1, s2)))))[3])))[3]))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(pairs), hydra.lib.flows.pure(((), ((), hydra.substitution.id_type_subst))), dflt())

def infer_type_of_annotated_term[T0](cx: hydra.typing.InferenceContext, at: hydra.core.AnnotatedTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    term = at.body
    ann = at.annotation
    return hydra.lib.flows.bind(infer_type_of_term(cx, term, "annotated term"), (lambda result: (iterm := result.term, itype := result.type, isubst := result.subst, hydra.lib.flows.pure(hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(iterm, ann))), itype, isubst)))[3]))

def infer_type_of_application[T0](cx: hydra.typing.InferenceContext, app: hydra.core.Application) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    e0 = app.function
    e1 = app.argument
    return hydra.lib.flows.bind(infer_type_of_term(cx, e0, "lhs"), (lambda lhs_result: (a := lhs_result.term, t0 := lhs_result.type, s0 := lhs_result.subst, hydra.lib.flows.bind(infer_type_of_term(hydra.substitution.subst_in_context(s0, cx), e1, "rhs"), (lambda rhs_result: (b := rhs_result.term, t1 := rhs_result.type, s1 := rhs_result.subst, hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda v: hydra.lib.flows.bind(hydra.unification.unify_types(cx.schema_types, hydra.substitution.subst_in_type(s1, t0), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(t1, cast(hydra.core.Type, hydra.core.TypeVariable(v))))), "application lhs"), (lambda s2: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx, s2), (lambda _: (r_expr := cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(s1, s2), a), hydra.substitution.subst_types_in_term(s2, b)))), r_type := hydra.substitution.subst_in_type(s2, cast(hydra.core.Type, hydra.core.TypeVariable(v))), r_subst := hydra.substitution.compose_type_subst_list((s0, s1, s2)), hydra.lib.flows.pure(hydra.typing.InferenceResult(r_expr, r_type, r_subst)))[3])))))))[3])))[3]))

def infer_type_of_case_statement[T0](cx: hydra.typing.InferenceContext, case_stmt: hydra.core.CaseStatement) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    tname = case_stmt.type_name
    dflt = case_stmt.default
    cases = case_stmt.cases
    fnames = hydra.lib.lists.map((lambda v1: v1.name), cases)
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.union_type(tname, stype), (lambda sfields: hydra.lib.flows.bind(hydra.lib.flows.map_optional((lambda t: infer_type_of_term(cx, t, hydra.lib.strings.cat(("case ", tname.value, ".<default>")))), dflt), (lambda dflt_result: hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat(("case ", tname.value, ".", f.name.value)))), cases)), (lambda case_results: (iterms := case_results[0], itypes := case_results[1][0], isubst := case_results[1][1], hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda codv: (cod := cast(hydra.core.Type, hydra.core.TypeVariable(codv)), case_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), sfields)), dflt_constraints := hydra.monads.optional_to_list(hydra.lib.optionals.map((lambda r: hydra.typing.TypeConstraint(cod, r.type, "match default")), dflt_result)), case_constraints := hydra.lib.optionals.cat(hydra.lib.lists.zip_with((lambda fname, itype: hydra.lib.optionals.map((lambda ftype: hydra.typing.TypeConstraint(itype, cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(ftype, cod))), "case type")), hydra.lib.maps.lookup(fname, case_map))), fnames, itypes)), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(tname, hydra.lib.optionals.map((lambda v1: v1.term), dflt_result), hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames, iterms))))))))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), cod))), hydra.substitution.compose_type_subst_list(hydra.lib.lists.concat((hydra.monads.optional_to_list(hydra.lib.optionals.map((lambda v1: v1.subst), dflt_result)), (isubst, subst)))))), hydra.lib.lists.concat((dflt_constraints, case_constraints))))[4])))[3])))))))[2]))

def infer_type_of_collection[T0](cx: hydra.typing.InferenceContext, typ_cons: Callable[[hydra.core.Type], hydra.core.Type], trm_cons: Callable[[frozenlist[hydra.core.Term]], hydra.core.Term], desc: str, els: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda var: hydra.lib.logic.if_else(hydra.lib.lists.null(els), hydra.lib.flows.pure(yield_(build_type_application_term((var,), trm_cons(())), typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var))), hydra.substitution.id_type_subst)), hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.zip(els, hydra.lib.lists.map((lambda i: hydra.lib.strings.cat(("#", hydra.lib.literals.show_int32(i)))), hydra.lib.math.range_(1, hydra.lib.math.add(hydra.lib.lists.length(els), 1))))), (lambda results: (terms := results[0], types := results[1][0], subst1 := results[1][1], constraints := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(var)), t, desc)), types), map_constraints(cx, (lambda subst2: (iterm := trm_cons(terms), itype := typ_cons(cast(hydra.core.Type, hydra.core.TypeVariable(var))), isubst := hydra.substitution.compose_type_subst(subst1, subst2), yield_(iterm, itype, isubst))[3]), constraints))[4])))))

def infer_type_of_elimination[T0](cx: hydra.typing.InferenceContext, elm: hydra.core.Elimination) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    match elm:
        case hydra.core.EliminationProduct(value=tp):
            return infer_type_of_tuple_projection(cx, tp)
        
        case hydra.core.EliminationRecord(value=p):
            return infer_type_of_projection(cx, p)
        
        case hydra.core.EliminationUnion(value=c):
            return infer_type_of_case_statement(cx, c)
        
        case hydra.core.EliminationWrap(value=tname):
            return infer_type_of_unwrap(cx, tname)

def infer_type_of_function[T0](cx: hydra.typing.InferenceContext, f: hydra.core.Function) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    match f:
        case hydra.core.FunctionElimination(value=elm):
            return infer_type_of_elimination(cx, elm)
        
        case hydra.core.FunctionLambda(value=l):
            return infer_type_of_lambda(cx, l)
        
        case hydra.core.FunctionPrimitive(value=name):
            return infer_type_of_primitive(cx, name)

def infer_type_of_injection[T0](cx: hydra.typing.InferenceContext, injection: hydra.core.Injection) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    tname = injection.type_name
    field = injection.field
    fname = field.name
    term = field.term
    return hydra.lib.flows.bind(infer_type_of_term(cx, term, "injected term"), (lambda result: hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: (svars := schema_type.variables, stype := schema_type.type, iterm := result.term, ityp := result.type, isubst := result.subst, hydra.lib.flows.bind(hydra.extract.core.union_type(tname, stype), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname, sfields), (lambda ftyp: map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(tname, hydra.core.Field(fname, iterm))))), hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field"),)))))))[5]))))

def infer_type_of_lambda[T0](cx: hydra.typing.InferenceContext, lambda_: hydra.core.Lambda) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    var = lambda_.parameter
    body = lambda_.body
    return hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda vdom: (dom := cast(hydra.core.Type, hydra.core.TypeVariable(vdom)), cx2 := extend_context(((var, hydra.core.TypeScheme((), dom)),), cx), hydra.lib.flows.bind(infer_type_of_term(cx2, body, "lambda body"), (lambda result: (iterm := result.term, icod := result.type, isubst := result.subst, rdom := hydra.substitution.subst_in_type(isubst, dom), rterm := cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var, cast(Maybe[hydra.core.Type], Just(rdom)), iterm))))), rtype := cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(rdom, icod))), vars := hydra.lib.sets.unions((hydra.rewriting.free_variables_in_type(rdom), hydra.rewriting.free_variables_in_type(icod), free_variables_in_context(hydra.substitution.subst_in_context(isubst, cx2)))), cx3 := hydra.substitution.subst_in_context(isubst, cx), hydra.lib.flows.pure(hydra.typing.InferenceResult(rterm, rtype, isubst)))[8])))[2]))

def infer_type_of_let[T0](cx: hydra.typing.InferenceContext, let0: hydra.core.Let) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    bindings0 = let0.bindings
    body0 = let0.body
    names = hydra.lib.lists.map((lambda v1: v1.name), bindings0)
    name_set = hydra.lib.sets.from_list(names)
    def to_pair(binding: hydra.core.Binding) -> Tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        name = binding.name
        term = binding.term
        return (name, hydra.lib.lists.filter((lambda n: hydra.lib.sets.member(n, name_set)), hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_term(term))))
    adj_list = hydra.lib.lists.map(to_pair, bindings0)
    groups = hydra.sorting.topological_sort_components(adj_list)
    binding_map = hydra.lib.maps.from_list(hydra.lib.lists.zip(names, bindings0))
    def create_let(e: hydra.core.Term, group: frozenlist[hydra.core.Name]) -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.optionals.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map)), group)), e)))
    rewritten_let = hydra.lib.lists.foldl(create_let, body0, hydra.lib.lists.reverse(groups))
    def restore_let(iterm: hydra.core.Term) -> hydra.core.Term:
        def helper(level: int, bins: frozenlist[hydra.core.Binding], term: hydra.core.Term) -> Tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
            def nonzero(term2: hydra.core.Term) -> Tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                match term2:
                    case hydra.core.TermLet(value=l):
                        bs = l.bindings
                        e = l.body
                        return helper(hydra.lib.math.sub(level, 1), hydra.lib.lists.concat((bs, bins)), e)
                    
                    case _:
                        raise TypeError("Unsupported Term")
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(level, 0), (bins, term), nonzero(term))
        result = helper(hydra.lib.lists.length(groups), (), iterm)
        binding_list = result[0]
        e = result[1]
        binding_map2 = hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b)), binding_list))
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.optionals.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, binding_map2)), names)), e)))
    def rewrite_result(result: hydra.typing.InferenceResult) -> hydra.typing.InferenceResult:
        iterm = result.term
        itype = result.type
        isubst = result.subst
        return hydra.typing.InferenceResult(restore_let(iterm), itype, isubst)
    def res() -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
        match rewritten_let:
            case hydra.core.TermLet(value=l):
                return infer_type_of_let_normalized(cx, l)
            
            case _:
                return infer_type_of_term(cx, rewritten_let, "empty let term")
    return hydra.lib.flows.map(rewrite_result, res())

def infer_type_of_let_normalized[T0](cx0: hydra.typing.InferenceContext, let_term: hydra.core.Let) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    bins0 = let_term.bindings
    body0 = let_term.body
    bnames = hydra.lib.lists.map((lambda v1: v1.name), bins0)
    return hydra.lib.flows.bind(hydra.schemas.fresh_names(hydra.lib.lists.length(bins0)), (lambda bvars: (tbins0 := hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), bvars), cx1 := extend_context(hydra.lib.lists.zip(bnames, hydra.lib.lists.map((lambda t: hydra.core.TypeScheme((), t)), tbins0)), cx0), hydra.lib.flows.bind(infer_types_of_temporary_bindings(cx1, bins0), (lambda inferred_result: (bterms1 := inferred_result[0], tbins1 := inferred_result[1][0], s1 := inferred_result[1][1], hydra.lib.flows.bind(hydra.unification.unify_type_lists(cx0.schema_types, hydra.lib.lists.map((lambda v1: hydra.substitution.subst_in_type(s1, v1)), tbins0), tbins1, "temporary type bindings"), (lambda s2: hydra.lib.flows.bind(hydra.checking.check_type_subst(cx0, s2), (lambda _: (g2 := hydra.substitution.subst_in_context(hydra.substitution.compose_type_subst(s1, s2), cx0), bterms1_subst := hydra.lib.lists.map((lambda v1: hydra.substitution.subst_types_in_term(s2, v1)), bterms1), tsbins1 := hydra.lib.lists.zip(bnames, hydra.lib.lists.map((lambda t: generalize(g2, hydra.substitution.subst_in_type(s2, t))), tbins1)), hydra.lib.flows.bind(infer_type_of_term(extend_context(tsbins1, g2), body0, "let body"), (lambda body_result: (body1 := body_result.term, tbody := body_result.type, sbody := body_result.subst, st1 := hydra.typing.TermSubst(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda pair: (name := pair[0], ts := pair[1], (name, build_type_application_term(ts.variables, cast(hydra.core.Term, hydra.core.TermVariable(name)))))[2]), tsbins1))), create_binding := (lambda binding_pair: (name_ts_pair := binding_pair[0], term := binding_pair[1], name := name_ts_pair[0], ts := name_ts_pair[1], type_lambda_term := hydra.lib.lists.foldl((lambda b, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, b)))), hydra.substitution.substitute_in_term(st1, term), hydra.lib.lists.reverse(ts.variables)), hydra.core.Binding(name, hydra.substitution.subst_types_in_term(hydra.substitution.compose_type_subst(sbody, s2), type_lambda_term), cast(Maybe[hydra.core.TypeScheme], Just(hydra.substitution.subst_in_type_scheme(sbody, ts)))))[5]), bins1 := hydra.lib.lists.map(create_binding, hydra.lib.lists.zip(tsbins1, bterms1_subst)), ret := hydra.typing.InferenceResult(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins1, body1))), tbody, hydra.substitution.compose_type_subst_list((s1, s2, sbody))), hydra.lib.flows.pure(ret))[7])))[3])))))[3])))[2]))

def infer_type_of_list[T0](cx: hydra.typing.InferenceContext, v1: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), "list element", v1)

def infer_type_of_map[T0](cx: hydra.typing.InferenceContext, m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda kvar: hydra.lib.flows.bind(hydra.schemas.fresh_name, (lambda vvar: hydra.lib.logic.if_else(hydra.lib.maps.null(m), hydra.lib.flows.pure(yield_(build_type_application_term((kvar, vvar), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.empty()))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), cast(hydra.core.Type, hydra.core.TypeVariable(vvar))))), hydra.substitution.id_type_subst)), hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda k: (k, "map key")), hydra.lib.maps.keys(m))), (lambda kresults: (kterms := kresults[0], ktypes := kresults[1][0], ksubst := kresults[1][1], hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda v: (v, "map value")), hydra.lib.maps.elems(m))), (lambda vresults: (vterms := vresults[0], vtypes := vresults[1][0], vsubst := vresults[1][1], kcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), t, "map key")), ktypes), vcons := hydra.lib.lists.map((lambda t: hydra.typing.TypeConstraint(cast(hydra.core.Type, hydra.core.TypeVariable(vvar)), t, "map value")), vtypes), map_constraints(cx, (lambda subst: yield_(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.zip(kterms, vterms)))), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeVariable(kvar)), cast(hydra.core.Type, hydra.core.TypeVariable(vvar))))), hydra.substitution.compose_type_subst_list((ksubst, vsubst, subst)))), hydra.lib.lists.concat((kcons, vcons))))[5])))[3])))))))

def infer_type_of_optional[T0](cx: hydra.typing.InferenceContext, m: Maybe[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def trm_cons(terms: frozenlist[hydra.core.Term]) -> hydra.core.Term:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(terms), cast(hydra.core.Term, hydra.core.TermOptional(cast(Maybe[hydra.core.Term], Nothing()))), cast(hydra.core.Term, hydra.core.TermOptional(cast(Maybe[hydra.core.Term], Just(hydra.lib.lists.head(terms))))))
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeOptional(x))), trm_cons, "optional element", hydra.lib.optionals.maybe((), hydra.lib.lists.singleton, m))

def infer_type_of_product[T0](cx: hydra.typing.InferenceContext, els: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return hydra.lib.flows.map((lambda results: (iterms := results[0], itypes := results[1][0], isubst := results[1][1], yield_(cast(hydra.core.Term, hydra.core.TermProduct(iterms)), cast(hydra.core.Type, hydra.core.TypeProduct(itypes)), isubst))[3]), infer_many(cx, hydra.lib.lists.map((lambda e: (e, "tuple element")), els)))

def infer_type_of_record[T0](cx: hydra.typing.InferenceContext, record: hydra.core.Record) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    tname = record.type_name
    fields = record.fields
    fnames = hydra.lib.lists.map((lambda v1: v1.name), fields)
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: hydra.lib.flows.bind(infer_many(cx, hydra.lib.lists.map((lambda f: (f.term, hydra.lib.strings.cat2("field ", f.name.value))), fields)), (lambda results: (svars := schema_type.variables, stype := schema_type.type, iterms := results[0], itypes := results[1][0], isubst := results[1][1], ityp := cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(tname, hydra.lib.lists.zip_with((lambda n, t: hydra.core.FieldType(n, t)), fnames, itypes)))), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, hydra.lib.lists.zip_with((lambda n, t: hydra.core.Field(n, t)), fnames, iterms))))), hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of record"),)))[6]))))

def infer_type_of_set[T0](cx: hydra.typing.InferenceContext, s: frozenset[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_collection(cx, (lambda x: cast(hydra.core.Type, hydra.core.TypeSet(x))), (lambda terms: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(terms)))), "set element", hydra.lib.sets.to_list(s))

def infer_type_of_sum[T0](cx: hydra.typing.InferenceContext, sum: hydra.core.Sum) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    i = sum.index
    s = sum.size
    term = sum.term
    def to_type(v1: hydra.mantle.Either[hydra.core.Type, hydra.core.Name]) -> hydra.core.Type:
        match v1:
            case hydra.mantle.EitherLeft(value=t):
                return t
            
            case hydra.mantle.EitherRight(value=v):
                return cast(hydra.core.Type, hydra.core.TypeVariable(v))
    return hydra.lib.flows.bind(infer_type_of_term(cx, term, "sum term"), (lambda result: (iterm := result.term, ityp := result.type, isubst := result.subst, var_or_term := (lambda t, j: hydra.lib.logic.if_else(hydra.lib.equality.equal(i, j), hydra.lib.flows.pure(cast(hydra.mantle.Either, hydra.mantle.EitherLeft(t))), hydra.lib.flows.map((lambda x: cast(hydra.mantle.Either, hydra.mantle.EitherRight(x))), hydra.schemas.fresh_name))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: var_or_term(ityp, v1)), hydra.lib.math.range_(0, hydra.lib.math.sub(s, 1))), (lambda vars: hydra.lib.flows.pure(yield_(cast(hydra.core.Term, hydra.core.TermSum(hydra.core.Sum(i, s, iterm))), cast(hydra.core.Type, hydra.core.TypeSum(hydra.lib.lists.map(to_type, vars))), isubst)))))[4]))

def infer_type_of_term[T0](cx: hydra.typing.InferenceContext, term: hydra.core.Term, desc: str) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    def match_term() -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
        match term:
            case hydra.core.TermAnnotated(value=a):
                return infer_type_of_annotated_term(cx, a)
            
            case hydra.core.TermApplication(value=a2):
                return infer_type_of_application(cx, a2)
            
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
            
            case hydra.core.TermOptional(value=m2):
                return infer_type_of_optional(cx, m2)
            
            case hydra.core.TermProduct(value=els2):
                return infer_type_of_product(cx, els2)
            
            case hydra.core.TermRecord(value=r):
                return infer_type_of_record(cx, r)
            
            case hydra.core.TermSet(value=s):
                return infer_type_of_set(cx, s)
            
            case hydra.core.TermSum(value=s2):
                return infer_type_of_sum(cx, s2)
            
            case hydra.core.TermTypeLambda(value=ta):
                return infer_type_of_type_lambda(cx, ta)
            
            case hydra.core.TermTypeApplication(value=tt):
                return infer_type_of_type_application(cx, tt)
            
            case hydra.core.TermUnion(value=i):
                return infer_type_of_injection(cx, i)
            
            case hydra.core.TermUnit():
                return hydra.lib.flows.pure(infer_type_of_unit)
            
            case hydra.core.TermVariable(value=name):
                return infer_type_of_variable(cx, name)
            
            case hydra.core.TermWrap(value=w):
                return infer_type_of_wrapped_term(cx, w)
    return hydra.monads.with_trace(desc, match_term())

def infer_type_of_type_application[T0](cx: hydra.typing.InferenceContext, tt: hydra.core.TypeApplicationTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_term(cx, tt.body, "type application term")

def infer_type_of_type_lambda[T0](cx: hydra.typing.InferenceContext, ta: hydra.core.TypeLambda) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    return infer_type_of_term(cx, ta.body, "type abstraction")

def infer_type_of_wrapped_term[T0](cx: hydra.typing.InferenceContext, wt: hydra.core.WrappedTerm) -> hydra.compute.Flow[T0, hydra.typing.InferenceResult]:
    tname = wt.type_name
    term = wt.body
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(cx, tname), (lambda schema_type: hydra.lib.flows.bind(infer_type_of_term(cx, term, "wrapped term"), (lambda result: (svars := schema_type.variables, stype := schema_type.type, iterm := result.term, itype := result.type, isubst := result.subst, ityp := cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(tname, itype))), map_constraints(cx, (lambda subst: yield_(build_type_application_term(svars, cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, iterm)))), hydra.schemas.nominal_application(tname, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), svars)), hydra.substitution.compose_type_subst(isubst, subst))), (hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper"),)))[6]))))

def infer_types_of_temporary_bindings[T0](cx: hydra.typing.InferenceContext, bins: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[T0, Tuple[frozenlist[hydra.core.Term], Tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
    def dflt() -> hydra.compute.Flow[T0, Tuple[frozenlist[hydra.core.Term], Tuple[frozenlist[hydra.core.Type], hydra.typing.TypeSubst]]]:
        binding = hydra.lib.lists.head(bins)
        k = binding.name
        v = binding.term
        tl = hydra.lib.lists.tail(bins)
        return hydra.lib.flows.bind(infer_type_of_term(cx, v, hydra.lib.strings.cat(("temporary let binding '", k.value, "'"))), (lambda result1: (j := result1.term, u_prime := result1.type, u := result1.subst, hydra.lib.flows.bind(infer_types_of_temporary_bindings(hydra.substitution.subst_in_context(u, cx), tl), (lambda result2: (h := result2[0], r_prime := result2[1][0], r := result2[1][1], hydra.lib.flows.pure((hydra.lib.lists.cons(hydra.substitution.subst_types_in_term(r, j), h), (hydra.lib.lists.cons(hydra.substitution.subst_in_type(r, u_prime), r_prime), hydra.substitution.compose_type_subst(u, r)))))[3])))[3]))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(bins), hydra.lib.flows.pure(((), ((), hydra.substitution.id_type_subst))), dflt())

def for_inferred_term[T0, T1](cx: hydra.typing.InferenceContext, term: hydra.core.Term, desc: str, f: Callable[[hydra.typing.InferenceResult], T0]) -> hydra.compute.Flow[T1, T0]:
    return hydra.lib.flows.map(f, infer_type_of_term(cx, term, desc))

fresh_variable_type = hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), hydra.schemas.fresh_name)

def infer_graph_types[T0](g0: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.graph.Graph]:
    def from_let_term(l: hydra.core.Let) -> hydra.graph.Graph:
        bindings = l.bindings
        body = l.body
        def from_binding(b: hydra.core.Binding) -> Tuple[hydra.core.Name, hydra.core.Binding]:
            return (b.name, b)
        return hydra.graph.Graph(hydra.lib.maps.from_list(hydra.lib.lists.map(from_binding, bindings)), hydra.lib.maps.empty(), hydra.lib.maps.empty(), body, g0.primitives, g0.schema)
    def to_let_term(g: hydra.graph.Graph) -> hydra.core.Term:
        def to_binding(el: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(el.name, el.term, cast(Maybe[hydra.core.TypeScheme], Nothing()))
        return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(to_binding, hydra.lib.maps.elems(g.elements)), g.body)))
    def for_final[T1](finalized: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.graph.Graph]:
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
    
    return hydra.lib.flows.bind(hydra.monads.get_state, (lambda g: hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda cx: infer_type_of_term(cx, term, "single term")))))

def infer_type_of[T0](cx: hydra.typing.InferenceContext, term: hydra.core.Term) -> hydra.compute.Flow[T0, Tuple[hydra.core.Term, hydra.core.TypeScheme]]:
    let_term = cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let((hydra.core.Binding(hydra.core.Name("ignoredVariableName"), term, cast(Maybe[hydra.core.TypeScheme], Nothing())),), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("ignoredBody")))))))
    def for_bindings[T1](bindings: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[T1, Tuple[hydra.core.Term, hydra.core.TypeScheme]]:
        binding = hydra.lib.lists.head(bindings)
        term1 = binding.term
        mts = binding.type
        return hydra.lib.optionals.maybe(hydra.lib.flows.fail("Expected a type scheme"), (lambda ts: hydra.lib.flows.pure((term1, ts))), mts)
    def unify_and_subst[T1](result: hydra.typing.InferenceResult) -> hydra.compute.Flow[T1, Tuple[hydra.core.Term, hydra.core.TypeScheme]]:
        subst = result.subst
        return hydra.lib.flows.bind(finalize_inferred_term(cx, result.term), (lambda finalized: hydra.lib.flows.bind(hydra.lexical.with_empty_graph(hydra.extract.core.let_term(finalized)), (lambda let_result: (bindings := let_result.bindings, hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(bindings)), for_bindings(bindings), hydra.lib.flows.fail(hydra.lib.strings.cat(("Expected a single binding with a type scheme, but got: ", hydra.lib.literals.show_int32(hydra.lib.lists.length(bindings)), " bindings")))))[1]))))
    return hydra.lib.flows.bind(infer_type_of_term(cx, let_term, "infer type of term"), (lambda result: unify_and_subst(result)))

def initial_type_context[T0](g: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.TypeContext]:
    def to_pair[T1](pair: Tuple[hydra.core.Name, hydra.core.Binding]) -> hydra.compute.Flow[T1, Tuple[hydra.core.Name, hydra.core.Type]]:
        name = pair[0]
        el = pair[1]
        return hydra.lib.optionals.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("untyped element: ", name.value))), (lambda ts: hydra.lib.flows.pure((name, hydra.schemas.type_scheme_to_f_type(ts)))), el.type)
    return hydra.lib.flows.bind(hydra.schemas.graph_to_inference_context(g), (lambda ix: hydra.lib.flows.bind(hydra.lib.flows.map(hydra.lib.maps.from_list, hydra.lib.flows.map_list(to_pair, hydra.lib.maps.to_list(g.elements))), (lambda types: hydra.lib.flows.pure(hydra.typing.TypeContext(types, hydra.lib.sets.empty(), ix))))))

def show_inference_result(result: hydra.typing.InferenceResult) -> str:
    r"""Show an inference result for debugging."""
    
    term = result.term
    typ = result.type
    subst = result.subst
    return hydra.lib.strings.cat(("{term=", hydra.show.core.term(term), ", type=", hydra.show.core.type(typ), ", subst=", hydra.show.typing.type_subst(subst), "}"))

def yield_debug[T0, T1, T2](cx: T0, debug_id: T1, term: hydra.core.Term, typ: hydra.core.Type, subst: hydra.typing.TypeSubst) -> hydra.compute.Flow[T2, hydra.typing.InferenceResult]:
    rterm = hydra.substitution.subst_types_in_term(subst, term)
    rtyp = hydra.substitution.subst_in_type(subst, typ)
    return hydra.lib.flows.bind(hydra.annotations.debug_if(debug_id, hydra.lib.strings.cat(("\n\tterm: ", hydra.show.core.term(term), "\n\ttyp: ", hydra.show.core.type(typ), "\n\tsubst: ", hydra.show.typing.type_subst(subst), "\n\trterm: ", hydra.show.core.term(rterm), "\n\trtyp: ", hydra.show.core.type(rtyp)))), (lambda result: hydra.lib.flows.pure(hydra.typing.InferenceResult(rterm, rtyp, subst))))
