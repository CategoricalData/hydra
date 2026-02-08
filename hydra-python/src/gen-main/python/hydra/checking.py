# Note: this is an automatically generated file. Do not edit.

r"""Type checking and type reconstruction (type-of) for the results of Hydra unification and inference."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.constants
import hydra.core
import hydra.extract.core
import hydra.formatting
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.monads
import hydra.reflect
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.show.meta
import hydra.substitution
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def all_equal(els: frozenlist[T0]) -> bool:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : True), (lambda : hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.and_(b, hydra.lib.equality.equal(t, hydra.lib.lists.head(els)))), True, hydra.lib.lists.tail(els))))

def check_type_variables(_tx: T0, _typ: T1) -> hydra.compute.Flow[T2, None]:
    return hydra.lib.flows.pure(None)

def apply_type_arguments_to_type(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], t: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def nonnull() -> hydra.compute.Flow[T0, hydra.core.Type]:
        match t:
            case hydra.core.TypeForall(value=ft):
                @lru_cache(1)
                def v() -> hydra.core.Name:
                    return ft.parameter
                @lru_cache(1)
                def tbody() -> hydra.core.Type:
                    return ft.body
                return apply_type_arguments_to_type(tx, hydra.lib.lists.tail(type_args), hydra.substitution.subst_in_type(hydra.typing.TypeSubst(hydra.lib.maps.singleton(v(), hydra.lib.lists.head(type_args))), tbody()))
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat(("not a forall type: ", hydra.show.core.type(t), ". Trying to apply ", hydra.lib.literals.show_int32(hydra.lib.lists.length(type_args)), " type args: ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), type_args), ". Context has vars: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(tx.types))), "}")))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(type_args), (lambda : hydra.lib.flows.bind(check_type_variables(tx, t), (lambda _: hydra.lib.flows.pure(t)))), (lambda : nonnull()))

def check_for_unbound_type_variables(cx: hydra.typing.InferenceContext, term0: hydra.core.Term) -> hydra.compute.Flow[T0, None]:
    @lru_cache(1)
    def svars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(cx.schema_types))
    def check_recursive(vars: frozenset[hydra.core.Name], trace: frozenlist[str], lbinding: Maybe[hydra.core.Binding], term: hydra.core.Term) -> hydra.compute.Flow[T1, None]:
        def recurse(v1: hydra.core.Term) -> hydra.compute.Flow[T1, None]:
            return check_recursive(vars, trace, lbinding, v1)
        @lru_cache(1)
        def dflt() -> hydra.compute.Flow[T1, None]:
            return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: recurse(x1)), hydra.rewriting.subterms(term)), (lambda _: hydra.lib.flows.pure(None)))
        def check(typ: hydra.core.Type) -> hydra.compute.Flow[T2, None]:
            @lru_cache(1)
            def freevars() -> frozenset[hydra.core.Name]:
                return hydra.rewriting.free_variables_in_type(typ)
            @lru_cache(1)
            def badvars() -> frozenset[hydra.core.Name]:
                return hydra.lib.sets.difference(hydra.lib.sets.difference(freevars(), vars), svars())
            return hydra.lib.logic.if_else(hydra.lib.sets.null(badvars()), (lambda : hydra.lib.flows.pure(None)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("unbound type variables: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.sets.to_list(badvars())))), "} in type "), hydra.show.core.type(typ)), " at path: "), hydra.lib.strings.intercalate(" >> ", hydra.lib.lists.reverse(trace))), hydra.lib.maybes.maybe("none", (lambda binding: hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(". bound term = ", hydra.show.core.term(binding.term)), ". bound type = "), hydra.lib.maybes.maybe("none", (lambda x1: hydra.show.core.type_scheme(x1)), binding.type))), lbinding)))))
        def check_optional(m: Maybe[hydra.core.Type]) -> hydra.compute.Flow[T2, None]:
            return hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda x1: check(x1)), m), (lambda _: hydra.lib.flows.pure(None)))
        def check_optional_list(ml: Maybe[frozenlist[hydra.core.Type]]) -> hydra.compute.Flow[T2, None]:
            return hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda l: hydra.lib.flows.map_list((lambda x1: check(x1)), l)), ml), (lambda _: hydra.lib.flows.pure(None)))
        def _hoist_body_1(v1: hydra.core.Function) -> hydra.compute.Flow[T1, None]:
            match v1:
                case hydra.core.FunctionElimination():
                    return dflt()
                
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.flows.bind(check_optional(l.domain), (lambda _: recurse(l.body)))
                
                case _:
                    return dflt()
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)
            
            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> hydra.compute.Flow[T1, None]:
                    @lru_cache(1)
                    def bterm() -> hydra.core.Term:
                        return b.term
                    @lru_cache(1)
                    def new_vars() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe(vars, (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    @lru_cache(1)
                    def new_trace() -> frozenlist[str]:
                        return hydra.lib.lists.cons(b.name.value, trace)
                    return check_recursive(new_vars(), new_trace(), Just(b), bterm())
                return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: for_binding(x1)), l.bindings), (lambda _: recurse(l.body)))
            
            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.flows.bind(check(tt.type), (lambda _: recurse(tt.body)))
            
            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.flows.bind(check(cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), (lambda _: recurse(tl.body)))
            
            case _:
                return dflt()
    return check_recursive(hydra.lib.sets.empty(), ("top level",), Nothing(), term0)

def check_nominal_application(tx: hydra.typing.TypeContext, tname: hydra.core.Name, type_args: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[T0, None]:
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(tx.inference_context, tname), (lambda schema_type: (vars := schema_type.variables, body := schema_type.type, varslen := hydra.lib.lists.length(vars), argslen := hydra.lib.lists.length(type_args), hydra.lib.logic.if_else(hydra.lib.equality.equal(varslen, argslen), (lambda : hydra.lib.flows.pure(None)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("nominal type ", tname.value), " applied to the wrong number of type arguments: "), "(expected "), hydra.lib.literals.show_int32(varslen)), " arguments, got "), hydra.lib.literals.show_int32(argslen)), "): "), hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), type_args))))))[4]))

def normalize_type_free_vars(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Normalize free type variables in a type to canonical names based on order of first occurrence. This allows comparing types that differ only in the naming of free type variables."""
    
    def collect_vars(acc: FrozenDict[hydra.core.Name, hydra.core.Name], t: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.maps.member(v, acc), (lambda : acc), (lambda : hydra.lib.maps.insert(v, hydra.core.Name(hydra.lib.strings.cat2("_tv", hydra.lib.literals.show_int32(hydra.lib.maps.size(acc)))), acc)))
            
            case _:
                return acc
    @lru_cache(1)
    def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: collect_vars(x1, x2)), hydra.lib.maps.empty(), typ)
    return hydra.rewriting.substitute_type_variables(subst(), typ)

def types_all_effectively_equal(tx: hydra.typing.TypeContext, tlist: frozenlist[hydra.core.Type]) -> bool:
    r"""Check whether a list of types are effectively equal, disregarding type aliases and free type variable naming. Also treats free type variables (not in schema) as wildcards, since inference has already verified consistency."""
    
    @lru_cache(1)
    def types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return tx.inference_context.schema_types
    def contains_free_var(t: hydra.core.Type) -> bool:
        @lru_cache(1)
        def all_vars() -> frozenset[hydra.core.Name]:
            return hydra.rewriting.free_variables_in_type_simple(t)
        @lru_cache(1)
        def schema_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.sets.from_list(hydra.lib.maps.keys(types()))
        return hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.difference(all_vars(), schema_names())))
    @lru_cache(1)
    def any_contains_free_var() -> bool:
        return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.logic.or_(acc, contains_free_var(t))), False, tlist)
    return hydra.lib.logic.if_else(any_contains_free_var(), (lambda : True), (lambda : hydra.lib.logic.if_else(all_equal(hydra.lib.lists.map((lambda t: normalize_type_free_vars(t)), tlist)), (lambda : True), (lambda : all_equal(hydra.lib.lists.map((lambda t: normalize_type_free_vars(hydra.rewriting.deannotate_type_recursive(hydra.rewriting.replace_typedefs(types(), t)))), tlist))))))

def check_same_type(tx: hydra.typing.TypeContext, desc: str, types: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.logic.if_else(types_all_effectively_equal(tx, types), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(types))), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("unequal types ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), types), " in ", desc)))))

def contains_in_scope_type_vars(tx: hydra.typing.TypeContext, t: hydra.core.Type) -> bool:
    r"""Check if a type contains any type variable from the current scope."""
    
    @lru_cache(1)
    def vars() -> frozenset[hydra.core.Name]:
        return tx.type_variables
    @lru_cache(1)
    def free_vars() -> frozenset[hydra.core.Name]:
        return hydra.rewriting.free_variables_in_type_simple(t)
    return hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.intersection(vars(), free_vars())))

def types_effectively_equal(tx: hydra.typing.TypeContext, t1: hydra.core.Type, t2: hydra.core.Type) -> bool:
    r"""Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards."""
    
    return hydra.lib.logic.or_(contains_in_scope_type_vars(tx, t1), hydra.lib.logic.or_(contains_in_scope_type_vars(tx, t2), types_all_effectively_equal(tx, (hydra.schemas.fully_strip_and_normalize_type(t1), hydra.schemas.fully_strip_and_normalize_type(t2)))))

def type_of_injection(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], injection: hydra.core.Injection) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return injection.type_name
    @lru_cache(1)
    def field() -> hydra.core.Field:
        return injection.field
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return field().name
    @lru_cache(1)
    def fterm() -> hydra.core.Term:
        return field().term
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(tx.inference_context, tname()), (lambda schema_type: (svars := schema_type.variables, sbody := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.union_type(tname(), sbody), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname(), sfields), (lambda ftyp: hydra.lib.flows.pure(hydra.schemas.nominal_application(tname(), type_args)))))))[2]))

def type_lists_effectively_equal(tx: hydra.typing.TypeContext, tlist1: frozenlist[hydra.core.Type], tlist2: frozenlist[hydra.core.Type]) -> bool:
    r"""Check whether two lists of types are effectively equal, disregarding type aliases."""
    
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(tlist1), hydra.lib.lists.length(tlist2)), (lambda : hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.zip_with((lambda v1, v2: types_effectively_equal(tx, v1, v2)), tlist1, tlist2))), (lambda : False))

def type_of_literal(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], lit: hydra.core.Literal) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def t() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeLiteral(hydra.reflect.literal_type(lit)))
    return apply_type_arguments_to_type(tx, type_args, t())

def type_of_primitive(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such primitive: ", name.value))), (lambda x1: hydra.schemas.instantiate_type_scheme(x1)), hydra.lib.maps.lookup(name, tx.inference_context.primitive_types)), (lambda ts: (t := hydra.schemas.type_scheme_to_f_type(ts), apply_type_arguments_to_type(tx, type_args, t))[1]))

def type_of_projection(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], p: hydra.core.Projection) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return p.type_name
    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return p.field
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(tx.inference_context, tname()), (lambda schema_type: (svars := schema_type.variables, sbody := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.record_type(tname(), sbody), (lambda sfields: hydra.lib.flows.bind(hydra.schemas.find_field_type(fname(), sfields), (lambda ftyp: (subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(svars, type_args))), sftyp := hydra.substitution.subst_in_type(subst, ftyp), hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname(), type_args), sftyp)))))[2])))))[2]))

def type_of_unit(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return apply_type_arguments_to_type(tx, type_args, cast(hydra.core.Type, hydra.core.TypeUnit()))

def type_of_unwrap(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], tname: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.flows.bind(hydra.schemas.require_schema_type(tx.inference_context, tname), (lambda schema_type: (svars := schema_type.variables, sbody := schema_type.type, hydra.lib.flows.bind(hydra.extract.core.wrapped_type(tname, sbody), (lambda wrapped: (subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(svars, type_args))), swrapped := hydra.substitution.subst_in_type(subst, wrapped), hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname, type_args), swrapped)))))[2])))[2]))

def type_of_variable(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def raw_type() -> Maybe[hydra.core.Type]:
        return hydra.lib.maps.lookup(name, tx.types)
    @lru_cache(1)
    def fail_msg() -> hydra.compute.Flow[T1, T2]:
        return hydra.lib.flows.fail(hydra.lib.strings.cat(("unbound variable: ", name.value, ". Variables: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(tx.types))), "}")))
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(fail_msg(), (lambda t: hydra.lib.logic.if_else(hydra.lib.lists.null(type_args), (lambda : hydra.schemas.instantiate_type(t)), (lambda : hydra.lib.flows.pure(t)))), raw_type()), (lambda t: apply_type_arguments_to_type(tx, type_args, t)))

def type_of(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def check() -> hydra.compute.Flow[T0, hydra.core.Type]:
        def _hoist_check_1(v1: hydra.core.Elimination) -> hydra.compute.Flow[T0, hydra.core.Type]:
            match v1:
                case hydra.core.EliminationRecord(value=v12):
                    return type_of_projection(tx, type_args, v12)
                
                case hydra.core.EliminationUnion(value=v122):
                    return type_of_case_statement(tx, type_args, v122)
                
                case hydra.core.EliminationWrap(value=v123):
                    return type_of_unwrap(tx, type_args, v123)
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_check_2(v1: hydra.core.Function) -> hydra.compute.Flow[T0, hydra.core.Type]:
            match v1:
                case hydra.core.FunctionElimination(value=elm):
                    return _hoist_check_1(elm)
                
                case hydra.core.FunctionLambda(value=v12):
                    return type_of_lambda(tx, type_args, v12)
                
                case hydra.core.FunctionPrimitive(value=v122):
                    return type_of_primitive(tx, type_args, v122)
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        match term:
            case hydra.core.TermAnnotated(value=v1):
                return type_of_annotated_term(tx, type_args, v1)
            
            case hydra.core.TermApplication(value=v12):
                return type_of_application(tx, type_args, v12)
            
            case hydra.core.TermEither(value=v13):
                return type_of_either(tx, type_args, v13)
            
            case hydra.core.TermFunction(value=f):
                return _hoist_check_2(f)
            
            case hydra.core.TermLet(value=v14):
                return type_of_let(tx, type_args, v14)
            
            case hydra.core.TermList(value=v15):
                return type_of_list(tx, type_args, v15)
            
            case hydra.core.TermLiteral(value=v16):
                return type_of_literal(tx, type_args, v16)
            
            case hydra.core.TermMap(value=v17):
                return type_of_map(tx, type_args, v17)
            
            case hydra.core.TermMaybe(value=v18):
                return type_of_maybe(tx, type_args, v18)
            
            case hydra.core.TermPair(value=v19):
                return type_of_pair(tx, type_args, v19)
            
            case hydra.core.TermRecord(value=v110):
                return type_of_record(tx, type_args, v110)
            
            case hydra.core.TermSet(value=v111):
                return type_of_set(tx, type_args, v111)
            
            case hydra.core.TermTypeApplication(value=v112):
                return type_of_type_application(tx, type_args, v112)
            
            case hydra.core.TermTypeLambda(value=v113):
                return type_of_type_lambda(tx, type_args, v113)
            
            case hydra.core.TermUnion(value=v114):
                return type_of_injection(tx, type_args, v114)
            
            case hydra.core.TermUnit():
                return type_of_unit(tx, type_args)
            
            case hydra.core.TermVariable(value=v115):
                return type_of_variable(tx, type_args, v115)
            
            case hydra.core.TermWrap(value=v116):
                return type_of_wrapped_term(tx, type_args, v116)
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat(("unsupported term variant in typeOf: ", hydra.show.meta.term_variant(hydra.reflect.term_variant(term)))))
    return hydra.monads.with_trace("typeOf", check())

def type_of_annotated_term(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], at: hydra.core.AnnotatedTerm) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return type_of(tx, type_args, at.body)

def type_of_application(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], app: hydra.core.Application) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def fun() -> hydra.core.Term:
        return app.function
    @lru_cache(1)
    def arg() -> hydra.core.Term:
        return app.argument
    def try_type(tfun: hydra.core.Type, targ: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.core.Type]:
        match tfun:
            case hydra.core.TypeForall(value=ft):
                return try_type(ft.body, targ)
            
            case hydra.core.TypeFunction(value=ft2):
                @lru_cache(1)
                def dom() -> hydra.core.Type:
                    return ft2.domain
                @lru_cache(1)
                def cod() -> hydra.core.Type:
                    return ft2.codomain
                return hydra.lib.logic.if_else(types_effectively_equal(tx, dom(), targ), (lambda : hydra.lib.flows.pure(cod())), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("in application, expected ", hydra.show.core.type(dom()), " but found ", hydra.show.core.type(targ))))))
            
            case hydra.core.TypeVariable():
                return hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), hydra.schemas.fresh_name())
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat(("left hand side of application (", hydra.show.core.term(fun()), ") is not function-typed (", hydra.show.core.type(tfun), ")", ". types: ", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda p: hydra.lib.strings.cat((hydra.lib.pairs.first(p).value, ": ", hydra.show.core.type(hydra.lib.pairs.second(p))))), hydra.lib.maps.to_list(tx.types))))))
    return hydra.lib.flows.bind(type_of(tx, (), fun()), (lambda tfun: hydra.lib.flows.bind(type_of(tx, (), arg()), (lambda targ: hydra.lib.flows.bind(try_type(tfun, targ), (lambda t: apply_type_arguments_to_type(tx, type_args, t)))))))

def type_of_case_statement(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], cs: hydra.core.CaseStatement) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return cs.type_name
    @lru_cache(1)
    def dflt() -> Maybe[hydra.core.Term]:
        return cs.default
    @lru_cache(1)
    def cases() -> frozenlist[hydra.core.Field]:
        return cs.cases
    @lru_cache(1)
    def cterms() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.map((lambda v1: v1.term), cases())
    return hydra.lib.flows.bind(hydra.lib.flows.map_maybe((lambda e: type_of(tx, (), e)), dflt()), (lambda tdflt: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda e: type_of(tx, (), e)), cterms()), (lambda tcterms: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda t: hydra.lib.flows.map((lambda v1: v1.codomain), hydra.extract.core.function_type(t))), tcterms), (lambda fcods: (cods := hydra.lib.maybes.cat(hydra.lib.lists.cons(tdflt, hydra.lib.lists.map((lambda x1: hydra.lib.maybes.pure(x1)), fcods))), hydra.lib.flows.bind(check_same_type(tx, "case branches", cods), (lambda cod: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(hydra.schemas.nominal_application(tname(), type_args), cod)))))))[1]))))))

def type_of_either(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], et: Either[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def check_length() -> hydra.compute.Flow[T1, None]:
        @lru_cache(1)
        def n() -> int:
            return hydra.lib.lists.length(type_args)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 2), (lambda : hydra.lib.flows.pure(None)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2("either type requires 2 type arguments, got ", hydra.lib.literals.show_int32(n())))))
    return hydra.lib.flows.bind(check_length(), (lambda _: hydra.lib.eithers.either((lambda left_term: hydra.lib.flows.bind(type_of(tx, (), left_term), (lambda left_type: hydra.lib.flows.bind(check_type_variables(tx, left_type), (lambda _2: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left_type, hydra.lib.lists.at(1, type_args)))))))))), (lambda right_term: hydra.lib.flows.bind(type_of(tx, (), right_term), (lambda right_type: hydra.lib.flows.bind(check_type_variables(tx, right_type), (lambda _2: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(hydra.lib.lists.at(0, type_args), right_type))))))))), et)))

def type_of_lambda(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], l: hydra.core.Lambda) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def v() -> hydra.core.Name:
        return l.parameter
    @lru_cache(1)
    def mdom() -> Maybe[hydra.core.Type]:
        return l.domain
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return l.body
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.fail("untyped lambda"), (lambda dom: hydra.lib.flows.bind(check_type_variables(tx, dom), (lambda _: (types2 := hydra.lib.maps.insert(v(), dom, tx.types), hydra.lib.flows.bind(type_of(hydra.typing.TypeContext(types2, tx.metadata, tx.type_variables, tx.lambda_variables, tx.let_variables, tx.inference_context), (), body()), (lambda cod: hydra.lib.flows.bind(check_type_variables(tx, cod), (lambda _2: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod)))))))))[1]))), mdom()), (lambda tbody: apply_type_arguments_to_type(tx, type_args, tbody)))

def type_of_let(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], let_term: hydra.core.Let) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def bs() -> frozenlist[hydra.core.Binding]:
        return let_term.bindings
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return let_term.body
    @lru_cache(1)
    def bnames() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bs())
    @lru_cache(1)
    def bterms() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.map((lambda v1: v1.term), bs())
    def binding_type(b: hydra.core.Binding) -> hydra.compute.Flow[T1, hydra.core.Type]:
        return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("untyped let binding: ", hydra.show.core.binding(b)))), (lambda ts: hydra.lib.flows.pure(hydra.schemas.type_scheme_to_f_type(ts))), b.type)
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: binding_type(x1)), bs()), (lambda btypes: (tx2 := hydra.typing.TypeContext(hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.zip(bnames(), btypes)), tx.types), tx.metadata, tx.type_variables, tx.lambda_variables, tx.let_variables, tx.inference_context), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx2, (), v1)), bterms()), (lambda typeofs: hydra.lib.flows.bind(hydra.lib.logic.if_else(type_lists_effectively_equal(tx, typeofs, btypes), (lambda : type_of(tx2, (), body())), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("binding types disagree: ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), btypes), " and ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), typeofs), " from terms: ", hydra.formatting.show_list((lambda x1: hydra.show.core.term(x1)), bterms())))))), (lambda t: apply_type_arguments_to_type(tx, type_args, t))))))[1]))

def type_of_list(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], els: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.logic.if_else(hydra.lib.lists.null(els), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 1), (lambda : hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeList(hydra.lib.lists.head(type_args))))), (lambda : hydra.lib.flows.fail("list type applied to more or less than one argument")))), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx, (), v1)), els), (lambda eltypes: hydra.lib.flows.bind(check_same_type(tx, "list elements", eltypes), (lambda unified_type: hydra.lib.flows.bind(check_type_variables(tx, unified_type), (lambda _: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeList(unified_type)))))))))))

def type_of_map(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def nonnull() -> hydra.compute.Flow[T0, hydra.core.Type]:
        @lru_cache(1)
        def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.maps.to_list(m)
        return hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx, (), v1)), hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), pairs())), (lambda v1: check_same_type(tx, "map keys", v1))), (lambda kt: hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx, (), v1)), hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), pairs())), (lambda v1: check_same_type(tx, "map values", v1))), (lambda vt: hydra.lib.flows.bind(check_type_variables(tx, kt), (lambda _: hydra.lib.flows.bind(check_type_variables(tx, vt), (lambda _2: apply_type_arguments_to_type(tx, type_args, cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kt, vt))))))))))))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 2), (lambda : hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(hydra.lib.lists.at(0, type_args), hydra.lib.lists.at(1, type_args)))))), (lambda : hydra.lib.flows.fail("map type applied to more or less than two arguments")))), (lambda : nonnull()))

def type_of_maybe(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], mt: Maybe[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def for_nothing() -> hydra.compute.Flow[T1, hydra.core.Type]:
        @lru_cache(1)
        def n() -> int:
            return hydra.lib.lists.length(type_args)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 1), (lambda : hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeMaybe(hydra.lib.lists.head(type_args))))), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2("optional type applied to ", hydra.lib.literals.show_int32(n())), " argument(s). Expected 1."))))
    def for_just(term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Type]:
        return hydra.lib.flows.bind(hydra.lib.flows.bind(type_of(tx, (), term), (lambda term_type: hydra.lib.flows.bind(check_type_variables(tx, term_type), (lambda _: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeMaybe(term_type))))))), (lambda t: apply_type_arguments_to_type(tx, type_args, t)))
    return hydra.lib.maybes.maybe(for_nothing(), (lambda x1: for_just(x1)), mt)

def type_of_pair(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], p: tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def check_length() -> hydra.compute.Flow[T1, None]:
        @lru_cache(1)
        def n() -> int:
            return hydra.lib.lists.length(type_args)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(n(), 2), (lambda : hydra.lib.flows.pure(None)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2("pair type requires 2 type arguments, got ", hydra.lib.literals.show_int32(n())))))
    return hydra.lib.flows.bind(check_length(), (lambda _: (pair_fst := hydra.lib.pairs.first(p), pair_snd := hydra.lib.pairs.second(p), hydra.lib.flows.bind(type_of(tx, (), pair_fst), (lambda first_type: hydra.lib.flows.bind(check_type_variables(tx, first_type), (lambda _2: hydra.lib.flows.bind(type_of(tx, (), pair_snd), (lambda second_type: hydra.lib.flows.bind(check_type_variables(tx, second_type), (lambda _3: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(first_type, second_type)))))))))))))[2]))

def type_of_record(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], record: hydra.core.Record) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return record.type_name
    @lru_cache(1)
    def fields() -> frozenlist[hydra.core.Field]:
        return record.fields
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx, (), v1)), hydra.lib.lists.map((lambda v1: v1.term), fields())), (lambda ftypes: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: check_type_variables(tx, v1)), ftypes), (lambda _: hydra.lib.flows.pure(hydra.schemas.nominal_application(tname(), type_args))))))

def type_of_set(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], els: frozenset[hydra.core.Term]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.logic.if_else(hydra.lib.sets.null(els), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(type_args), 1), (lambda : hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeSet(hydra.lib.lists.head(type_args))))), (lambda : hydra.lib.flows.fail("set type applied to more or less than one argument")))), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: type_of(tx, (), v1)), hydra.lib.sets.to_list(els)), (lambda eltypes: hydra.lib.flows.bind(check_same_type(tx, "set elements", eltypes), (lambda unified_type: hydra.lib.flows.bind(check_type_variables(tx, unified_type), (lambda _: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeSet(unified_type)))))))))))

def type_of_type_application(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], tyapp: hydra.core.TypeApplicationTerm) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return tyapp.body
    @lru_cache(1)
    def t() -> hydra.core.Type:
        return tyapp.type
    return type_of(tx, hydra.lib.lists.cons(t(), type_args), body())

def type_of_type_lambda(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], tl: hydra.core.TypeLambda) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def v() -> hydra.core.Name:
        return tl.parameter
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return tl.body
    @lru_cache(1)
    def vars() -> frozenset[hydra.core.Name]:
        return tx.type_variables
    @lru_cache(1)
    def tx2() -> hydra.typing.TypeContext:
        return hydra.typing.TypeContext(tx.types, tx.metadata, hydra.lib.sets.insert(v(), vars()), tx.lambda_variables, tx.let_variables, tx.inference_context)
    return hydra.lib.flows.bind(type_of(tx2(), (), body()), (lambda t1: hydra.lib.flows.bind(check_type_variables(tx2(), t1), (lambda _: apply_type_arguments_to_type(tx, type_args, cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v(), t1))))))))

def type_of_wrapped_term(tx: hydra.typing.TypeContext, type_args: frozenlist[hydra.core.Type], wt: hydra.core.WrappedTerm) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def tname() -> hydra.core.Name:
        return wt.type_name
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return wt.body
    return hydra.lib.flows.bind(type_of(tx, (), body()), (lambda btype: hydra.lib.flows.bind(check_type_variables(tx, btype), (lambda _: hydra.lib.flows.pure(hydra.schemas.nominal_application(tname(), type_args))))))

def check_type(tx: hydra.typing.TypeContext, term: hydra.core.Term, typ: hydra.core.Type) -> hydra.compute.Flow[T0, None]:
    @lru_cache(1)
    def cx() -> hydra.typing.InferenceContext:
        return tx.inference_context
    @lru_cache(1)
    def vars() -> frozenset[hydra.core.Name]:
        return tx.type_variables
    return hydra.lib.logic.if_else(hydra.constants.debug_inference, (lambda : hydra.lib.flows.bind(type_of(tx, (), term), (lambda t0: hydra.lib.logic.if_else(types_effectively_equal(tx, t0, typ), (lambda : hydra.lib.flows.pure(None)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("type checking failed: expected ", hydra.show.core.type(typ), " but found ", hydra.show.core.type(t0))))))))), (lambda : hydra.lib.flows.pure(None)))

def check_type_subst(cx: hydra.typing.InferenceContext, subst: hydra.typing.TypeSubst) -> hydra.compute.Flow[T0, hydra.typing.TypeSubst]:
    @lru_cache(1)
    def s() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return subst.value
    @lru_cache(1)
    def vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.maps.keys(s()))
    @lru_cache(1)
    def suspect_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.intersection(vars(), hydra.lib.sets.from_list(hydra.lib.maps.keys(cx.schema_types)))
    def is_nominal(ts: hydra.core.TypeScheme) -> bool:
        match hydra.rewriting.deannotate_type(ts.type):
            case hydra.core.TypeRecord():
                return True
            
            case hydra.core.TypeUnion():
                return True
            
            case hydra.core.TypeWrap():
                return True
            
            case _:
                return False
    @lru_cache(1)
    def bad_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda v: hydra.lib.maybes.maybe(False, (lambda x1: is_nominal(x1)), hydra.lexical.dereference_schema_type(v, cx.schema_types))), hydra.lib.sets.to_list(suspect_vars())))
    @lru_cache(1)
    def bad_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.lists.filter((lambda p: hydra.lib.sets.member(hydra.lib.pairs.first(p), bad_vars())), hydra.lib.maps.to_list(s()))
    def print_pair(p: tuple[hydra.core.Name, hydra.core.Type]) -> str:
        return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.pairs.first(p).value, " --> "), hydra.show.core.type(hydra.lib.pairs.second(p)))
    return hydra.lib.logic.if_else(hydra.lib.sets.null(bad_vars()), (lambda : hydra.lib.flows.pure(subst)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Schema type(s) incorrectly unified: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda x1: print_pair(x1)), bad_pairs()))), "}"))))

def to_f_context(cx: hydra.typing.InferenceContext) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    r"""Convert an inference context to a type environment by converting type schemes to System F types."""
    
    return hydra.lib.maps.map((lambda x1: hydra.schemas.type_scheme_to_f_type(x1)), cx.data_types)
