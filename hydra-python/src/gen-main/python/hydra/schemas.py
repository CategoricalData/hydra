# Note: this is an automatically generated file. Do not edit.

r"""Various functions for dereferencing and decoding schema types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.graph
import hydra.lexical
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
import hydra.module
import hydra.monads
import hydra.names
import hydra.reflect
import hydra.rewriting
import hydra.show.core
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.util
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def add_names_to_namespaces(encode_namespace: Callable[[hydra.module.Namespace], T0], names: frozenset[hydra.core.Name], ns0: hydra.module.Namespaces[T0]) -> hydra.module.Namespaces[T0]:
    @lru_cache(1)
    def nss() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(names))))
    def to_pair(ns: hydra.module.Namespace) -> tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, hydra.lib.sets.to_list(nss())))))

def definition_dependency_namespaces(defs: frozenlist[hydra.module.Definition]) -> frozenset[hydra.module.Namespace]:
    r"""Get dependency namespaces from definitions."""
    
    def def_names(def_: hydra.module.Definition) -> frozenset[hydra.core.Name]:
        match def_:
            case hydra.module.DefinitionType(value=type_def):
                return hydra.rewriting.type_dependency_names(True, type_def.type)
            
            case hydra.module.DefinitionTerm(value=term_def):
                return hydra.rewriting.term_dependency_names(True, True, True, term_def.term)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def all_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map(def_names, defs))
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(all_names()))))

def is_encoded_term(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded term (meta-level term)."""
    
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermApplication(value=a):
            return is_encoded_term(a.function)
        
        case hydra.core.TermUnion(value=i):
            return hydra.lib.equality.equal("hydra.core.Term", i.type_name.value)
        
        case _:
            return False

def is_encoded_type(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded type."""
    
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermApplication(value=a):
            return is_encoded_type(a.function)
        
        case hydra.core.TermUnion(value=i):
            return hydra.lib.equality.equal("hydra.core.Type", i.type_name.value)
        
        case _:
            return False

def dependency_namespaces(binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all of a set of terms."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (dep_names := (lambda el: (term := el.term, deannotated_term := hydra.rewriting.deannotate_term(term), data_names := hydra.rewriting.term_dependency_names(binds, with_prims, with_noms, term), schema_names := hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.maybes.maybe(hydra.lib.sets.empty(), (lambda ts: hydra.rewriting.type_dependency_names(True, ts.type)), el.type)), (lambda : hydra.lib.sets.empty())), hydra.lib.logic.if_else(is_encoded_type(deannotated_term), (lambda : hydra.lib.flows.bind(hydra.monads.with_trace("dependency namespace (type)", hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, term))), (lambda typ: hydra.lib.flows.pure(hydra.lib.sets.unions((data_names, schema_names, hydra.rewriting.type_dependency_names(True, typ))))))), (lambda : hydra.lib.logic.if_else(is_encoded_term(deannotated_term), (lambda : hydra.lib.flows.bind(hydra.monads.with_trace("dependency namespace (term)", hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.term(cx, term))), (lambda decoded_term: hydra.lib.flows.pure(hydra.lib.sets.unions((data_names, schema_names, hydra.rewriting.term_dependency_names(binds, with_prims, with_noms, decoded_term))))))), (lambda : hydra.lib.flows.pure(hydra.lib.sets.unions((data_names, schema_names))))))))[4]), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: dep_names(x1)), els), (lambda names_list: hydra.lib.flows.pure(hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(hydra.lib.sets.delete(hydra.constants.placeholder_name, hydra.lib.sets.unions(names_list))))))))))[1]))

def dereference_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    r"""Dereference a type name to get the actual type."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.lib.flows.bind(hydra.lexical.dereference_element(name), (lambda mel: hydra.lib.maybes.maybe(hydra.lib.flows.pure(Nothing()), (lambda el: hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.monads.with_trace("dereference type", hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term))))), mel)))))

def element_as_type_application_term(el: hydra.core.Binding) -> hydra.compute.Flow[T0, hydra.core.TypeApplicationTerm]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail("missing element type"), (lambda ts: hydra.lib.flows.pure(hydra.core.TypeApplicationTerm(el.term, ts.type))), el.type)

def elements_with_dependencies(original: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Binding]]:
    r"""Get elements with their dependencies."""
    
    def dep_names(el: hydra.core.Binding) -> frozenlist[hydra.core.Name]:
        return hydra.lib.sets.to_list(hydra.rewriting.term_dependency_names(True, False, False, el.term))
    @lru_cache(1)
    def all_dep_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda v1: v1.name), original), hydra.lib.lists.concat(hydra.lib.lists.map(dep_names, original))))
    return hydra.lib.flows.map_list(hydra.lexical.require_element, all_dep_names())

def extend_type_context_for_lambda(tcontext: hydra.typing.TypeContext, lam: hydra.core.Lambda) -> hydra.core.Type:
    r"""Extend a type context by descending into a System F lambda body."""
    
    @lru_cache(1)
    def var() -> hydra.core.Type:
        return lam.parameter
    return hydra.typing.TypeContext(hydra.lib.maybes.maybe(tcontext.types, (lambda dom: hydra.lib.maps.insert(var(), dom, tcontext.types)), lam.domain), hydra.lib.maps.delete(var(), tcontext.metadata), tcontext.type_variables, hydra.lib.sets.insert(var(), tcontext.lambda_variables), tcontext.inference_context)

def type_scheme_to_f_type(ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Convert a type scheme to a forall type."""
    
    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return ts.variables
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return ts.type
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t)))), body(), hydra.lib.lists.reverse(vars()))

def extend_type_context_for_let(for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], tcontext: hydra.typing.TypeContext, letrec: hydra.core.Let) -> hydra.core.Type:
    r"""Extend a type context by descending into a let body."""
    
    @lru_cache(1)
    def bindings() -> frozenlist[hydra.core.Binding]:
        return letrec.bindings
    return hydra.typing.TypeContext(hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, hydra.lib.maybes.maybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_"))), (lambda ts: type_scheme_to_f_type(ts)), b.type))), bindings())), tcontext.types), hydra.lib.lists.foldl((lambda m, b: hydra.lib.maybes.maybe(hydra.lib.maps.delete(b.name, m), (lambda t: hydra.lib.maps.insert(b.name, t, m)), for_binding(tcontext, b))), tcontext.metadata, bindings()), tcontext.type_variables, hydra.lib.lists.foldl((lambda s, b: hydra.lib.sets.delete(b.name, s)), tcontext.lambda_variables, bindings()), tcontext.inference_context)

def extend_type_context_for_type_lambda(tcontext: hydra.typing.TypeContext, tlam: hydra.core.TypeLambda) -> hydra.core.Type:
    r"""Extend a type context by descending into a System F type lambda body."""
    
    @lru_cache(1)
    def name() -> hydra.core.Type:
        return tlam.parameter
    return hydra.typing.TypeContext(tcontext.types, tcontext.metadata, hydra.lib.sets.insert(name(), tcontext.type_variables), tcontext.lambda_variables, tcontext.inference_context)

def f_type_to_type_scheme(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Convert a forall type to a type scheme."""
    
    def gather_forall(vars: frozenlist[hydra.core.Name], typ2: hydra.core.Type) -> hydra.core.Type:
        match hydra.rewriting.deannotate_type(typ2):
            case hydra.core.TypeForall(value=ft):
                return gather_forall(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ2, Nothing())
    return gather_forall((), typ)

def field_map(fields: frozenlist[hydra.core.Field]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    def to_pair(f: hydra.core.Field) -> tuple[hydra.core.Name, hydra.core.Term]:
        return (f.name, f.term)
    return hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, fields))

def field_type_map(fields: frozenlist[hydra.core.FieldType]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    def to_pair(f: hydra.core.FieldType) -> tuple[hydra.core.Name, hydra.core.Type]:
        return (f.name, f.type)
    return hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, fields))

def field_types(t: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get field types from a record or union type."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (to_map := (lambda fields: hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), fields))), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[1]))

def find_field_type(fname: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    @lru_cache(1)
    def matching_fields() -> frozenlist[hydra.core.FieldType]:
        return hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2("No such field: ", fname.value))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields()), 1), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(matching_fields()).type)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2("Multiple fields named ", fname.value))))))

def normal_type_variable(i: int) -> hydra.core.Type:
    r"""Type variable naming convention follows Haskell: t0, t1, etc."""
    
    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(i)))

@lru_cache(1)
def fresh_name() -> hydra.compute.Flow[T0, hydra.core.Name]:
    return hydra.lib.flows.map(normal_type_variable, hydra.annotations.next_count(hydra.constants.key_fresh_type_variable_count))

def fresh_names(n: int) -> hydra.compute.Flow[T0, frozenlist[hydra.core.Name]]:
    return hydra.lib.flows.sequence(hydra.lib.lists.replicate(n, fresh_name()))

def fully_strip_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeForall(value=ft):
            return fully_strip_type(ft.body)
        
        case _:
            return typ

def graph_as_term(g: hydra.graph.Graph) -> hydra.core.Type:
    r"""Convert a graph to a term, taking advantage of the built-in duality between graphs and terms."""
    
    def to_binding(el: hydra.core.Binding) -> hydra.core.Type:
        @lru_cache(1)
        def name() -> hydra.core.Type:
            return el.name
        @lru_cache(1)
        def term() -> hydra.core.Type:
            return el.term
        @lru_cache(1)
        def mts() -> Maybe[hydra.core.TypeScheme]:
            return el.type
        return hydra.core.Binding(name(), term(), mts())
    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(to_binding, hydra.lib.maps.elems(g.elements)), g.body)))

def graph_as_types(sg: hydra.graph.Graph) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Decode a schema graph which encodes a set of named types."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (els := hydra.lib.maps.elems(sg.elements), to_pair := (lambda el: hydra.lib.flows.bind(hydra.monads.with_trace(hydra.lib.strings.cat2("graph as types: ", el.name.value), hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term))), (lambda typ: hydra.lib.flows.pure((el.name, typ))))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: to_pair(x1)), els), (lambda pairs: hydra.lib.flows.pure(hydra.lib.maps.from_list(pairs)))))[2]))

def schema_graph_to_typing_environment(g: hydra.graph.Graph) -> hydra.compute.Flow[T0, FrozenDict[hydra.core.Name, hydra.core.TypeScheme]]:
    def to_type_scheme(vars: frozenlist[hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                return to_type_scheme(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ, Nothing())
    def to_pair(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[tuple[hydra.core.Name, hydra.core.TypeScheme]]]:
        return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (for_term := (lambda term: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.map((lambda typ: Just(f_type_to_type_scheme(typ))), hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term))), (lambda ts: hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.TypeScheme"))), Nothing())), (lambda : hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type_scheme(cx, el.term)))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing())), (lambda : hydra.lib.flows.map((lambda decoded: Just(to_type_scheme((), decoded))), hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term)))), (lambda : for_term(hydra.rewriting.deannotate_term(el.term))))))), el.type), (lambda mts: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda ts: (el.name, ts)), mts)))))[1]))
    return hydra.monads.with_trace("schema graph to typing environment", hydra.monads.with_state(g, hydra.lib.flows.bind(hydra.lib.flows.map_list(to_pair, hydra.lib.maps.elems(g.elements)), (lambda mpairs: hydra.lib.flows.pure(hydra.lib.maps.from_list(hydra.lib.maybes.cat(mpairs)))))))

def graph_to_inference_context(graph: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.InferenceContext]:
    @lru_cache(1)
    def schema() -> hydra.core.Type:
        return hydra.lib.maybes.from_maybe(graph, graph.schema)
    @lru_cache(1)
    def prim_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (p.name, p.type)), hydra.lib.maps.elems(graph.primitives)))
    @lru_cache(1)
    def var_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, ts)), b.type)), hydra.lib.maps.elems(graph.elements))))
    return hydra.lib.flows.bind(schema_graph_to_typing_environment(schema()), (lambda schema_types: hydra.lib.flows.pure(hydra.typing.InferenceContext(schema_types, prim_types(), var_types(), hydra.lib.maps.empty(), False))))

def graph_to_type_context(graph: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.TypeContext]:
    return hydra.lib.flows.bind(graph_to_inference_context(graph), (lambda ix: (element_types := hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, type_scheme_to_f_type(ts))), b.type)), hydra.lib.maps.elems(graph.elements)))), hydra.lib.flows.pure(hydra.typing.TypeContext(element_types, hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), ix)))[1]))

def instantiate_type_scheme(scheme: hydra.core.TypeScheme) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    @lru_cache(1)
    def old_vars() -> frozenlist[hydra.core.Name]:
        return scheme.variables
    return hydra.lib.flows.bind(fresh_names(hydra.lib.lists.length(old_vars())), (lambda new_vars: (subst := hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(old_vars(), hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), new_vars)))), name_subst := hydra.lib.maps.from_list(hydra.lib.lists.zip(old_vars(), new_vars)), renamed_constraints := hydra.lib.maybes.map((lambda old_constraints: hydra.lib.maps.from_list(hydra.lib.lists.map((lambda kv: (hydra.lib.maybes.from_maybe(hydra.lib.pairs.first(kv), hydra.lib.maps.lookup(hydra.lib.pairs.first(kv), name_subst)), hydra.lib.pairs.second(kv))), hydra.lib.maps.to_list(old_constraints)))), scheme.constraints), hydra.lib.flows.pure(hydra.core.TypeScheme(new_vars, hydra.substitution.subst_in_type(subst, scheme.type), renamed_constraints)))[3]))

def type_to_type_scheme(t0: hydra.core.Type) -> hydra.core.Type:
    r"""Convert a (System F -style) type to a type scheme."""
    
    def helper(vars: frozenlist[hydra.core.Name], t: hydra.core.Type) -> hydra.core.Type:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeForall(value=ft):
                return helper(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), t, Nothing())
    return helper((), t0)

def instantiate_type(typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.flows.bind(instantiate_type_scheme(type_to_type_scheme(typ)), (lambda ts: hydra.lib.flows.pure(type_scheme_to_f_type(ts))))

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""
    
    match v1:
        case hydra.core.TypeUnit():
            return True
        
        case _:
            return False

def is_enum_row_type(rt: hydra.core.RowType) -> bool:
    r"""Check if a row type represents an enum (all fields are unit-typed)."""
    
    return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda f: is_unit_type(hydra.rewriting.deannotate_type(f.type))), rt.fields))

def is_enum_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is an enum type."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeUnion(value=rt):
            return is_enum_row_type(rt)
        
        case _:
            return False

def type_dependencies(with_schema: bool, transform: Callable[[hydra.core.Type], hydra.core.Type], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get all type dependencies for a given type name."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (require_type := (lambda name2: hydra.monads.with_trace(hydra.lib.strings.cat2("type dependencies of ", name2.value), hydra.lib.flows.bind(hydra.lexical.require_element(name2), (lambda el: hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term)))))), to_pair := (lambda name2: hydra.lib.flows.bind(require_type(name2), (lambda typ: hydra.lib.flows.pure((name2, transform(typ)))))), deps := (lambda seeds, names: hydra.lib.logic.if_else(hydra.lib.sets.null(seeds), (lambda : hydra.lib.flows.pure(names)), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list(to_pair, hydra.lib.sets.to_list(seeds)), (lambda pairs: (new_names := hydra.lib.maps.union(names, hydra.lib.maps.from_list(pairs)), refs := hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), hydra.lib.lists.map((lambda pair: hydra.rewriting.type_dependency_names(with_schema, hydra.lib.pairs.second(pair))), pairs)), visited := hydra.lib.sets.from_list(hydra.lib.maps.keys(names)), new_seeds := hydra.lib.sets.difference(refs, visited), deps(new_seeds, new_names))[4]))))), hydra.monads.with_trace("type dependencies", deps(hydra.lib.sets.singleton(name), hydra.lib.maps.empty())))[3]))

def is_serializable(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, bool]:
    r"""Check if an element is serializable (no function types in dependencies)."""
    
    def variants(typ: hydra.core.Type) -> frozenlist[hydra.variants.TypeVariant]:
        return hydra.lib.lists.map(hydra.reflect.type_variant, hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ))
    return hydra.lib.flows.map((lambda deps: (all_variants := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map(variants, hydra.lib.maps.elems(deps)))), hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants)))[1]), type_dependencies(False, (lambda x1: hydra.lib.equality.identity(x1)), el.name))

def is_serializable_by_name(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, bool]:
    r"""Check if a type (by name) is serializable, resolving all type dependencies."""
    
    def variants(typ: hydra.core.Type) -> frozenlist[hydra.variants.TypeVariant]:
        return hydra.lib.lists.map(hydra.reflect.type_variant, hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ))
    return hydra.lib.flows.map((lambda deps: (all_variants := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map(variants, hydra.lib.maps.elems(deps)))), hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants)))[1]), type_dependencies(False, (lambda x1: hydra.lib.equality.identity(x1)), name))

def is_serializable_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is serializable (no function types in the type itself)."""
    
    @lru_cache(1)
    def all_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map(hydra.reflect.type_variant, hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ)))
    return hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants()))

def is_type(t: hydra.core.Type) -> bool:
    r"""Check whether a type is a type (always true for non-encoded types)."""
    
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeApplication(value=a):
            return is_type(a.function)
        
        case hydra.core.TypeForall(value=l):
            return is_type(l.body)
        
        case hydra.core.TypeUnion(value=rt):
            return hydra.lib.equality.equal("hydra.core.Type", rt.type_name.value)
        
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.equality.equal(v, hydra.core.Name("hydra.core.Type"))
        
        case _:
            return False

def is_unit_term(v1: hydra.core.Term) -> bool:
    r"""Check whether a term is the unit term."""
    
    match v1:
        case hydra.core.TermUnit():
            return True
        
        case _:
            return False

def module_contains_binary_literals(mod: hydra.module.Module) -> bool:
    r"""Check whether a module contains any binary literal values."""
    
    def check_term(found: bool, term: hydra.core.Term) -> bool:
        def _hoist_check_term_1(v1: hydra.core.Literal) -> bool:
            match v1:
                case hydra.core.LiteralBinary():
                    return True
                
                case _:
                    return False
        def _hoist_check_term_2(v1: hydra.core.Term) -> bool:
            match v1:
                case hydra.core.TermLiteral(value=lit):
                    return _hoist_check_term_1(lit)
                
                case _:
                    return False
        return hydra.lib.logic.or_(found, _hoist_check_term_2(term))
    def term_contains_binary(term: hydra.core.Term) -> bool:
        return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, check_term, False, term)
    return hydra.lib.lists.foldl((lambda acc, el: hydra.lib.logic.or_(acc, term_contains_binary(el.term))), False, mod.elements)

def module_dependency_namespaces(binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all elements of a module, excluding the module's own namespace."""
    
    return hydra.lib.flows.bind(dependency_namespaces(binds, with_prims, with_noms, with_schema, mod.elements), (lambda deps: hydra.lib.flows.pure(hydra.lib.sets.delete(mod.namespace, deps))))

def namespaces_for_definitions(encode_namespace: Callable[[hydra.module.Namespace], T0], focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.module.Namespaces[T0]:
    @lru_cache(1)
    def nss() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.delete(focus_ns, definition_dependency_namespaces(defs))
    def to_pair(ns: hydra.module.Namespace) -> tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.module.Namespaces(to_pair(focus_ns), hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, hydra.lib.sets.to_list(nss()))))

def nominal_application(tname: hydra.core.Name, args: frozenlist[hydra.core.Type]) -> hydra.core.Type:
    r"""Apply type arguments to a nominal type."""
    
    return hydra.lib.lists.foldl((lambda t, a: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t, a)))), cast(hydra.core.Type, hydra.core.TypeVariable(tname)), args)

def partition_definitions(defs: frozenlist[hydra.module.Definition]) -> tuple[frozenlist[hydra.module.TypeDefinition], frozenlist[hydra.module.TermDefinition]]:
    r"""Partition a list of definitions into type definitions and term definitions."""
    
    def get_type(def_: hydra.module.Definition) -> Maybe[hydra.module.TypeDefinition]:
        match def_:
            case hydra.module.DefinitionType(value=td):
                return Just(td)
            
            case hydra.module.DefinitionTerm():
                return Nothing()
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def get_term(def_: hydra.module.Definition) -> Maybe[hydra.module.TermDefinition]:
        match def_:
            case hydra.module.DefinitionType():
                return Nothing()
            
            case hydra.module.DefinitionTerm(value=td):
                return Just(td)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return (hydra.lib.maybes.cat(hydra.lib.lists.map(get_type, defs)), hydra.lib.maybes.cat(hydra.lib.lists.map(get_term, defs)))

def require_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Require a type by name."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.monads.with_trace(hydra.lib.strings.cat2("require type ", name.value), hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lexical.require_element(name)), (lambda el: hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, el.term)))))))

def require_row_type(label: str, getter: Callable[[hydra.core.Type], Maybe[T0]], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    def raw_type(t: hydra.core.Type) -> hydra.core.Type:
        match t:
            case hydra.core.TypeAnnotated(value=at):
                return raw_type(at.body)
            
            case hydra.core.TypeForall(value=ft):
                return raw_type(ft.body)
            
            case _:
                return t
    return hydra.lib.flows.bind(require_type(name), (lambda t: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat((name.value, " does not resolve to a ", label, " type: ", hydra.show.core.type(t)))), (lambda x1: hydra.lib.flows.pure(x1)), getter(raw_type(t)))))

def require_record_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    r"""Require a name to resolve to a record type."""
    
    def to_record(t: hydra.core.Type) -> Maybe[hydra.core.RowType]:
        match t:
            case hydra.core.TypeRecord(value=rt):
                return Just(rt)
            
            case _:
                return Nothing()
    return require_row_type("record type", to_record, name)

def require_schema_type(cx: hydra.typing.InferenceContext, tname: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    @lru_cache(1)
    def types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return cx.schema_types
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("No such schema type: ", tname.value, ". Available types are: ", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(types())))))), (lambda ts: instantiate_type_scheme(hydra.rewriting.deannotate_type_scheme_recursive(ts))), hydra.lib.maps.lookup(tname, types()))

def require_union_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    r"""Require a name to resolve to a union type."""
    
    def to_union(t: hydra.core.Type) -> Maybe[hydra.core.RowType]:
        match t:
            case hydra.core.TypeUnion(value=rt):
                return Just(rt)
            
            case _:
                return Nothing()
    return require_row_type("union", to_union, name)

def require_union_field(tname: hydra.core.Name, fname: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Require a field type from a union type."""
    
    def with_row_type(rt: hydra.core.RowType) -> hydra.compute.Flow[T0, hydra.core.Type]:
        @lru_cache(1)
        def matches() -> frozenlist[hydra.core.FieldType]:
            return hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name, fname)), rt.fields)
        return hydra.lib.logic.if_else(hydra.lib.lists.null(matches()), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat(("no field \"", fname.value, "\" in union type \"", tname.value)))), (lambda : hydra.lib.flows.pure(hydra.lib.lists.head(matches()).type)))
    return hydra.lib.flows.bind(require_union_type(tname), (lambda x1: with_row_type(x1)))

def resolve_type(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    def _hoist_hydra_schemas_resolve_type_1(cx: hydra.graph.Graph, typ: hydra.core.Type, v1: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
        match v1:
            case hydra.core.TypeVariable(value=name):
                return hydra.lexical.with_schema_context(hydra.lib.flows.bind(hydra.lexical.resolve_term(name), (lambda mterm: hydra.lib.maybes.maybe(hydra.lib.flows.pure(Nothing()), (lambda t: hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, t)))), mterm))))
            
            case _:
                return hydra.lib.flows.pure(Just(typ))
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: _hoist_hydra_schemas_resolve_type_1(cx, typ, hydra.rewriting.deannotate_type(typ))))

def term_as_graph(term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
    r"""Find the equivalent graph representation of a term."""
    
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def bindings() -> frozenlist[hydra.core.Binding]:
                return lt.bindings
            def from_binding(b: hydra.core.Binding) -> tuple[hydra.core.Name, hydra.core.Binding]:
                @lru_cache(1)
                def name() -> hydra.core.Type:
                    return b.name
                @lru_cache(1)
                def term() -> hydra.core.Type:
                    return b.term
                @lru_cache(1)
                def ts() -> Maybe[hydra.core.TypeScheme]:
                    return b.type
                return (name(), hydra.core.Binding(name(), term(), ts()))
            return hydra.lib.maps.from_list(hydra.lib.lists.map(from_binding, bindings()))
        
        case _:
            return hydra.lib.maps.empty()

def topological_sort_type_definitions(defs: frozenlist[hydra.module.TypeDefinition]) -> frozenlist[frozenlist[hydra.module.TypeDefinition]]:
    r"""Topologically sort type definitions by dependencies."""
    
    def to_pair(def_: hydra.module.TypeDefinition) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (def_.name, hydra.lib.sets.to_list(hydra.rewriting.type_dependency_names(False, def_.type)))
    @lru_cache(1)
    def name_to_def() -> FrozenDict[hydra.core.Name, hydra.module.TypeDefinition]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda d: (d.name, d)), defs))
    @lru_cache(1)
    def sorted() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(hydra.lib.lists.map(to_pair, defs))
    return hydra.lib.lists.map((lambda names: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, name_to_def())), names))), sorted())

def types_to_elements(type_map: FrozenDict[hydra.core.Name, hydra.core.Type]) -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
    r"""Encode a map of named types to a map of elements."""
    
    def to_element(pair: tuple[hydra.core.Name, hydra.core.Type]) -> tuple[hydra.core.Name, hydra.core.Binding]:
        @lru_cache(1)
        def name() -> hydra.core.Type:
            return hydra.lib.pairs.first(pair)
        return (name(), hydra.core.Binding(name(), hydra.encode.core.type(hydra.lib.pairs.second(pair)), Nothing()))
    return hydra.lib.maps.from_list(hydra.lib.lists.map(to_element, hydra.lib.maps.to_list(type_map)))

def with_lambda_context(get_context: Callable[[T0], hydra.typing.TypeContext], set_context: Callable[[hydra.typing.TypeContext, T0], T1], env: T0, lam: hydra.core.Lambda, body: Callable[[T1], T2]) -> T2:
    @lru_cache(1)
    def new_context() -> hydra.core.Type:
        return extend_type_context_for_lambda(get_context(env), lam)
    return body(set_context(new_context(), env))

def with_let_context(get_context: Callable[[T0], hydra.typing.TypeContext], set_context: Callable[[hydra.typing.TypeContext, T0], T1], for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], env: T0, letrec: hydra.core.Let, body: Callable[[T1], T2]) -> T2:
    @lru_cache(1)
    def new_context() -> hydra.core.Type:
        return extend_type_context_for_let(for_binding, get_context(env), letrec)
    return body(set_context(new_context(), env))

def with_type_lambda_context(get_context: Callable[[T0], hydra.typing.TypeContext], set_context: Callable[[hydra.typing.TypeContext, T0], T1], env: T0, tlam: hydra.core.TypeLambda, body: Callable[[T1], T2]) -> T2:
    @lru_cache(1)
    def new_context() -> hydra.core.Type:
        return extend_type_context_for_type_lambda(get_context(env), tlam)
    return body(set_context(new_context(), env))
