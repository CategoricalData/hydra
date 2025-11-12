# Note: this is an automatically generated file. Do not edit.

r"""Various functions for dereferencing and decoding schema types."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
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
import hydra.lib.sets
import hydra.lib.strings
import hydra.meta
import hydra.module
import hydra.monads
import hydra.names
import hydra.rewriting
import hydra.show.core
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.variants

def add_names_to_namespaces[T0](encode_namespace: Callable[[hydra.module.Namespace], T0], names: frozenset[hydra.core.Name], ns0: hydra.module.Namespaces[T0]) -> hydra.module.Namespaces[T0]:
    nss = hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(names))))
    def to_pair(ns: hydra.module.Namespace) -> Tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return cast(hydra.module.Namespaces[T0], hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, cast(FrozenDict[hydra.module.Namespace, T0], hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, hydra.lib.sets.to_list(nss)))))))

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
    all_names = hydra.lib.sets.unions(hydra.lib.lists.map(def_names, defs))
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(all_names))))

def dependency_namespaces(binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all of a set of terms."""
    
    def dep_names(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.core.Name]]:
        term = el.term
        data_names = hydra.rewriting.term_dependency_names(binds, with_prims, with_noms, term)
        schema_names = hydra.lib.logic.if_else(with_schema, hydra.lib.maybes.maybe(cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), (lambda ts: hydra.rewriting.type_dependency_names(True, ts.type)), el.type), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()))
        return hydra.lib.logic.if_else(hydra.encode.core.is_encoded_type(hydra.rewriting.deannotate_term(term)), hydra.lib.flows.bind(hydra.monads.with_trace("dependency namespace", hydra.decode.core.type(term)), (lambda typ: hydra.lib.flows.pure(hydra.lib.sets.unions((data_names, schema_names, hydra.rewriting.type_dependency_names(True, typ)))))), hydra.lib.flows.pure(hydra.lib.sets.unions((data_names, schema_names))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(dep_names, els), (lambda names_list: hydra.lib.flows.pure(hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(hydra.names.namespace_of, hydra.lib.sets.to_list(hydra.lib.sets.delete(hydra.constants.placeholder_name, hydra.lib.sets.unions(names_list)))))))))

def dereference_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    r"""Dereference a type name to get the actual type."""
    
    return hydra.lib.flows.bind(hydra.lexical.dereference_element(name), (lambda mel: hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Type], Nothing())), (lambda el: hydra.lib.flows.map(cast(Callable[[hydra.core.Type], Maybe[hydra.core.Type]], hydra.lib.maybes.pure), hydra.monads.with_trace("dereference type", hydra.decode.core.type(el.term)))), mel)))

def element_as_type_application_term[T0](el: hydra.core.Binding) -> hydra.compute.Flow[T0, hydra.core.TypeApplicationTerm]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail("missing element type"), (lambda ts: hydra.lib.flows.pure(hydra.core.TypeApplicationTerm(el.term, ts.type))), el.type)

def elements_with_dependencies(original: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Binding]]:
    r"""Get elements with their dependencies."""
    
    def dep_names(el: hydra.core.Binding) -> frozenlist[hydra.core.Name]:
        return hydra.lib.sets.to_list(hydra.rewriting.term_dependency_names(True, False, False, el.term))
    all_dep_names = hydra.lib.lists.nub(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda v1: v1.name), original), hydra.lib.lists.concat(hydra.lib.lists.map(dep_names, original))))
    return hydra.lib.flows.map_list(hydra.lexical.require_element, all_dep_names)

def extend_type_context_for_lambda(tcontext: hydra.typing.TypeContext, lam: hydra.core.Lambda) -> hydra.typing.TypeContext:
    r"""Extend a type context by descending into a System F lambda body."""
    
    var = lam.parameter
    dom = hydra.lib.maybes.from_just(lam.domain)
    return hydra.typing.TypeContext(hydra.lib.maps.insert(var, dom, tcontext.types), tcontext.variables, tcontext.inference_context)

def type_scheme_to_f_type(ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Convert a type scheme to a forall type."""
    
    vars = ts.variables
    body = ts.type
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t)))), body, hydra.lib.lists.reverse(vars))

def extend_type_context_for_let(tcontext: hydra.typing.TypeContext, letrec: hydra.core.Let) -> hydra.typing.TypeContext:
    r"""Extend a type context by descending into a let body."""
    
    bindings = letrec.bindings
    return hydra.typing.TypeContext(hydra.lib.maps.union(tcontext.types, cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, type_scheme_to_f_type(hydra.lib.maybes.from_just(b.type)))), bindings)))), tcontext.variables, tcontext.inference_context)

def extend_type_context_for_type_lambda(tcontext: hydra.typing.TypeContext, tlam: hydra.core.TypeLambda) -> hydra.typing.TypeContext:
    r"""Extend a type context by descending into a System F type lambda body."""
    
    name = tlam.parameter
    return hydra.typing.TypeContext(tcontext.types, hydra.lib.sets.insert(name, tcontext.variables), tcontext.inference_context)

def f_type_to_type_scheme(typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Convert a forall type to a type scheme."""
    
    def gather_forall(vars: frozenlist[hydra.core.Name], typ2: hydra.core.Type) -> hydra.core.TypeScheme:
        match hydra.rewriting.deannotate_type(typ2):
            case hydra.core.TypeForall(value=ft):
                return gather_forall(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ2)
    return gather_forall(cast(frozenlist[hydra.core.Name], ()), typ)

def field_map(fields: frozenlist[hydra.core.Field]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    def to_pair(f: hydra.core.Field) -> Tuple[hydra.core.Name, hydra.core.Term]:
        return (f.name, f.term)
    return cast(FrozenDict[hydra.core.Name, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, fields)))

def field_type_map(fields: frozenlist[hydra.core.FieldType]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    def to_pair(f: hydra.core.FieldType) -> Tuple[hydra.core.Name, hydra.core.Type]:
        return (f.name, f.type)
    return cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, fields)))

def field_types(t: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get field types from a record or union type."""
    
    def to_map(fields: frozenlist[hydra.core.FieldType]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), fields)))
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeForall(value=ft):
            return field_types(ft.body)
        
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.flows.pure(to_map(rt.fields))
        
        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.flows.pure(to_map(rt2.fields))
        
        case hydra.core.TypeVariable(value=name):
            return hydra.monads.with_trace(hydra.lib.strings.cat2("field types of ", name.value), hydra.lib.flows.bind(hydra.lexical.require_element(name), (lambda el: hydra.lib.flows.bind(hydra.monads.with_trace("field types", hydra.decode.core.type(el.term)), field_types))))
        
        case _:
            return hydra.monads.unexpected("record or union type", hydra.show.core.type(t))

def find_field_type[T0](fname: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> hydra.compute.Flow[T0, hydra.core.Type]:
    matching_fields = hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), hydra.lib.flows.fail(hydra.lib.strings.cat2("No such field: ", fname.value)), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields), 1), hydra.lib.flows.pure(hydra.lib.lists.head(matching_fields).type), hydra.lib.flows.fail(hydra.lib.strings.cat2("Multiple fields named ", fname.value))))

def normal_type_variable(i: int) -> hydra.core.Name:
    r"""Type variable naming convention follows Haskell: t0, t1, etc."""
    
    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(i)))

def fresh_name[T0]() -> hydra.compute.Flow[T0, hydra.core.Name]:
    return hydra.lib.flows.map(normal_type_variable, hydra.annotations.next_count(hydra.constants.key_fresh_type_variable_count))

def fresh_names[T0](n: int) -> hydra.compute.Flow[T0, frozenlist[hydra.core.Name]]:
    return hydra.lib.flows.sequence(hydra.lib.lists.replicate(n, cast(hydra.compute.Flow[T0, hydra.core.Name], fresh_name)))

def fully_strip_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeForall(value=ft):
            return fully_strip_type(ft.body)
        
        case _:
            return typ

def graph_as_term(g: hydra.graph.Graph) -> hydra.core.Term:
    r"""Convert a graph to a term, taking advantage of the built-in duality between graphs and terms."""
    
    def to_binding(el: hydra.core.Binding) -> hydra.core.Binding:
        name = el.name
        term = el.term
        mts = el.type
        return hydra.core.Binding(name, term, mts)
    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(to_binding, hydra.lib.maps.elems(g.elements)), g.body)))

def graph_as_types(sg: hydra.graph.Graph) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Decode a schema graph which encodes a set of named types."""
    
    els = hydra.lib.maps.elems(sg.elements)
    def to_pair(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.flows.bind(hydra.monads.with_trace(hydra.lib.strings.cat(("graph as types: ", el.name.value)), hydra.decode.core.type(el.term)), (lambda typ: hydra.lib.flows.pure((el.name, typ))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(to_pair, els), (lambda pairs: hydra.lib.flows.pure(cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(pairs)))))

def schema_graph_to_typing_environment[T0](g: hydra.graph.Graph) -> hydra.compute.Flow[T0, FrozenDict[hydra.core.Name, hydra.core.TypeScheme]]:
    def to_type_scheme(vars: frozenlist[hydra.core.Name], typ: hydra.core.Type) -> hydra.core.TypeScheme:
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                return to_type_scheme(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ)
    def to_pair(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[Tuple[hydra.core.Name, hydra.core.TypeScheme]]]:
        def for_term(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.TypeScheme]]:
            match term:
                case hydra.core.TermRecord(value=r):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(r.type_name, hydra.core.Name("hydra.core.TypeScheme")), hydra.lib.flows.map(cast(Callable[[hydra.core.TypeScheme], Maybe[hydra.core.TypeScheme]], hydra.lib.maybes.pure), hydra.decode.core.type_scheme(el.term)), hydra.lib.flows.pure(cast(Maybe[hydra.core.TypeScheme], Nothing())))
                
                case hydra.core.TermUnion(value=i):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(i.type_name, hydra.core.Name("hydra.core.Type")), hydra.lib.flows.map((lambda decoded: cast(Maybe[hydra.core.TypeScheme], Just(to_type_scheme(cast(frozenlist[hydra.core.Name], ()), decoded)))), hydra.decode.core.type(el.term)), hydra.lib.flows.pure(cast(Maybe[hydra.core.TypeScheme], Nothing())))
                
                case _:
                    return hydra.lib.flows.pure(cast(Maybe[hydra.core.TypeScheme], Nothing()))
        return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.map((lambda typ: cast(Maybe[hydra.core.TypeScheme], Just(f_type_to_type_scheme(typ)))), hydra.decode.core.type(el.term)), (lambda ts: hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.TypeScheme"))))), hydra.lib.flows.map(cast(Callable[[hydra.core.TypeScheme], Maybe[hydra.core.TypeScheme]], hydra.lib.maybes.pure), hydra.decode.core.type_scheme(el.term)), hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))))), hydra.lib.flows.map((lambda decoded: cast(Maybe[hydra.core.TypeScheme], Just(to_type_scheme(cast(frozenlist[hydra.core.Name], ()), decoded)))), hydra.decode.core.type(el.term)), for_term(hydra.rewriting.deannotate_term(el.term))))), el.type), (lambda mts: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda ts: (el.name, ts)), mts))))
    return hydra.monads.with_trace("schema graph to typing environment", hydra.monads.with_state(g, hydra.lib.flows.bind(hydra.lib.flows.map_list(to_pair, hydra.lib.maps.elems(g.elements)), (lambda mpairs: hydra.lib.flows.pure(cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.from_list(hydra.lib.maybes.cat(mpairs))))))))

def graph_to_inference_context[T0](graph: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.InferenceContext]:
    schema = hydra.lib.maybes.from_maybe(graph, graph.schema)
    prim_types = cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (p.name, p.type)), hydra.lib.maps.elems(graph.primitives))))
    def var_types[T1, T2]() -> FrozenDict[T1, T2]:
        return cast(FrozenDict[T1, T2], hydra.lib.maps.empty())
    return hydra.lib.flows.bind(schema_graph_to_typing_environment(schema), (lambda schema_types: hydra.lib.flows.pure(hydra.typing.InferenceContext(schema_types, prim_types, cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], var_types), False))))

def graph_to_type_context[T0](graph: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.typing.TypeContext]:
    return hydra.lib.flows.bind(graph_to_inference_context(graph), (lambda ix: hydra.lib.flows.pure(hydra.typing.TypeContext(cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.empty()), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), ix))))

def instantiate_type_scheme[T0](scheme: hydra.core.TypeScheme) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    old_vars = scheme.variables
    return hydra.lib.flows.bind(fresh_names(hydra.lib.lists.length(old_vars)), (lambda new_vars: (subst := hydra.typing.TypeSubst(cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(hydra.lib.lists.zip(old_vars, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), new_vars))))), hydra.lib.flows.pure(hydra.core.TypeScheme(new_vars, hydra.substitution.subst_in_type(subst, scheme.type))))[1]))

def type_to_type_scheme(t0: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Convert a (System F -style) type to a type scheme."""
    
    def helper(vars: frozenlist[hydra.core.Name], t: hydra.core.Type) -> hydra.core.TypeScheme:
        match hydra.rewriting.deannotate_type(t):
            case hydra.core.TypeForall(value=ft):
                return helper(hydra.lib.lists.cons(ft.parameter, vars), ft.body)
            
            case _:
                return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), t)
    return helper(cast(frozenlist[hydra.core.Name], ()), t0)

def instantiate_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    return hydra.lib.flows.bind(instantiate_type_scheme(type_to_type_scheme(typ)), (lambda ts: hydra.lib.flows.pure(type_scheme_to_f_type(ts))))

def is_enum_row_type(rt: hydra.core.RowType) -> bool:
    r"""Check if a row type represents an enum (all fields are unit-typed)."""
    
    return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda f: hydra.encode.core.is_unit_type(hydra.rewriting.deannotate_type(f.type))), rt.fields))

def is_enum_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is an enum type."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeUnion(value=rt):
            return is_enum_row_type(rt)
        
        case _:
            return False

def type_dependencies(with_schema: bool, transform: Callable[[hydra.core.Type], hydra.core.Type], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get all type dependencies for a given type name."""
    
    def require_type(name2: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
        return hydra.monads.with_trace(hydra.lib.strings.cat2("type dependencies of ", name2.value), hydra.lib.flows.bind(hydra.lexical.require_element(name2), (lambda el: hydra.decode.core.type(el.term))))
    def to_pair(name2: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.flows.bind(require_type(name2), (lambda typ: hydra.lib.flows.pure((name2, transform(typ)))))
    def deps(seeds: frozenset[hydra.core.Name], names: FrozenDict[hydra.core.Name, hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.logic.if_else(hydra.lib.sets.null(seeds), hydra.lib.flows.pure(names), hydra.lib.flows.bind(hydra.lib.flows.map_list(to_pair, hydra.lib.sets.to_list(seeds)), (lambda pairs: (new_names := hydra.lib.maps.union(names, cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(pairs))), refs := hydra.lib.lists.foldl(cast(Callable[[frozenset[hydra.core.Name], frozenset[hydra.core.Name]], frozenset[hydra.core.Name]], hydra.lib.sets.union), cast(frozenset[hydra.core.Name], hydra.lib.sets.empty()), hydra.lib.lists.map((lambda tuple2: hydra.rewriting.type_dependency_names(with_schema, tuple2[1])), pairs)), visited := hydra.lib.sets.from_list(hydra.lib.maps.keys(names)), new_seeds := hydra.lib.sets.difference(refs, visited), deps(new_seeds, new_names))[4])))
    return hydra.monads.with_trace("type dependencies", deps(hydra.lib.sets.singleton(name), cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.empty())))

def is_serializable(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, bool]:
    r"""Check if an element is serializable (no function types in dependencies)."""
    
    def variants(typ: hydra.core.Type) -> frozenlist[hydra.meta.TypeVariant]:
        return hydra.lib.lists.map(hydra.variants.type_variant, hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), cast(frozenlist[hydra.core.Type], ()), typ))
    return hydra.lib.flows.map((lambda deps: (all_variants := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map(variants, hydra.lib.maps.elems(deps)))), hydra.lib.logic.not_(hydra.lib.sets.member(hydra.meta.TypeVariant.FUNCTION, all_variants)))[1]), type_dependencies(False, cast(Callable[[hydra.core.Type], hydra.core.Type], hydra.lib.equality.identity), el.name))

def module_dependency_namespaces(binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all elements of a module, excluding the module's own namespace."""
    
    return hydra.lib.flows.bind(dependency_namespaces(binds, with_prims, with_noms, with_schema, mod.elements), (lambda deps: hydra.lib.flows.pure(hydra.lib.sets.delete(mod.namespace, deps))))

def namespaces_for_definitions[T0](encode_namespace: Callable[[hydra.module.Namespace], T0], focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.module.Namespaces[T0]:
    nss = hydra.lib.sets.delete(focus_ns, definition_dependency_namespaces(defs))
    def to_pair(ns: hydra.module.Namespace) -> Tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return cast(hydra.module.Namespaces[T0], hydra.module.Namespaces(to_pair(focus_ns), cast(FrozenDict[hydra.module.Namespace, T0], hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, hydra.lib.sets.to_list(nss))))))

def nominal_application(tname: hydra.core.Name, args: frozenlist[hydra.core.Type]) -> hydra.core.Type:
    r"""Apply type arguments to a nominal type."""
    
    return hydra.lib.lists.foldl((lambda t, a: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t, a)))), cast(hydra.core.Type, hydra.core.TypeVariable(tname)), args)

def require_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Require a type by name."""
    
    return hydra.monads.with_trace(hydra.lib.strings.cat2("require type ", name.value), hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lexical.require_element(name)), (lambda el: hydra.decode.core.type(el.term))))

def require_row_type[T0](label: str, getter: Callable[[hydra.core.Type], Maybe[T0]], name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    def raw_type(t: hydra.core.Type) -> hydra.core.Type:
        match t:
            case hydra.core.TypeAnnotated(value=at):
                return raw_type(at.body)
            
            case hydra.core.TypeForall(value=ft):
                return raw_type(ft.body)
            
            case _:
                return t
    return hydra.lib.flows.bind(require_type(name), (lambda t: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat((name.value, " does not resolve to a ", label, " type: ", hydra.show.core.type(t)))), cast(Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, T0]], hydra.lib.flows.pure), getter(raw_type(t)))))

def require_record_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    r"""Require a name to resolve to a record type."""
    
    def to_record(t: hydra.core.Type) -> Maybe[hydra.core.RowType]:
        match t:
            case hydra.core.TypeRecord(value=rt):
                return cast(Maybe[hydra.core.RowType], Just(rt))
            
            case _:
                return cast(Maybe[hydra.core.RowType], Nothing())
    return require_row_type("record type", to_record, name)

def require_schema_type[T0](cx: hydra.typing.InferenceContext, tname: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    types = cx.schema_types
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("No such schema type: ", tname.value, ". Available types are: ", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(types)))))), (lambda ts: instantiate_type_scheme(hydra.rewriting.deannotate_type_scheme_recursive(ts))), hydra.lib.maps.lookup(tname, types))

def require_union_type(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    r"""Require a name to resolve to a union type."""
    
    def to_union(t: hydra.core.Type) -> Maybe[hydra.core.RowType]:
        match t:
            case hydra.core.TypeUnion(value=rt):
                return cast(Maybe[hydra.core.RowType], Just(rt))
            
            case _:
                return cast(Maybe[hydra.core.RowType], Nothing())
    return require_row_type("union", to_union, name)

def resolve_type(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    r"""Resolve a type, dereferencing type variables."""
    
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeVariable(value=name):
            return hydra.lexical.with_schema_context(hydra.lib.flows.bind(hydra.lexical.resolve_term(name), (lambda mterm: hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Type], Nothing())), (lambda t: hydra.lib.flows.map(cast(Callable[[hydra.core.Type], Maybe[hydra.core.Type]], hydra.lib.maybes.pure), hydra.decode.core.type(t))), mterm))))
        
        case _:
            return hydra.lib.flows.pure(cast(Maybe[hydra.core.Type], Just(typ)))

def term_as_graph(term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
    r"""Find the equivalent graph representation of a term."""
    
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            def from_binding(b: hydra.core.Binding) -> Tuple[hydra.core.Name, hydra.core.Binding]:
                name = b.name
                term = b.term
                ts = b.type
                return (name, hydra.core.Binding(name, term, ts))
            return cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.from_list(hydra.lib.lists.map(from_binding, bindings)))
        
        case _:
            return cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.empty())

def topological_sort_type_definitions(defs: frozenlist[hydra.module.TypeDefinition]) -> frozenlist[frozenlist[hydra.module.TypeDefinition]]:
    r"""Topologically sort type definitions by dependencies."""
    
    def to_pair(def_: hydra.module.TypeDefinition) -> Tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (def_.name, hydra.lib.sets.to_list(hydra.rewriting.type_dependency_names(False, def_.type)))
    name_to_def = cast(FrozenDict[hydra.core.Name, hydra.module.TypeDefinition], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda d: (d.name, d)), defs)))
    sorted = hydra.sorting.topological_sort_components(hydra.lib.lists.map(to_pair, defs))
    return hydra.lib.lists.map((lambda names: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, name_to_def)), names))), sorted)

def types_to_elements(type_map: FrozenDict[hydra.core.Name, hydra.core.Type]) -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
    r"""Encode a map of named types to a map of elements."""
    
    def to_element(tuple2: Tuple[hydra.core.Name, hydra.core.Type]) -> Tuple[hydra.core.Name, hydra.core.Binding]:
        name = tuple2[0]
        return (name, hydra.core.Binding(name, hydra.encode.core.type(tuple2[1]), cast(Maybe[hydra.core.TypeScheme], Nothing())))
    return cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.from_list(hydra.lib.lists.map(to_element, hydra.lib.maps.to_list(type_map))))
