# Note: this is an automatically generated file. Do not edit.

r"""Pure code generation pipeline for bootstrapping Hydra across languages."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.simple
import hydra.annotations
import hydra.coders
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.decode.module
import hydra.encode.module
import hydra.error
import hydra.graph
import hydra.inference
import hydra.json.decode
import hydra.json.encode
import hydra.json.writer
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.rewriting
import hydra.schemas
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def build_schema_map(g: hydra.graph.Graph) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    r"""Build a schema map (Name -> Type) from a graph's schema types."""
    
    return hydra.lib.maps.map((lambda ts: hydra.rewriting.deannotate_type(ts.type)), g.schema_types)

def transitive_deps(get_deps: Callable[[hydra.module.Module], frozenlist[hydra.module.Namespace]], ns_map: FrozenDict[hydra.module.Namespace, hydra.module.Module], start_mods: frozenlist[hydra.module.Module]) -> frozenset[hydra.module.Namespace]:
    r"""Compute transitive closure of module dependencies."""
    
    @lru_cache(1)
    def initial_deps() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.lists.filter((lambda dep: hydra.lib.logic.not_(hydra.lib.equality.equal(dep, m.namespace))), get_deps(m))), start_mods)))
    def go(pending: frozenset[hydra.module.Namespace], visited: frozenset[hydra.module.Namespace]) -> frozenset[hydra.module.Namespace]:
        return hydra.lib.logic.if_else(hydra.lib.sets.null(pending), (lambda : visited), (lambda : (new_visited := hydra.lib.sets.union(visited, pending), (next_deps := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda nsv: hydra.lib.maybes.maybe((lambda : ()), (lambda dep_mod: get_deps(dep_mod)), hydra.lib.maps.lookup(nsv, ns_map))), hydra.lib.sets.to_list(pending)))), (new_pending := hydra.lib.sets.difference(next_deps, new_visited), go(new_pending, new_visited))[1])[1])[1]))
    return go(initial_deps(), hydra.lib.sets.empty())

def module_term_deps_transitive(ns_map: FrozenDict[hydra.module.Namespace, hydra.module.Module], modules: frozenlist[hydra.module.Module]) -> frozenlist[hydra.module.Module]:
    r"""Compute transitive closure of term dependencies for a set of modules."""
    
    @lru_cache(1)
    def closure() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.union(transitive_deps((lambda m: m.term_dependencies), ns_map, modules), hydra.lib.sets.from_list(hydra.lib.lists.map((lambda m: m.namespace), modules)))
    return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, ns_map)), hydra.lib.sets.to_list(closure())))

def module_type_deps_transitive(ns_map: FrozenDict[hydra.module.Namespace, hydra.module.Module], modules: frozenlist[hydra.module.Module]) -> frozenlist[hydra.module.Module]:
    r"""Compute transitive closure of type dependencies for a set of modules."""
    
    @lru_cache(1)
    def term_mods() -> frozenlist[hydra.module.Module]:
        return module_term_deps_transitive(ns_map, modules)
    @lru_cache(1)
    def type_namespaces() -> frozenlist[hydra.module.Namespace]:
        return hydra.lib.sets.to_list(transitive_deps((lambda m: m.type_dependencies), ns_map, term_mods()))
    return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, ns_map)), type_namespaces()))

def modules_to_graph(bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.module.Module], modules: frozenlist[hydra.module.Module]) -> hydra.graph.Graph:
    r"""Build a graph from universe modules and working modules, using an explicit bootstrap graph."""
    
    @lru_cache(1)
    def universe() -> FrozenDict[hydra.module.Namespace, hydra.module.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, modules)))
    @lru_cache(1)
    def schema_modules() -> frozenlist[hydra.module.Module]:
        return module_type_deps_transitive(universe(), modules)
    @lru_cache(1)
    def data_modules() -> frozenlist[hydra.module.Module]:
        return module_term_deps_transitive(universe(), modules)
    @lru_cache(1)
    def schema_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.annotations.is_native_type(e)), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), hydra.lib.lists.concat2(schema_modules(), modules))))
    @lru_cache(1)
    def data_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.lib.logic.not_(hydra.annotations.is_native_type(e))), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), data_modules())))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.schemas.schema_graph_to_typing_environment(hydra.context.Context((), (), hydra.lib.maps.empty()), schema_graph()))
    return hydra.lexical.elements_to_graph(bs_graph, schema_types(), data_elements())

def strip_module_type_schemes(m: hydra.module.Module) -> hydra.module.Module:
    r"""Strip TypeSchemes from term bindings in a module, preserving type binding TypeSchemes. JSON-loaded modules carry inferred TypeSchemes from the original compilation. After adaptation (e.g., bigfloat -> float64), these TypeSchemes become stale and can cause inference errors. Stripping them allows the inference engine to reconstruct correct TypeSchemes from scratch."""
    
    def strip_if_term(b: hydra.core.Binding) -> hydra.core.Binding:
        return hydra.lib.logic.if_else(hydra.annotations.is_native_type(b), (lambda : b), (lambda : hydra.core.Binding(b.name, b.term, Nothing())))
    return hydra.module.Module(m.namespace, hydra.lib.lists.map((lambda x1: strip_if_term(x1)), m.elements), m.term_dependencies, m.type_dependencies, m.description)

def decode_module_from_json(bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.module.Module], do_strip_type_schemes: bool, json_val: hydra.json.model.Value) -> Either[str, hydra.module.Module]:
    r"""Decode a single module from a JSON value."""
    
    @lru_cache(1)
    def graph() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, universe_modules, universe_modules)
    @lru_cache(1)
    def schema_map() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return build_schema_map(graph())
    mod_type = cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.module.Module")))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda term: hydra.lib.eithers.either((lambda dec_err: Left(dec_err.value)), (lambda mod: Right(hydra.lib.logic.if_else(do_strip_type_schemes, (lambda : strip_module_type_schemes(mod)), (lambda : mod)))), hydra.decode.module.module(graph(), term))), hydra.json.decode.from_json(schema_map(), mod_type, json_val))

def escape_control_chars_in_json(input: frozenlist[int]) -> frozenlist[int]:
    r"""Escape unescaped control characters inside JSON string literals."""
    
    def hex_digit(n: int) -> int:
        return hydra.lib.logic.if_else(hydra.lib.equality.lt(n, 10), (lambda : hydra.lib.math.add(48, n)), (lambda : hydra.lib.math.add(97, hydra.lib.math.sub(n, 10))))
    def escape_to_unicode(b: int) -> frozenlist[int]:
        return (92, 117, 48, 48, hex_digit(hydra.lib.math.div(b, 16)), hex_digit(hydra.lib.math.mod(b, 16)))
    def go(in_str: bool, esc: bool, bytes: frozenlist[int]) -> frozenlist[int]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(bytes), (lambda : ()), (lambda : (b := hydra.lib.lists.head(bytes), (bs := hydra.lib.lists.tail(bytes), hydra.lib.logic.if_else(esc, (lambda : hydra.lib.lists.cons(b, go(in_str, False, bs))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(b, 92), in_str), (lambda : hydra.lib.lists.cons(b, go(in_str, True, bs))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(b, 34), (lambda : hydra.lib.lists.cons(b, go(hydra.lib.logic.not_(in_str), False, bs))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(in_str, hydra.lib.equality.lt(b, 32)), (lambda : hydra.lib.lists.concat2(escape_to_unicode(b), go(in_str, False, bs))), (lambda : hydra.lib.lists.cons(b, go(in_str, False, bs)))))))))))[1])[1]))
    return go(False, False, input)

def format_primitive(prim: hydra.graph.Primitive) -> str:
    r"""Format a primitive for the lexicon."""
    
    name = prim.name.value
    @lru_cache(1)
    def type_str() -> str:
        return hydra.show.core.type_scheme(prim.type)
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ", name), " : "), type_str())

def format_term_binding(binding: hydra.core.Binding) -> str:
    r"""Format a term binding for the lexicon."""
    
    name = binding.name.value
    @lru_cache(1)
    def type_str() -> str:
        return hydra.lib.maybes.maybe((lambda : "?"), (lambda scheme: hydra.show.core.type_scheme(scheme)), binding.type)
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ", name), " : "), type_str())

def format_type_binding(graph: hydra.graph.Graph, binding: hydra.core.Binding) -> Either[hydra.error.DecodingError, str]:
    r"""Format a type binding for the lexicon."""
    
    return hydra.lib.eithers.bind(hydra.decode.core.type(graph, binding.term), (lambda typ: Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ", binding.name.value), " = "), hydra.show.core.type(typ)))))

def generate_coder_modules(codec: Callable[[T0, hydra.graph.Graph, T1], Either[T2, Maybe[T3]]], bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.module.Module], type_modules: frozenlist[T1], cx: T0) -> Either[T2, frozenlist[T3]]:
    r"""Generate encoder or decoder modules for a list of type modules."""
    
    @lru_cache(1)
    def universe() -> FrozenDict[hydra.module.Namespace, hydra.module.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, universe_modules)))
    @lru_cache(1)
    def schema_modules() -> frozenlist[hydra.module.Module]:
        return module_type_deps_transitive(universe(), universe_modules)
    @lru_cache(1)
    def data_modules() -> frozenlist[hydra.module.Module]:
        return module_term_deps_transitive(universe(), universe_modules)
    @lru_cache(1)
    def schema_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.annotations.is_native_type(e)), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), hydra.lib.lists.concat2(schema_modules(), universe_modules))))
    @lru_cache(1)
    def data_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.lib.logic.not_(hydra.annotations.is_native_type(e))), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), data_modules())))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.schemas.schema_graph_to_typing_environment(hydra.context.Context((), (), hydra.lib.maps.empty()), schema_graph()))
    @lru_cache(1)
    def all_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.concat2(schema_elements(), data_elements())
    @lru_cache(1)
    def graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, schema_types(), all_elements())
    return hydra.lib.eithers.map((lambda results: hydra.lib.maybes.cat(results)), hydra.lib.eithers.map_list((lambda m: codec(cx, graph(), m)), type_modules))

def generate_lexicon(graph: hydra.graph.Graph) -> Either[hydra.error.DecodingError, str]:
    r"""Generate the lexicon content from a graph."""
    
    @lru_cache(1)
    def bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lexical.graph_to_bindings(graph)
    @lru_cache(1)
    def primitives() -> frozenlist[hydra.graph.Primitive]:
        return hydra.lib.maps.elems(graph.primitives)
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.core.Binding], frozenlist[hydra.core.Binding]]:
        return hydra.lib.lists.partition((lambda b: hydra.annotations.is_native_type(b)), bindings())
    @lru_cache(1)
    def type_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.pairs.second(partitioned())
    @lru_cache(1)
    def sorted_primitives() -> frozenlist[hydra.graph.Primitive]:
        return hydra.lib.lists.sort_on((lambda p: p.name), primitives())
    @lru_cache(1)
    def sorted_types() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.sort_on((lambda b: b.name), type_bindings())
    @lru_cache(1)
    def sorted_terms() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.sort_on((lambda b: b.name), term_bindings())
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda b: format_type_binding(graph, b)), sorted_types()), (lambda type_lines: (term_lines := hydra.lib.lists.map((lambda b: format_term_binding(b)), sorted_terms()), primitive_lines := hydra.lib.lists.map((lambda p: format_primitive(p)), sorted_primitives()), Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Primitives:\n", hydra.lib.strings.unlines(primitive_lines)), "\nTypes:\n"), hydra.lib.strings.unlines(type_lines)), "\nTerms:\n"), hydra.lib.strings.unlines(term_lines))))[2]))

def generate_source_files(print_definitions: Callable[[
  hydra.module.Module,
  frozenlist[hydra.module.Definition],
  hydra.context.Context,
  hydra.graph.Graph], Either[hydra.context.InContext[hydra.error.OtherError], FrozenDict[T0, T1]]], lang: hydra.coders.Language, do_infer: bool, do_expand: bool, do_hoist_case_statements: bool, do_hoist_polymorphic_let_bindings: bool, bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.module.Module], mods_to_generate: frozenlist[hydra.module.Module], cx: hydra.context.Context) -> Either[hydra.context.InContext[hydra.error.OtherError], frozenlist[tuple[T0, T1]]]:
    r"""Pure core of code generation: given a coder, language, flags, bootstrap graph, universe, and modules to generate, produce a list of (filePath, content) pairs."""
    
    @lru_cache(1)
    def namespace_map() -> FrozenDict[hydra.module.Namespace, hydra.module.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, mods_to_generate)))
    constraints = lang.constraints
    def is_type_module(mod: hydra.module.Module) -> bool:
        return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda e: hydra.annotations.is_native_type(e)), mod.elements)))
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.module.Module], frozenlist[hydra.module.Module]]:
        return hydra.lib.lists.partition((lambda x1: is_type_module(x1)), mods_to_generate)
    @lru_cache(1)
    def type_modules_to_generate() -> frozenlist[hydra.module.Module]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def term_modules_to_generate() -> frozenlist[hydra.module.Module]:
        return hydra.lib.pairs.second(partitioned())
    @lru_cache(1)
    def schema_mods() -> frozenlist[hydra.module.Module]:
        return module_type_deps_transitive(namespace_map(), mods_to_generate)
    @lru_cache(1)
    def schema_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.annotations.is_native_type(e)), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), hydra.lib.lists.concat2(schema_mods(), type_modules_to_generate()))))
    @lru_cache(1)
    def data_mods() -> frozenlist[hydra.module.Module]:
        return module_term_deps_transitive(namespace_map(), mods_to_generate)
    @lru_cache(1)
    def data_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), data_mods()))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types2() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.schemas.schema_graph_to_typing_environment(hydra.context.Context((), (), hydra.lib.maps.empty()), schema_graph()))
    @lru_cache(1)
    def data_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, schema_types2(), data_elements())
    return hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(type_modules_to_generate()), (lambda : Right(())), (lambda : (name_lists := hydra.lib.lists.map((lambda m: hydra.lib.lists.map((lambda e: e.name), hydra.lib.lists.filter((lambda e: hydra.annotations.is_native_type(e)), m.elements))), type_modules_to_generate()), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda s: hydra.context.InContext(hydra.error.OtherError(s), cx)), (lambda r: r), hydra.adapt.simple.schema_graph_to_definitions(constraints, schema_graph(), name_lists, cx)), (lambda schema_result: (def_lists := hydra.lib.pairs.second(schema_result), schema_graph_with_types := hydra.graph.Graph(schema_graph().bound_terms, schema_graph().bound_types, schema_graph().class_constraints, schema_graph().lambda_variables, schema_graph().metadata, schema_graph().primitives, schema_types2(), schema_graph().type_variables), hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda p: (mod := hydra.lib.pairs.first(p), defs := hydra.lib.pairs.second(p), hydra.lib.eithers.map((lambda m: hydra.lib.maps.to_list(m)), print_definitions(mod, hydra.lib.lists.map((lambda d: cast(hydra.module.Definition, hydra.module.DefinitionType(d))), defs), cx, schema_graph_with_types)))[2]), hydra.lib.lists.zip(type_modules_to_generate(), def_lists))))[2])))[1])), (lambda schema_files: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(term_modules_to_generate()), (lambda : Right(())), (lambda : (namespaces := hydra.lib.lists.map((lambda m: m.namespace), term_modules_to_generate()), hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda s: hydra.context.InContext(hydra.error.OtherError(s), cx)), (lambda r: r), hydra.adapt.simple.data_graph_to_definitions(constraints, do_infer, do_expand, do_hoist_case_statements, do_hoist_polymorphic_let_bindings, data_elements(), data_graph(), namespaces, cx)), (lambda data_result: (g1 := hydra.lib.pairs.first(data_result), def_lists := hydra.lib.pairs.second(data_result), refresh_module := (lambda els, m: hydra.module.Module(m.namespace, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda e: hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, e.name)), els)), m.elements)), m.term_dependencies, m.type_dependencies, m.description)), refreshed_mods := hydra.lib.lists.map((lambda m: refresh_module(hydra.lexical.graph_to_bindings(g1), m)), term_modules_to_generate()), hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda p: (mod := hydra.lib.pairs.first(p), defs := hydra.lib.pairs.second(p), hydra.lib.eithers.map((lambda m: hydra.lib.maps.to_list(m)), print_definitions(mod, hydra.lib.lists.map((lambda d: cast(hydra.module.Definition, hydra.module.DefinitionTerm(d))), defs), cx, g1)))[2]), hydra.lib.lists.zip(refreshed_mods, def_lists))))[4])))[1])), (lambda term_files: Right(hydra.lib.lists.concat2(schema_files, term_files))))))

def infer_and_generate_lexicon(cx: hydra.context.Context, bs_graph: hydra.graph.Graph, kernel_modules: frozenlist[hydra.module.Module]) -> Either[str, str]:
    r"""Perform type inference and generate the lexicon for a set of modules."""
    
    @lru_cache(1)
    def g0() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, kernel_modules, kernel_modules)
    @lru_cache(1)
    def data_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.lib.logic.not_(hydra.annotations.is_native_type(e))), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), kernel_modules)))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: ic.object.value), (lambda x: x), hydra.inference.infer_graph_types(cx, data_elements(), g0())), (lambda infer_result_with_cx: (g1 := hydra.lib.pairs.first(hydra.lib.pairs.first(infer_result_with_cx)), hydra.lib.eithers.bimap((lambda v1: v1.value), (lambda x: x), generate_lexicon(g1)))[1]))

def infer_modules(cx: hydra.context.Context, bs_graph: hydra.graph.Graph, universe_mods: frozenlist[hydra.module.Module], target_mods: frozenlist[hydra.module.Module]) -> Either[hydra.context.InContext[hydra.error.OtherError], frozenlist[hydra.module.Module]]:
    r"""Perform type inference on modules and reconstruct with inferred types."""
    
    @lru_cache(1)
    def g0() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, universe_mods, universe_mods)
    @lru_cache(1)
    def data_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda e: hydra.lib.logic.not_(hydra.annotations.is_native_type(e))), hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: m.elements), universe_mods)))
    return hydra.lib.eithers.bind(hydra.inference.infer_graph_types(cx, data_elements(), g0()), (lambda infer_result_with_cx: (infer_result := hydra.lib.pairs.first(infer_result_with_cx), g1 := hydra.lib.pairs.first(infer_result), inferred_elements := hydra.lib.pairs.second(infer_result), is_type_module := (lambda mod: hydra.lib.lists.null(hydra.lib.lists.filter((lambda e: hydra.lib.logic.not_(hydra.annotations.is_native_type(e))), mod.elements))), refresh_module := (lambda m: hydra.lib.logic.if_else(is_type_module(m), (lambda : m), (lambda : hydra.module.Module(m.namespace, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda e: hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, e.name)), inferred_elements)), m.elements)), m.term_dependencies, m.type_dependencies, m.description)))), Right(hydra.lib.lists.map((lambda x1: refresh_module(x1)), target_mods)))[5]))

def module_to_json(m: hydra.module.Module) -> Either[str, str]:
    r"""Convert a Module to a JSON string."""
    
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return hydra.encode.module.module(m)
    return hydra.lib.eithers.map((lambda json: hydra.json.writer.print_json(json)), hydra.json.encode.to_json(term()))

def module_to_source_module(m: hydra.module.Module) -> hydra.module.Module:
    r"""Convert a generated Module into a Source module."""
    
    @lru_cache(1)
    def source_ns() -> hydra.module.Namespace:
        return hydra.module.Namespace(hydra.lib.strings.cat2("hydra.sources.", hydra.lib.strings.intercalate(".", hydra.lib.lists.drop(1, hydra.lib.strings.split_on(".", m.namespace.value)))))
    mod_type_ns = hydra.module.Namespace("hydra.module")
    @lru_cache(1)
    def module_binding() -> hydra.core.Binding:
        return hydra.core.Binding(hydra.core.Name(hydra.lib.strings.cat2(source_ns().value, ".module_")), hydra.encode.module.module(m), Nothing())
    return hydra.module.Module(source_ns(), (module_binding(),), (mod_type_ns,), (mod_type_ns,), Just(hydra.lib.strings.cat2("Source module for ", m.namespace.value)))

def namespace_to_path(ns: hydra.module.Namespace) -> str:
    r"""Convert a namespace to a file path (e.g., hydra.core -> hydra/core)."""
    
    return hydra.lib.strings.intercalate("/", hydra.lib.strings.split_on(".", ns.value))
