# Note: this is an automatically generated file. Do not edit.

r"""Pure code generation pipeline for bootstrapping Hydra across languages."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt
import hydra.annotations
import hydra.coders
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.decode.packaging
import hydra.encode.core
import hydra.encode.packaging
import hydra.environment
import hydra.errors
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
import hydra.packaging
import hydra.show.core
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def build_schema_map(g: hydra.graph.Graph) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    r"""Build a schema map (Name -> Type) from a graph's schema types."""

    return hydra.lib.maps.map((lambda ts: hydra.strip.deannotate_type(ts.type)), g.schema_types)

def transitive_deps(get_deps: Callable[[hydra.packaging.Module], frozenlist[hydra.packaging.Namespace]], ns_map: FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module], start_mods: frozenlist[hydra.packaging.Module]) -> frozenset[hydra.packaging.Namespace]:
    r"""Compute transitive closure of module dependencies."""

    @lru_cache(1)
    def initial_deps() -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.lists.filter((lambda dep: hydra.lib.logic.not_(hydra.lib.equality.equal(dep, m.namespace))), get_deps(m))), start_mods)))
    def go(pending: frozenset[hydra.packaging.Namespace], visited: frozenset[hydra.packaging.Namespace]) -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.logic.if_else(hydra.lib.sets.null(pending), (lambda : visited), (lambda : (new_visited := hydra.lib.sets.union(visited, pending), (next_deps := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda nsv: hydra.lib.maybes.maybe((lambda : ()), (lambda dep_mod: get_deps(dep_mod)), hydra.lib.maps.lookup(nsv, ns_map))), hydra.lib.sets.to_list(pending)))), (new_pending := hydra.lib.sets.difference(next_deps, new_visited), go(new_pending, new_visited))[1])[1])[1]))
    return go(initial_deps(), hydra.lib.sets.empty())

def module_term_deps_transitive(ns_map: FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module], modules: frozenlist[hydra.packaging.Module]) -> frozenlist[hydra.packaging.Module]:
    r"""Compute transitive closure of term dependencies for a set of modules."""

    @lru_cache(1)
    def closure() -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.sets.union(transitive_deps((lambda m: m.term_dependencies), ns_map, modules), hydra.lib.sets.from_list(hydra.lib.lists.map((lambda m: m.namespace), modules)))
    return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, ns_map)), hydra.lib.sets.to_list(closure())))

def module_type_deps_transitive(ns_map: FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module], modules: frozenlist[hydra.packaging.Module]) -> frozenlist[hydra.packaging.Module]:
    r"""Compute transitive closure of type dependencies for a set of modules."""

    @lru_cache(1)
    def term_mods() -> frozenlist[hydra.packaging.Module]:
        return module_term_deps_transitive(ns_map, modules)
    @lru_cache(1)
    def type_namespaces() -> frozenlist[hydra.packaging.Namespace]:
        return hydra.lib.sets.to_list(transitive_deps((lambda m: m.type_dependencies), ns_map, term_mods()))
    return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, ns_map)), type_namespaces()))

def modules_to_graph(bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.packaging.Module], modules: frozenlist[hydra.packaging.Module]) -> hydra.graph.Graph:
    r"""Build a graph from universe modules and working modules, using an explicit bootstrap graph."""

    @lru_cache(1)
    def universe() -> FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, modules)))
    @lru_cache(1)
    def schema_modules() -> frozenlist[hydra.packaging.Module]:
        return module_type_deps_transitive(universe(), modules)
    @lru_cache(1)
    def data_modules() -> frozenlist[hydra.packaging.Module]:
        return module_term_deps_transitive(universe(), modules)
    @lru_cache(1)
    def schema_elements():
        def _hoist_schema_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_schema_elements_1(d)), m.definitions))), hydra.lib.lists.concat2(schema_modules(), modules)))
    @lru_cache(1)
    def data_elements():
        def _hoist_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_data_elements_1(d)), m.definitions))), data_modules()))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.environment.schema_graph_to_typing_environment(schema_graph()))
    @lru_cache(1)
    def base_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, schema_types(), data_elements())
    @lru_cache(1)
    def universe_data_elements():
        def _hoist_universe_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_universe_data_elements_1(d)), m.definitions))), universe_modules))
    @lru_cache(1)
    def universe_bound_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, ts)), b.type)), universe_data_elements())))
    return hydra.graph.Graph(base_graph().bound_terms, universe_bound_types(), base_graph().class_constraints, base_graph().lambda_variables, base_graph().metadata, base_graph().primitives, base_graph().schema_types, base_graph().type_variables)

def decode_module_from_json(bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.packaging.Module], json_val: hydra.json.model.Value) -> Either[hydra.errors.Error, hydra.packaging.Module]:
    r"""Decode a single module from a JSON value."""

    @lru_cache(1)
    def graph() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, universe_modules, universe_modules)
    @lru_cache(1)
    def schema_map() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return build_schema_map(graph())
    mod_type = cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.packaging.Module")))
    return hydra.lib.eithers.either((lambda err: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(err))))), (lambda term: hydra.lib.eithers.either((lambda dec_err: Left(cast(hydra.errors.Error, hydra.errors.ErrorDecoding(dec_err)))), (lambda mod: Right(mod)), hydra.decode.packaging.module(graph(), term))), hydra.json.decode.from_json(schema_map(), hydra.core.Name("hydra.packaging.Module"), mod_type, json_val))

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

def format_type_binding(graph: hydra.graph.Graph, binding: hydra.core.Binding) -> Either[hydra.errors.Error, str]:
    r"""Format a type binding for the lexicon."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(_e))), (lambda _a: _a), hydra.decode.core.type(graph, binding.term)), (lambda typ: Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ", binding.name.value), " = "), hydra.show.core.type(typ)))))

def generate_coder_modules(codec: Callable[[T0, hydra.graph.Graph, T1], Either[T2, Maybe[T3]]], bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.packaging.Module], type_modules: frozenlist[T1], cx: T0) -> Either[T2, frozenlist[T3]]:
    r"""Generate encoder or decoder modules for a list of type modules."""

    @lru_cache(1)
    def universe() -> FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, universe_modules)))
    @lru_cache(1)
    def schema_modules() -> frozenlist[hydra.packaging.Module]:
        return module_type_deps_transitive(universe(), universe_modules)
    @lru_cache(1)
    def data_modules() -> frozenlist[hydra.packaging.Module]:
        return module_term_deps_transitive(universe(), universe_modules)
    @lru_cache(1)
    def schema_elements():
        def _hoist_schema_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_schema_elements_1(d)), m.definitions))), hydra.lib.lists.concat2(schema_modules(), universe_modules)))
    @lru_cache(1)
    def data_elements():
        def _hoist_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_data_elements_1(d)), m.definitions))), data_modules()))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.environment.schema_graph_to_typing_environment(schema_graph()))
    @lru_cache(1)
    def all_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.concat2(schema_elements(), data_elements())
    @lru_cache(1)
    def graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, schema_types(), all_elements())
    return hydra.lib.eithers.map((lambda results: hydra.lib.maybes.cat(results)), hydra.lib.eithers.map_list((lambda m: codec(cx, graph(), m)), type_modules))

def generate_lexicon(graph: hydra.graph.Graph) -> Either[hydra.errors.Error, str]:
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
  hydra.packaging.Module,
  frozenlist[hydra.packaging.Definition],
  hydra.context.Context,
  hydra.graph.Graph], Either[hydra.errors.Error, FrozenDict[T0, T1]]], lang: hydra.coders.Language, do_infer: bool, do_expand: bool, do_hoist_case_statements: bool, do_hoist_polymorphic_let_bindings: bool, bs_graph: hydra.graph.Graph, universe_modules: frozenlist[hydra.packaging.Module], mods_to_generate: frozenlist[hydra.packaging.Module], cx: hydra.context.Context) -> Either[hydra.errors.Error, frozenlist[tuple[T0, T1]]]:
    r"""Pure core of code generation: given a coder, language, flags, bootstrap graph, universe, and modules to generate, produce a list of (filePath, content) pairs."""

    @lru_cache(1)
    def namespace_map() -> FrozenDict[hydra.packaging.Namespace, hydra.packaging.Module]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda m: (m.namespace, m)), hydra.lib.lists.concat2(universe_modules, mods_to_generate)))
    constraints = lang.constraints
    @lru_cache(1)
    def type_modules_to_generate():
        def _hoist_type_modules_to_generate_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

                case _:
                    return Nothing()
        return hydra.lib.lists.filter((lambda mod: hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_type_modules_to_generate_1(d)), mod.definitions))))), mods_to_generate)
    @lru_cache(1)
    def term_modules_to_generate():
        def _hoist_term_modules_to_generate_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.filter((lambda mod: hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_term_modules_to_generate_1(d)), mod.definitions))))), mods_to_generate)
    @lru_cache(1)
    def schema_mods() -> frozenlist[hydra.packaging.Module]:
        return module_type_deps_transitive(namespace_map(), mods_to_generate)
    @lru_cache(1)
    def schema_elements():
        def _hoist_schema_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_schema_elements_1(d)), m.definitions))), hydra.lib.lists.concat2(schema_mods(), type_modules_to_generate())))
    @lru_cache(1)
    def data_mods() -> frozenlist[hydra.packaging.Module]:
        return module_term_deps_transitive(namespace_map(), mods_to_generate)
    @lru_cache(1)
    def data_elements():
        def _hoist_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_data_elements_1(d)), m.definitions))), data_mods()))
    @lru_cache(1)
    def schema_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, hydra.lib.maps.empty(), schema_elements())
    @lru_cache(1)
    def schema_types2() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.eithers.either((lambda _: hydra.lib.maps.empty()), (lambda _r: _r), hydra.environment.schema_graph_to_typing_environment(schema_graph()))
    @lru_cache(1)
    def data_graph() -> hydra.graph.Graph:
        return hydra.lexical.elements_to_graph(bs_graph, schema_types2(), data_elements())
    return hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(type_modules_to_generate()), (lambda : Right(())), (lambda : (name_lists := (_hoist_name_lists_1 := (lambda v1: (lambda td: Just(td.name))(v1.value) if isinstance(v1, hydra.packaging.DefinitionType) else Nothing()), hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_name_lists_1(d)), m.definitions))), type_modules_to_generate()))[1], hydra.lib.eithers.bind(hydra.adapt.schema_graph_to_definitions(constraints, schema_graph(), name_lists, cx), (lambda schema_result: (def_lists := hydra.lib.pairs.second(schema_result), schema_graph_with_types := hydra.graph.Graph(schema_graph().bound_terms, schema_graph().bound_types, schema_graph().class_constraints, schema_graph().lambda_variables, schema_graph().metadata, schema_graph().primitives, schema_types2(), schema_graph().type_variables), hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda p: (mod := hydra.lib.pairs.first(p), defs := hydra.lib.pairs.second(p), hydra.lib.eithers.map((lambda m: hydra.lib.maps.to_list(m)), print_definitions(mod, hydra.lib.lists.map((lambda d: cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(d))), defs), cx, schema_graph_with_types)))[2]), hydra.lib.lists.zip(type_modules_to_generate(), def_lists))))[2])))[1])), (lambda schema_files: hydra.lib.eithers.bind(hydra.lib.logic.if_else(hydra.lib.lists.null(term_modules_to_generate()), (lambda : Right(())), (lambda : (namespaces := hydra.lib.lists.map((lambda m: m.namespace), term_modules_to_generate()), hydra.lib.eithers.bind(hydra.adapt.data_graph_to_definitions(constraints, do_infer, do_expand, do_hoist_case_statements, do_hoist_polymorphic_let_bindings, data_elements(), data_graph(), namespaces, cx), (lambda data_result: (g1 := hydra.lib.pairs.first(data_result), def_lists := hydra.lib.pairs.second(data_result), def_name := (lambda d: (_hoist_def_name_1 := (lambda v1: (lambda td: td.name)(v1.value) if isinstance(v1, hydra.packaging.DefinitionTerm) else (lambda td: td.name)(v1.value) if isinstance(v1, hydra.packaging.DefinitionType) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_def_name_1(d))[1]), refresh_module := (lambda els, m: (_hoist_refresh_module_1 := (lambda els, v1: (lambda td: Just(cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(td))))(v1.value) if isinstance(v1, hydra.packaging.DefinitionType) else (lambda td: hydra.lib.maybes.map((lambda b: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(b.name, b.term, b.type)))), hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, td.name)), els)))(v1.value) if isinstance(v1, hydra.packaging.DefinitionTerm) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), hydra.packaging.Module(m.namespace, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_refresh_module_1(els, d)), m.definitions)), m.term_dependencies, m.type_dependencies, m.description))[1]), all_bindings := hydra.lexical.graph_to_bindings(g1), refreshed_mods := hydra.lib.lists.map((lambda m: refresh_module(all_bindings, m)), term_modules_to_generate()), dedup_defs := (lambda defs: hydra.lib.maps.elems(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda d: (d.name, d)), defs)))), deduped_def_lists := hydra.lib.lists.map((lambda x1: dedup_defs(x1)), def_lists), hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda p: (mod := hydra.lib.pairs.first(p), defs := hydra.lib.pairs.second(p), hydra.lib.eithers.map((lambda m: hydra.lib.maps.to_list(m)), print_definitions(mod, hydra.lib.lists.map((lambda d: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(d))), defs), cx, g1)))[2]), hydra.lib.lists.zip(refreshed_mods, deduped_def_lists))))[8])))[1])), (lambda term_files: Right(hydra.lib.lists.concat2(schema_files, term_files))))))

def infer_and_generate_lexicon(cx: hydra.context.Context, bs_graph: hydra.graph.Graph, kernel_modules: frozenlist[hydra.packaging.Module]) -> Either[hydra.errors.Error, str]:
    r"""Perform type inference and generate the lexicon for a set of modules."""

    @lru_cache(1)
    def g0() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, kernel_modules, kernel_modules)
    @lru_cache(1)
    def data_elements():
        def _hoist_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_data_elements_1(d)), m.definitions))), kernel_modules))
    return hydra.lib.eithers.bind(hydra.inference.infer_graph_types(cx, data_elements(), g0()), (lambda infer_result_with_cx: (g1 := hydra.lib.pairs.first(hydra.lib.pairs.first(infer_result_with_cx)), generate_lexicon(g1))[1]))

def refresh_module(inferred_elements: frozenlist[hydra.core.Binding], m: hydra.packaging.Module):
    def _hoist_hydra_codegen_refresh_module_1(v1):
        match v1:
            case hydra.packaging.DefinitionTerm(value=td):
                return Just(hydra.core.Binding(td.name, td.term, td.type))

            case _:
                return Nothing()
    def _hoist_hydra_codegen_refresh_module_2(inferred_elements, v1):
        match v1:
            case hydra.packaging.DefinitionType(value=td):
                return Just(cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(td)))

            case hydra.packaging.DefinitionTerm(value=td):
                return hydra.lib.maybes.map((lambda b: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(b.name, b.term, b.type)))), hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, td.name)), inferred_elements))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_hydra_codegen_refresh_module_1(d)), m.definitions))))), (lambda : m), (lambda : hydra.packaging.Module(m.namespace, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_hydra_codegen_refresh_module_2(inferred_elements, d)), m.definitions)), m.term_dependencies, m.type_dependencies, m.description)))

def infer_modules(cx: hydra.context.Context, bs_graph: hydra.graph.Graph, universe_mods: frozenlist[hydra.packaging.Module], target_mods: frozenlist[hydra.packaging.Module]) -> Either[hydra.errors.Error, frozenlist[hydra.packaging.Module]]:
    r"""Perform type inference on modules and reconstruct with inferred types."""

    @lru_cache(1)
    def g0() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, universe_mods, universe_mods)
    @lru_cache(1)
    def data_elements():
        def _hoist_data_elements_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_data_elements_1(d)), m.definitions))), universe_mods))
    return hydra.lib.eithers.bind(hydra.inference.infer_graph_types(cx, data_elements(), g0()), (lambda infer_result_with_cx: (infer_result := hydra.lib.pairs.first(infer_result_with_cx), inferred_elements := hydra.lib.pairs.second(infer_result), Right(hydra.lib.lists.map((lambda v1: refresh_module(inferred_elements, v1)), target_mods)))[2]))

def infer_modules_given(cx: hydra.context.Context, bs_graph: hydra.graph.Graph, universe_mods: frozenlist[hydra.packaging.Module], target_mods: frozenlist[hydra.packaging.Module]) -> Either[hydra.errors.Error, frozenlist[hydra.packaging.Module]]:
    r"""Incrementally infer types for target modules, using the universe as a seeded inference context."""

    @lru_cache(1)
    def g0() -> hydra.graph.Graph:
        return modules_to_graph(bs_graph, universe_mods, universe_mods)
    @lru_cache(1)
    def target_bindings():
        def _hoist_target_bindings_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda m: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_target_bindings_1(d)), m.definitions))), target_mods))
    return hydra.lib.eithers.bind(hydra.inference.infer_graph_types(cx, target_bindings(), g0()), (lambda infer_result_with_cx: (infer_result := hydra.lib.pairs.first(infer_result_with_cx), inferred_elements := hydra.lib.pairs.second(infer_result), Right(hydra.lib.lists.map((lambda v1: refresh_module(inferred_elements, v1)), target_mods)))[2]))

def module_to_json(schema_map: FrozenDict[hydra.core.Name, hydra.core.Type], m: hydra.packaging.Module) -> Either[hydra.errors.Error, str]:
    r"""Convert a Module to a JSON string."""

    @lru_cache(1)
    def term() -> hydra.core.Term:
        return hydra.encode.packaging.module(m)
    mod_type = cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.packaging.Module")))
    return hydra.lib.eithers.map((lambda json: hydra.json.writer.print_json(json)), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e)))), (lambda _a: _a), hydra.json.encode.to_json(schema_map, hydra.core.Name("hydra.packaging.Module"), mod_type, term())))

def module_to_source_module(m: hydra.packaging.Module) -> hydra.packaging.Module:
    r"""Convert a generated Module into a Source module."""

    @lru_cache(1)
    def source_ns() -> hydra.packaging.Namespace:
        return hydra.packaging.Namespace(hydra.lib.strings.cat2("hydra.sources.", hydra.lib.strings.intercalate(".", hydra.lib.lists.drop(1, hydra.lib.strings.split_on(".", m.namespace.value)))))
    mod_type_ns = hydra.packaging.Namespace("hydra.packaging")
    @lru_cache(1)
    def module_def() -> hydra.packaging.Definition:
        return cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(hydra.core.Name(hydra.lib.strings.cat2(source_ns().value, ".module_")), hydra.encode.packaging.module(m), Nothing())))
    return hydra.packaging.Module(source_ns(), (module_def(),), (mod_type_ns,), (mod_type_ns,), Just(hydra.lib.strings.cat2("Source module for ", m.namespace.value)))

def namespace_to_path(ns: hydra.packaging.Namespace) -> str:
    r"""Convert a namespace to a file path (e.g., hydra.core -> hydra/core)."""

    return hydra.lib.strings.intercalate("/", hydra.lib.strings.split_on(".", ns.value))
