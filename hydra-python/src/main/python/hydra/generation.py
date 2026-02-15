"""I/O wrapper for Hydra code generation in Python.

Provides file I/O around the pure/Flow-based functions in hydra.code_generation.
This is the Python equivalent of Haskell's Hydra.Generation module.
"""

import json
import os
import sys
from functools import lru_cache

# The generated JSON decoder uses recursive variant matching which can
# exceed Python's default recursion limit for deeply nested types.
sys.setrecursionlimit(10000)

from hydra.annotations import is_native_type
from hydra.code_generation import (
    build_schema_map,
    decode_module_from_json,
    generate_source_files,
    modules_to_graph,
    namespace_to_path,
    strip_module_type_schemes,
)
from hydra.compute import Trace
from hydra.core import Binding, LiteralString, TermLiteral
from hydra.dsl.python import FrozenDict, Just, Left, Nothing, Right
from hydra.graph import Graph
from hydra.json import model as JsonModel
from hydra.module import Module, Namespace
from hydra.rewriting import remove_types_from_term
from hydra.sources.libraries import standard_library


@lru_cache(1)
def kernel_modules():
    """Load all kernel Source modules (the Python equivalent of Haskell's kernelModules).

    These provide the type universe needed for decoding modules from JSON.
    Mirrors: kernelTypesModules ++ kernelTermsModules ++ jsonModules
    """
    # Kernel types modules (22)
    from hydra.sources import accessors as src_accessors
    from hydra.sources import ast as src_ast
    from hydra.sources import classes as src_classes
    from hydra.sources import coders as src_coders
    from hydra.sources import compute as src_compute
    from hydra.sources import constraints as src_constraints
    from hydra.sources import core as src_core
    from hydra.sources import grammar as src_grammar
    from hydra.sources import graph as src_graph
    from hydra.sources.json import model as src_json_model
    from hydra.sources import module as src_module
    from hydra.sources import parsing as src_parsing
    from hydra.sources import phantoms as src_phantoms
    from hydra.sources import query as src_query
    from hydra.sources import relational as src_relational
    from hydra.sources import tabular as src_tabular
    from hydra.sources import testing as src_testing
    from hydra.sources import topology as src_topology
    from hydra.sources import typing as src_typing
    from hydra.sources import util as src_util
    from hydra.sources import variants as src_variants
    from hydra.sources import workflow as src_workflow

    # Kernel primary terms modules (41 in Haskell, minus codeGeneration which has no Source module in Python)
    from hydra.sources.adapt import literals as src_adapt_literals
    from hydra.sources.adapt import modules as src_adapt_modules
    from hydra.sources.adapt import simple as src_adapt_simple
    from hydra.sources.adapt import terms as src_adapt_terms
    from hydra.sources.adapt import utils as src_adapt_utils
    from hydra.sources import annotations as src_annotations
    from hydra.sources import arity as src_arity
    from hydra.sources import checking as src_checking
    from hydra.sources import constants as src_constants
    # Note: hydra.sources.decoding is skipped (SyntaxError: too many nested parentheses)
    from hydra.sources import encoding as src_encoding
    from hydra.sources.extract import core as src_extract_core
    from hydra.sources.extract import helpers as src_extract_helpers
    from hydra.sources.extract import util as src_extract_util
    from hydra.sources import formatting as src_formatting
    from hydra.sources import grammars as src_grammars
    # Note: hydra.sources.hoisting is skipped (SyntaxError: too many nested parentheses)
    from hydra.sources import inference as src_inference
    from hydra.sources import languages as src_languages
    from hydra.sources import lexical as src_lexical
    from hydra.sources import literals as src_literals
    from hydra.sources import monads as src_monads
    from hydra.sources import names as src_names
    from hydra.sources import parsers as src_parsers
    from hydra.sources import reduction as src_reduction
    from hydra.sources import reflect as src_reflect
    from hydra.sources import rewriting as src_rewriting
    from hydra.sources import schemas as src_schemas
    from hydra.sources import serialization as src_serialization
    from hydra.sources.show import accessors as src_show_accessors
    from hydra.sources.show import core as src_show_core
    from hydra.sources.show import graph as src_show_graph
    from hydra.sources.show import meta as src_show_meta
    from hydra.sources.show import typing as src_show_typing
    from hydra.sources.show import util as src_show_util
    from hydra.sources import sorting as src_sorting
    from hydra.sources import substitution as src_substitution
    from hydra.sources import tarjan as src_tarjan
    from hydra.sources import templates as src_templates
    from hydra.sources import unification as src_unification

    # Kernel decoding modules (21)
    from hydra.sources.decode import accessors as src_dec_accessors
    from hydra.sources.decode import ast as src_dec_ast
    from hydra.sources.decode import classes as src_dec_classes
    from hydra.sources.decode import coders as src_dec_coders
    from hydra.sources.decode import compute as src_dec_compute
    from hydra.sources.decode import constraints as src_dec_constraints
    from hydra.sources.decode import core as src_dec_core
    from hydra.sources.decode import grammar as src_dec_grammar
    from hydra.sources.decode.json import model as src_dec_json
    from hydra.sources.decode import module as src_dec_module
    from hydra.sources.decode import parsing as src_dec_parsing
    from hydra.sources.decode import phantoms as src_dec_phantoms
    from hydra.sources.decode import query as src_dec_query
    from hydra.sources.decode import relational as src_dec_relational
    from hydra.sources.decode import tabular as src_dec_tabular
    from hydra.sources.decode import testing as src_dec_testing
    from hydra.sources.decode import topology as src_dec_topology
    from hydra.sources.decode import typing as src_dec_typing
    from hydra.sources.decode import util as src_dec_util
    from hydra.sources.decode import variants as src_dec_variants
    from hydra.sources.decode import workflow as src_dec_workflow

    # Kernel encoding modules (21)
    from hydra.sources.encode import accessors as src_enc_accessors
    from hydra.sources.encode import ast as src_enc_ast
    from hydra.sources.encode import classes as src_enc_classes
    from hydra.sources.encode import coders as src_enc_coders
    from hydra.sources.encode import compute as src_enc_compute
    from hydra.sources.encode import constraints as src_enc_constraints
    from hydra.sources.encode import core as src_enc_core
    from hydra.sources.encode import grammar as src_enc_grammar
    from hydra.sources.encode.json import model as src_enc_json
    from hydra.sources.encode import module as src_enc_module
    from hydra.sources.encode import parsing as src_enc_parsing
    from hydra.sources.encode import phantoms as src_enc_phantoms
    from hydra.sources.encode import query as src_enc_query
    from hydra.sources.encode import relational as src_enc_relational
    from hydra.sources.encode import tabular as src_enc_tabular
    from hydra.sources.encode import testing as src_enc_testing
    from hydra.sources.encode import topology as src_enc_topology
    from hydra.sources.encode import typing as src_enc_typing
    from hydra.sources.encode import util as src_enc_util
    from hydra.sources.encode import variants as src_enc_variants
    from hydra.sources.encode import workflow as src_enc_workflow

    # JSON modules (8 in Haskell; in Python: 5 in sources/json + 3 in sources/ext/org/json)
    from hydra.sources.json import decode as src_json_decode
    from hydra.sources.json import encode as src_json_encode
    from hydra.sources.json import parser as src_json_parser
    from hydra.sources.json import writer as src_json_writer
    from hydra.sources.extract import json as src_extract_json
    from hydra.sources.ext.org.json import coder as src_org_json_coder
    from hydra.sources.ext.org.json import decoding as src_org_json_decoding
    from hydra.sources.ext.org.json import language as src_org_json_language

    modules = [
        # Types (22)
        src_accessors.module(), src_ast.module(), src_classes.module(),
        src_coders.module(), src_compute.module(), src_constraints.module(),
        src_core.module(), src_grammar.module(), src_graph.module(),
        src_json_model.module(), src_module.module(), src_parsing.module(),
        src_phantoms.module(), src_query.module(), src_relational.module(),
        src_tabular.module(), src_testing.module(), src_topology.module(),
        src_typing.module(), src_util.module(), src_variants.module(),
        src_workflow.module(),
        # Primary terms (40, excluding codeGeneration)
        src_adapt_literals.module(), src_adapt_modules.module(),
        src_adapt_simple.module(), src_adapt_terms.module(), src_adapt_utils.module(),
        src_annotations.module(), src_arity.module(), src_checking.module(),
        src_constants.module(), src_encoding.module(),
        src_extract_core.module(), src_extract_helpers.module(), src_extract_util.module(),
        src_formatting.module(), src_grammars.module(),
        src_inference.module(), src_languages.module(), src_lexical.module(),
        src_literals.module(), src_monads.module(), src_names.module(),
        src_parsers.module(), src_reduction.module(), src_reflect.module(),
        src_rewriting.module(), src_schemas.module(), src_serialization.module(),
        src_show_accessors.module(), src_show_core.module(), src_show_graph.module(),
        src_show_meta.module(), src_show_typing.module(), src_show_util.module(),
        src_sorting.module(), src_substitution.module(), src_tarjan.module(),
        src_templates.module(), src_unification.module(),
        # Decoding (21)
        src_dec_accessors.module(), src_dec_ast.module(), src_dec_classes.module(),
        src_dec_coders.module(), src_dec_compute.module(), src_dec_constraints.module(),
        src_dec_core.module(), src_dec_grammar.module(), src_dec_json.module(),
        src_dec_module.module(), src_dec_parsing.module(), src_dec_phantoms.module(),
        src_dec_query.module(), src_dec_relational.module(), src_dec_tabular.module(),
        src_dec_testing.module(), src_dec_topology.module(), src_dec_typing.module(),
        src_dec_util.module(), src_dec_variants.module(), src_dec_workflow.module(),
        # Encoding (21)
        src_enc_accessors.module(), src_enc_ast.module(), src_enc_classes.module(),
        src_enc_coders.module(), src_enc_compute.module(), src_enc_constraints.module(),
        src_enc_core.module(), src_enc_grammar.module(), src_enc_json.module(),
        src_enc_module.module(), src_enc_parsing.module(), src_enc_phantoms.module(),
        src_enc_query.module(), src_enc_relational.module(), src_enc_tabular.module(),
        src_enc_testing.module(), src_enc_topology.module(), src_enc_typing.module(),
        src_enc_util.module(), src_enc_variants.module(), src_enc_workflow.module(),
        # JSON (8)
        src_json_decode.module(), src_json_encode.module(),
        src_json_parser.module(), src_json_writer.module(),
        src_extract_json.module(),
        src_org_json_coder.module(), src_org_json_decoding.module(),
        src_org_json_language.module(),
    ]
    return modules


def bootstrap_graph():
    """Create an empty graph with standard primitives (the bootstrap graph)."""
    primitives = standard_library()
    return Graph(
        elements=(),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=TermLiteral(LiteralString("empty graph")),
        primitives=FrozenDict(primitives),
        schema=Nothing(),
    )


def run_flow(state, flow):
    """Evaluate a Flow computation, raising on failure."""
    empty_trace = Trace(stack=(), messages=(), other=FrozenDict({}))
    result = flow.value(state, empty_trace)
    match result.value:
        case Just(v):
            return v
        case Nothing():
            msgs = list(result.trace.messages) if result.trace.messages else []
            raise RuntimeError("Flow failed: " + "; ".join(msgs))


def _python_to_hydra_json(obj):
    """Convert a Python JSON object (from json.loads) to a Hydra JSON value."""
    if obj is None:
        return JsonModel.ValueNull()
    elif isinstance(obj, bool):
        return JsonModel.ValueBoolean(obj)
    elif isinstance(obj, (int, float)):
        return JsonModel.ValueNumber(float(obj))
    elif isinstance(obj, str):
        return JsonModel.ValueString(obj)
    elif isinstance(obj, list):
        return JsonModel.ValueArray(tuple(_python_to_hydra_json(item) for item in obj))
    elif isinstance(obj, dict):
        return JsonModel.ValueObject(FrozenDict(
            {k: _python_to_hydra_json(v) for k, v in obj.items()}))
    else:
        raise ValueError(f"Unexpected JSON type: {type(obj)}")


def parse_json_file(path):
    """Read a JSON file, parse to hydra.json.model.Value.

    Uses Python's built-in json module for performance and to avoid
    recursion depth issues with the generated parser on large files.
    """
    with open(path, "r", encoding="utf-8") as f:
        obj = json.load(f)
    return _python_to_hydra_json(obj)


def decode_module(bs_graph, universe_modules, do_strip_type_schemes, json_val):
    """Decode a single module from a JSON value.

    Re-implements the logic of code_generation.decode_module_from_json to work
    around a generated code issue (post_process thunk called incorrectly).
    """
    import hydra.json.decode as json_decode
    import hydra.decode.module as decode_mod
    from hydra.core import Name, Type, TypeVariable

    graph = modules_to_graph(bs_graph, tuple(universe_modules), tuple(universe_modules))
    schema_map = build_schema_map(graph)
    mod_type = TypeVariable(Name("hydra.module.Module"))

    # Step 1: Decode JSON to a Term using the schema map
    json_result = json_decode.from_json(schema_map, mod_type, json_val)
    match json_result:
        case Left(value=err):
            raise RuntimeError(f"Module JSON decode error: {err}")
        case Right(value=term):
            pass
        case _:
            raise RuntimeError("Unexpected JSON decode result type")

    # Step 2: Decode the Term to a Module
    mod_result = decode_mod.module(graph, term)
    match mod_result:
        case Left(value=dec_err):
            raise RuntimeError(f"Module decode error: {dec_err.value}")
        case Right(value=mod):
            if do_strip_type_schemes:
                return strip_module_type_schemes(mod)
            return mod
        case _:
            raise RuntimeError("Unexpected module decode result type")


def load_modules_from_json(strip_type_schemes, base_path, universe_modules, namespaces):
    """Load modules from JSON files."""
    bs_graph = bootstrap_graph()
    modules = []
    for ns in namespaces:
        file_path = os.path.join(base_path, namespace_to_path(ns) + ".json")
        json_val = parse_json_file(file_path)
        mod = decode_module(bs_graph, universe_modules, strip_type_schemes, json_val)
        print(f"  Loaded: {ns.value}")
        modules.append(mod)
    return modules


def discover_json_namespaces(base_path):
    """Discover namespaces from JSON files in a directory tree."""
    if not os.path.isdir(base_path):
        return []

    namespaces = []
    for root, _dirs, files in os.walk(base_path):
        for filename in files:
            if filename.endswith(".json"):
                full_path = os.path.join(root, filename)
                rel_path = os.path.relpath(full_path, base_path)
                # Remove .json extension and convert path separators to dots
                without_ext = rel_path[:-5]
                ns = without_ext.replace(os.sep, ".").replace("/", ".")
                namespaces.append(Namespace(ns))

    namespaces.sort(key=lambda n: n.value)
    return namespaces


def load_all_modules_from_json_dir(base_path, universe_modules):
    """Load all modules from a JSON directory.
    TypeSchemes are stripped by default (suitable for main/kernel modules).
    """
    return load_all_modules_from_json_dir_with(True, base_path, universe_modules)


def load_all_modules_from_json_dir_with(strip_type_schemes, base_path, universe_modules):
    """Load all modules from a JSON directory with control over TypeScheme stripping."""
    namespaces = discover_json_namespaces(base_path)
    print(f"  Discovered {len(namespaces)} modules in {base_path}")
    return load_modules_from_json(strip_type_schemes, base_path, universe_modules, namespaces)


def generate_sources(coder, language, do_expand, do_hoist_case, do_hoist_poly,
                     base_path, universe, modules_to_generate):
    """Generate source files and write them to disk."""
    bs_graph = bootstrap_graph()
    flow = generate_source_files(
        coder, language,
        do_expand, do_hoist_case, do_hoist_poly,
        bs_graph, tuple(universe), tuple(modules_to_generate))
    files = run_flow(bs_graph, flow)
    for path, content in files:
        file_path = os.path.join(base_path, path)
        if not content.endswith("\n"):
            content = content + "\n"
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w") as f:
            f.write(content)


def strip_term_types(m):
    """Strip System F type annotations from all term bodies in a module.

    Uses remove_types_from_term which strips TypeApplication, TypeLambda,
    lambda domain types, and let-binding TypeSchemes from terms.
    Module-level TypeSchemes are stripped from term bindings (to avoid
    bigfloat/float64 conflicts) but preserved on type-defining bindings
    (needed by is_native_type for schema graph construction).
    """
    stripped = []
    for b in m.elements:
        new_term = remove_types_from_term(b.term)
        new_type = b.type if is_native_type(b) else Nothing()
        stripped.append(Binding(b.name, new_term, new_type))
    return Module(m.namespace, tuple(stripped), m.type_dependencies, m.term_dependencies, m.description)


def strip_all_term_types(modules):
    """Strip System F type annotations from all modules."""
    return [strip_term_types(m) for m in modules]


def filter_kernel_modules(modules):
    """Filter modules to only kernel modules (exclude hydra.ext.* namespaces)."""
    return [m for m in modules if not m.namespace.value.startswith("hydra.ext.")]


def filter_type_modules(modules):
    """Filter modules to only those containing type-defining bindings."""
    return [m for m in modules if any(is_native_type(b) for b in m.elements)]


def write_java(base_path, universe, mods):
    """Generate Java source files from modules."""
    from hydra.ext.java.coder import module_to_java
    from hydra.ext.java.language import java_language
    generate_sources(
        module_to_java, java_language(),
        True, False, True,
        base_path, universe, mods)


def write_python(base_path, universe, mods):
    """Generate Python source files from modules."""
    from hydra.ext.python.coder import module_to_python
    from hydra.ext.python.language import python_language
    generate_sources(
        module_to_python, python_language(),
        True, True, False,
        base_path, universe, mods)


def write_haskell(base_path, universe, mods):
    """Generate Haskell source files from modules."""
    from hydra.ext.haskell.coder import module_to_haskell
    from hydra.ext.haskell.language import haskell_language
    generate_sources(
        module_to_haskell, haskell_language(),
        False, False, False,
        base_path, universe, mods)
