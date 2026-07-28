"""
Test runner for Hydra-generated test cases.

This module provides pytest-based test execution for all generated test cases.
All test cases are now UniversalTestCase instances (string comparison).
"""

from __future__ import annotations

import sys
from pathlib import Path

# IMPORTANT: Add paths BEFORE any hydra imports
# Main path provides the full hydra package, gen-test provides hydra.test
_root = Path(__file__).parent.parent.parent
_main_path = _root / "main" / "python"
_gen_main_path = _root / "gen-main" / "python"
_gen_test_path = _root / "gen-test" / "python"

# All three source roots must be on sys.path for the extend_path namespace
# package mechanism to merge hydra.* across main, gen-main, and gen-test.
for _p in [str(_gen_test_path), str(_gen_main_path), str(_main_path)]:
    if _p not in sys.path:
        sys.path.insert(0, _p)

from typing import Optional

import hydra.core
import hydra.graph
import hydra.lexical
import hydra.rewriting
import hydra.testing
from hydra.overlay.python.dsl.python import FrozenDict, None_
import hydra.typing

import hydra.test.test_types

# Now we can import the test modules
import hydra.test.test_suite as test_suite
import hydra.test.test_graph as test_graph

from hydra_test_group_walker import (
    default_test_runner,
    generate_pytest_tests,
    write_benchmark_json,
)


def _load_kernel_term_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """
    Load kernel term bindings from JSON.

    The test graph needs kernel term bindings so that the evaluator can
    resolve references to kernel definitions at runtime.

    These are loaded from the JSON representation in hydra-haskell rather than
    from generated Python Source modules. This works because term modules don't
    contribute to the schema map (no chicken-and-egg problem) and modules loaded
    from JSON already carry full type annotations (no inference needed).

    Returns:
        Dictionary mapping binding names to Binding objects
    """
    import sys
    from hydra.generation import load_modules_from_json, read_manifest_field, strip_all_term_types

    # Bump recursion limit for the recursive JSON decoder
    old_limit = sys.getrecursionlimit()
    sys.setrecursionlimit(10000)

    # Locate dist/json/hydra-kernel: either via HYDRA_JSON_DIR env var or by
    # searching upward from CWD for a dist/json directory.
    import os
    json_dir = os.environ.get("HYDRA_JSON_DIR")
    if not json_dir:
        search = os.path.abspath(os.getcwd())
        while search != "/":
            candidate = os.path.join(search, "dist", "json", "hydra-kernel", "src", "main", "json")
            if os.path.isdir(candidate):
                json_dir = candidate
                break
            search = os.path.dirname(search)
    if not json_dir:
        json_dir = "../../dist/json/hydra-kernel/src/main/json"  # fallback

    # Load only the essential evaluator term modules (hydra.annotations
    # and their dependencies). Loading all 92 term modules from JSON is too slow.
    # This matches the optimization in Haskell (TestUtils.hs) and Java (TestSuiteRunner.java).
    evaluator_term_namespaces = [
        hydra.core.Name("hydra.annotations"),
        hydra.core.Name("hydra.constants"),
        hydra.core.Name("hydra.decode.core"),
        hydra.core.Name("hydra.dependencies"),
        hydra.core.Name("hydra.encode.core"),
        hydra.core.Name("hydra.extract.core"),
        hydra.core.Name("hydra.lexical"),
        hydra.core.Name("hydra.rewriting"),
        hydra.core.Name("hydra.scoping"),
        hydra.core.Name("hydra.print.core"),
        hydra.core.Name("hydra.strip"),
        hydra.core.Name("hydra.variables"),
    ]

    term_mods = load_modules_from_json(json_dir, evaluator_term_namespaces)

    # Strip System F type annotations (TypeLambda, TypeApplication, etc.) from
    # term bodies. The JSON representation preserves the full System F encoding,
    # but the evaluator works at the simply-typed level.
    term_mods = strip_all_term_types(term_mods)

    sys.setrecursionlimit(old_limit)

    from hydra.packaging import DefinitionTerm
    from hydra.core import Binding
    import hydra.overlay.python.lib.optionals as Optionals
    from hydra.scoping import term_signature_to_type_scheme
    bindings = {}
    for mod in term_mods:
        for d in mod.definitions:
            if isinstance(d, DefinitionTerm):
                td = d.value
                ts = Optionals.map(term_signature_to_type_scheme, td.signature)
                bindings[td.name] = Binding(td.name, td.body, ts)

    return bindings


def _load_bootstrap_type_schemes() -> FrozenDict:
    """
    Load bootstrap type schemes for the test schema graph.

    Uses hydra.json.bootstrap.types_by_name (the same bootstrap type map
    used for JSON decoding) to build a Map[Name, TypeScheme] suitable for
    the test graph's schema_types. This provides type definitions for
    hydra.core, hydra.util, hydra.typing, hydra.error, hydra.graph,
    and hydra.module — all the types needed by inference tests.

    This mirrors Java's Generation.bootstrapTypeSchemes().
    """
    from hydra.json.bootstrap import types_by_name
    from hydra.scoping import f_type_to_type_scheme

    result = {}
    for name, typ in types_by_name.items():
        result[name] = f_type_to_type_scheme(typ)
    return FrozenDict(result)


import os
import time
import atexit

# Benchmark output path. When set, the test runner records group-level
# wall-clock timing and writes a JSON benchmark file after all tests complete.
BENCHMARK_OUTPUT = os.environ.get("HYDRA_BENCHMARK_OUTPUT", "")

# Global state for benchmark timing
_benchmark_timers: dict[str, int] = {}  # path -> start time (perf_counter_ns)
_benchmark_results: dict[str, float] = {}  # path -> elapsed ms
_init_start_ns: int = 0  # start time for test infrastructure initialization


def _empty_context() -> hydra.typing.InferenceContext:
    """Create an empty InferenceContext for test use."""
    return hydra.typing.InferenceContext(
        fresh_type_variable_count=0,
        trace=(),
    )


def _patch_graph_with_default_impls(graph: hydra.graph.Graph) -> hydra.graph.Graph:
    """
    Return a copy of the graph where each primitive that has a
    primitiveDefinitionDefaultImplementation uses it (via reduce_term) instead
    of the native host implementation.  Primitives without a default keep their
    native implementation unchanged.
    """
    import hydra.reduction as reduction
    from hydra.overlay.python.dsl.python import FrozenDict, Given, None_
    from dataclasses import replace

    native_graph = graph  # used as the fallback evaluation context

    def make_default_impl(impl_term):
        """Build a primitive implementation that evaluates impl_term applied to args."""
        def default_impl(g, args):
            # Apply impl_term to each argument left-to-right
            applied = impl_term
            for arg in args:
                applied = hydra.core.TermApplication(hydra.core.Application(applied, arg))
            cx = _empty_context()
            return reduction.reduce_term(cx, native_graph, True, applied)
        return default_impl

    patched = {}
    for name, prim in graph.primitives.items():
        match prim.definition.default_implementation:
            case None_():
                patched[name] = prim
            case Given(impl_term):
                patched[name] = replace(prim, implementation=make_default_impl(impl_term))

    return replace(graph, primitives=FrozenDict(patched))


def build_test_graph(use_default_impls: bool = False) -> hydra.graph.Graph:
    """
    Build the test graph with schema and primitives.

    This mirrors the Haskell testGraph setup:
        testSchemaGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes hydraCoreGraph)
            (kernelElements ++ testElements)
        testGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes testSchemaGraph)
            (kernelTermBindings ++ dataBindings)

    Args:
        use_default_impls: If True, replace native primitive implementations with
            reducer-based wrappers that evaluate primitiveDefinitionDefaultImplementation
            where available (testing default implementations instead of native ones).

    Returns:
        Graph: The test graph
    """
    from hydra.overlay.python.dsl.python import FrozenDict, None_, Given
    import hydra.lexical

    from hydra.generation import bootstrap_graph
    bs_graph = bootstrap_graph()

    # Step 1: Build schema types from bootstrap type map + test types
    # The bootstrap type schemes provide types for hydra.core, hydra.util,
    # hydra.typing, hydra.error, hydra.graph, and hydra.module.
    bootstrap_types = _load_bootstrap_type_schemes()

    # Get test type definitions and convert each to a TypeScheme
    # (extracting forall variables, just like f_type_to_type_scheme does)
    from hydra.scoping import f_type_to_type_scheme
    test_types_dict = test_graph.test_types()

    # Merge bootstrap types with test-specific types
    all_schema_types = dict(bootstrap_types)
    for name, typ in test_types_dict.items():
        all_schema_types[name] = f_type_to_type_scheme(typ)
    schema_types = FrozenDict(all_schema_types)

    # Step 2: Build testGraph
    # Load kernel term bindings from JSON
    kernel_terms = _load_kernel_term_bindings()
    kernel_term_bindings = list(kernel_terms.values())

    # Build term bindings from test data
    test_terms_dict = test_graph.test_terms()
    data_bindings = [hydra.core.Binding(name=name, term=term, type_scheme=None_())
                     for name, term in test_terms_dict.items()]

    # Build the test graph with schema types and all term bindings
    graph = hydra.lexical.elements_to_graph(
        bs_graph, schema_types, tuple(kernel_term_bindings + data_bindings))

    if use_default_impls:
        graph = _patch_graph_with_default_impls(graph)

    return graph


# Cache the test graph at module level.
# This mirrors the Haskell approach where the graph is computed once and reused.
_test_graph: Optional[hydra.graph.Graph] = None

# When set to True (via --default-impls flag or HYDRA_DEFAULT_IMPLS=1 env var),
# the test graph uses default primitive implementations instead of native ones.
USE_DEFAULT_IMPLS: bool = (
    "--default-impls" in sys.argv or os.environ.get("HYDRA_DEFAULT_IMPLS", "") == "1"
)


def get_test_graph() -> hydra.graph.Graph:
    """Get the cached test graph, building it if necessary."""
    global _test_graph, _init_start_ns
    if _test_graph is None:
        if BENCHMARK_OUTPUT and _init_start_ns == 0:
            _init_start_ns = time.perf_counter_ns()
        _test_graph = build_test_graph(use_default_impls=USE_DEFAULT_IMPLS)
        # Record initialization time
        if BENCHMARK_OUTPUT and _init_start_ns > 0:
            elapsed_ns = time.perf_counter_ns() - _init_start_ns
            _benchmark_results["common/_initialization"] = elapsed_ns / 1_000_000.0
    return _test_graph


# Generate all test functions from the test suite
_all_tests = generate_pytest_tests(
    test_suite.all_tests(), default_test_runner, test_suite,
    benchmark_output=BENCHMARK_OUTPUT, benchmark_timers=_benchmark_timers,
    benchmark_results=_benchmark_results)

# Eagerly initialize test infrastructure so that JSON module loading
# and graph construction are not counted inside the first test group's timer.
get_test_graph()
get_test_graph()

# Build a mapping from Python test names to Hydra paths for cross-language benchmarking
# This can be imported by benchmark tools to correlate test results across implementations
HYDRA_PATH_MAP: dict[str, str] = {f"test_{name}": path for name, path, _ in _all_tests}

# Dynamically add test functions to module namespace for pytest discovery
for test_name, hydra_path, test_fn in _all_tests:
    globals()[f"test_{test_name}"] = test_fn

# Register benchmark output writing if HYDRA_BENCHMARK_OUTPUT is set.
# Uses atexit to write JSON after all tests complete.
if BENCHMARK_OUTPUT:
    atexit.register(
        write_benchmark_json, BENCHMARK_OUTPUT, default_test_runner,
        test_suite.all_tests(), test_suite, "python", _benchmark_results)
