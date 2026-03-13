"""
Test runner for Hydra-generated test cases.

This module provides pytest-based test execution for all generated test cases,
mirroring the functionality of TestSuiteSpec.hs in Haskell.
"""

from __future__ import annotations

import sys
from pathlib import Path

# IMPORTANT: Add paths BEFORE any hydra imports
# Main path provides the full hydra package, gen-test provides hydra.test
_root = Path(__file__).parent.parent.parent
_main_path = _root / "main" / "python"
_gen_test_path = _root / "gen-test" / "python"

# Main must come first so hydra.compute etc. are found there,
# but gen-test must also be present so hydra.test can be found
sys.path.insert(0, str(_gen_test_path))
sys.path.insert(0, str(_main_path))

# Note: Performance optimizations are now built into the generated code
# (short-circuits in substInType and composeTypeSubst)

from typing import Callable, Optional
import pytest
import importlib.util
import types

import hydra.coders
import hydra.compute
import hydra.core
import hydra.encode.core
import hydra.formatting
import hydra.graph
import hydra.inference
import hydra.lexical
import hydra.reduction
import hydra.serialization
import hydra.sorting
import hydra.util
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.sources.libraries
import hydra.testing
import hydra.typing
import hydra.hoisting
import hydra.substitution
import hydra.unification
from hydra.dsl.python import FrozenDict, Just, Left, Nothing, Right, frozenlist
import hydra.context

# Manually load and register hydra.test package
# Import the main hydra package first
import hydra

# Load the hydra.test package
_test_init_path = _gen_test_path / "hydra" / "test" / "__init__.py"
_test_pkg_spec = importlib.util.spec_from_file_location("hydra.test", _test_init_path)
_test_pkg = importlib.util.module_from_spec(_test_pkg_spec)
sys.modules["hydra.test"] = _test_pkg
_test_pkg_spec.loader.exec_module(_test_pkg)

# Register hydra.test as a submodule of hydra
hydra.test = _test_pkg

# Import test_types first so it's available when test_terms and test_suite load
import hydra.test.test_types

# Now we can import the test modules
import hydra.test.test_suite as test_suite
import hydra.test.test_graph as test_graph


# Type alias for test runner functions
TestRunner = Callable[[str, hydra.testing.TestCaseWithMetadata], Optional[Callable[[], None]]]


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

    json_dir = "../hydra-haskell/src/gen-main/json"

    # Load only the essential evaluator term modules (hydra.annotations
    # and their dependencies). Loading all 92 term modules from JSON is too slow.
    # This matches the optimization in Haskell (TestUtils.hs) and Java (TestSuiteRunner.java).
    evaluator_term_namespaces = [
        hydra.core.Name("hydra.constants"),
        hydra.core.Name("hydra.show.core"),
        hydra.core.Name("hydra.extract.core"),
        hydra.core.Name("hydra.lexical"),
        hydra.core.Name("hydra.rewriting"),
        hydra.core.Name("hydra.decode.core"),
        hydra.core.Name("hydra.encode.core"),
        hydra.core.Name("hydra.annotations"),
    ]

    term_mods = load_modules_from_json(False, json_dir, evaluator_term_namespaces)

    # Strip System F type annotations (TypeLambda, TypeApplication, etc.) from
    # term bodies. The JSON representation preserves the full System F encoding,
    # but the evaluator works at the simply-typed level.
    term_mods = strip_all_term_types(term_mods)

    sys.setrecursionlimit(old_limit)

    bindings = {}
    for mod in term_mods:
        for binding in mod.elements:
            bindings[binding.name] = binding

    return bindings


def _load_bootstrap_type_schemes() -> FrozenDict:
    """
    Load bootstrap type schemes for the test schema graph.

    Uses hydra.json.bootstrap.types_by_name (the same bootstrap type map
    used for JSON decoding) to build a Map[Name, TypeScheme] suitable for
    the test graph's schema_types. This provides type definitions for
    hydra.core, hydra.compute, hydra.context, hydra.error, hydra.graph,
    and hydra.module — all the types needed by inference tests.

    This mirrors Java's Generation.bootstrapTypeSchemes().
    """
    from hydra.json.bootstrap import types_by_name
    from hydra.rewriting import f_type_to_type_scheme

    result = {}
    for name, typ in types_by_name().items():
        result[name] = f_type_to_type_scheme(typ)
    return FrozenDict(result)


def is_disabled(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case is marked as disabled."""
    disabled_tag = hydra.testing.Tag("disabled")
    return disabled_tag in tcase.tags

import os
import time
import subprocess
import atexit
from decimal import Decimal

# Benchmark output path. When set, the test runner records group-level
# wall-clock timing and writes a JSON benchmark file after all tests complete.
BENCHMARK_OUTPUT = os.environ.get("HYDRA_BENCHMARK_OUTPUT", "")

# Global state for benchmark timing
_benchmark_timers: dict[str, int] = {}  # path -> start time (perf_counter_ns)
_benchmark_results: dict[str, float] = {}  # path -> elapsed ms
_init_start_ns: int = 0  # start time for test infrastructure initialization

def should_skip_test(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case should be skipped.

    Skip tests that are:
    - disabled: explicitly marked as not working
    """
    return is_disabled(tcase)


def _empty_context() -> hydra.context.Context:
    """Create an empty Context for test use."""
    return hydra.context.Context(
        trace=(),
        messages=(),
        other=FrozenDict({}),
    )


def build_test_graph() -> hydra.graph.Graph:
    """
    Build the test graph with schema and primitives.

    This mirrors the Haskell testGraph setup:
        testSchemaGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes hydraCoreGraph)
            (kernelElements ++ testElements)
        testGraph = elementsToGraph hydraCoreGraph (decodeSchemaTypes testSchemaGraph)
            (kernelTermBindings ++ dataBindings)

    Returns:
        Graph: The test graph
    """
    from hydra.dsl.python import FrozenDict, Nothing, Just
    import hydra.lexical

    from hydra.generation import bootstrap_graph
    bs_graph = bootstrap_graph()

    # Step 1: Build schema types from bootstrap type map + test types
    # The bootstrap type schemes provide types for hydra.core, hydra.compute,
    # hydra.context, hydra.error, hydra.graph, and hydra.module.
    bootstrap_types = _load_bootstrap_type_schemes()

    # Get test type definitions and convert each to a TypeScheme
    # (extracting forall variables, just like f_type_to_type_scheme does)
    from hydra.rewriting import f_type_to_type_scheme
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
    data_bindings = [hydra.core.Binding(name=name, term=term, type=Nothing())
                     for name, term in test_terms_dict.items()]

    # Build the test graph with schema types and all term bindings
    return hydra.lexical.elements_to_graph(
        bs_graph, schema_types, tuple(kernel_term_bindings + data_bindings))


# Cache the test graph at module level.
# This mirrors the Haskell approach where the graph is computed once and reused.
_test_graph: Optional[hydra.graph.Graph] = None


def get_test_graph() -> hydra.graph.Graph:
    """Get the cached test graph, building it if necessary."""
    global _test_graph, _init_start_ns
    if _test_graph is None:
        if BENCHMARK_OUTPUT and _init_start_ns == 0:
            _init_start_ns = time.perf_counter_ns()
        _test_graph = build_test_graph()
        # Record initialization time
        if BENCHMARK_OUTPUT and _init_start_ns > 0:
            elapsed_ns = time.perf_counter_ns() - _init_start_ns
            _benchmark_results["common/_initialization"] = elapsed_ns / 1_000_000.0
    return _test_graph


def default_test_runner(desc: str, tcase: hydra.testing.TestCaseWithMetadata) -> Optional[Callable[[], None]]:
    """
    Default test runner that handles all test case types.

    Args:
        desc: Full description of the test case (includes parent descriptions)
        tcase: The test case metadata and data

    Returns:
        Optional test function to execute, or None if test should be skipped
    """
    if should_skip_test(tcase):
        return None

    case = tcase.case

    match case:
        case hydra.testing.TestCaseAlphaConversion(value=tc):
            return lambda: run_alpha_conversion_test(tc)

        case hydra.testing.TestCaseCaseConversion(value=tc):
            return lambda: run_case_conversion_test(tc)

        case hydra.testing.TestCaseDeannotateTerm(value=tc):
            return lambda: run_deannotate_term_test(tc)

        case hydra.testing.TestCaseDeannotateType(value=tc):
            return lambda: run_deannotate_type_test(tc)

        case hydra.testing.TestCaseDelegatedEvaluation(value=tc):
            # Delegated evaluation runs in the target language - always passes here
            return lambda: None

        case hydra.testing.TestCaseEtaExpansion(value=tc):
            return lambda: run_eta_expansion_test(desc, tc)

        case hydra.testing.TestCaseFlattenLetTerms(value=tc):
            return lambda: run_flatten_let_terms_test(tc)

        case hydra.testing.TestCaseFreeVariables(value=tc):
            return lambda: run_free_variables_test(tc)

        case hydra.testing.TestCaseEvaluation(value=tc):
            return lambda: run_evaluation_test(desc, tc)

        case hydra.testing.TestCaseInference(value=tc):
            return lambda: run_inference_test(desc, tc)

        case hydra.testing.TestCaseInferenceFailure(value=tc):
            return lambda: run_inference_failure_test(desc, tc)

        case hydra.testing.TestCaseJsonParser(value=tc):
            # JSON parser tests require JSON parser module - skip for now
            return None

        case hydra.testing.TestCaseJsonWriter(value=tc):
            # JSON writer tests require JSON writer module - skip for now
            return None

        case hydra.testing.TestCaseLiftLambdaAboveLet(value=tc):
            return lambda: run_lift_lambda_above_let_test(tc)

        case hydra.testing.TestCaseSerialization(value=tc):
            return lambda: run_serialization_test(tc)

        case hydra.testing.TestCaseSimplifyTerm(value=tc):
            return lambda: run_simplify_term_test(tc)

        case hydra.testing.TestCaseTopologicalSort(value=tc):
            return lambda: run_topological_sort_test(tc)

        case hydra.testing.TestCaseTopologicalSortBindings(value=tc):
            return lambda: run_topological_sort_bindings_test(tc)

        case hydra.testing.TestCaseTopologicalSortSCC(value=tc):
            return lambda: run_topological_sort_scc_test(tc)

        case hydra.testing.TestCaseTypeChecking(value=tc):
            return lambda: run_type_checking_test(desc, tc)

        case hydra.testing.TestCaseTypeCheckingFailure(value=tc):
            # No test cases currently use this
            return None

        case hydra.testing.TestCaseTypeReduction(value=tc):
            return lambda: run_type_reduction_test(desc, tc)

        case hydra.testing.TestCaseNormalizeTypeVariables(value=tc):
            return lambda: run_normalize_type_variables_test(tc)

        case hydra.testing.TestCaseFoldOverTerm(value=tc):
            return lambda: run_fold_over_term_test(tc)

        case hydra.testing.TestCaseRewriteTerm(value=tc):
            return lambda: run_rewrite_term_test(tc)

        case hydra.testing.TestCaseRewriteType(value=tc):
            return lambda: run_rewrite_type_test(tc)

        case hydra.testing.TestCaseJsonDecode(value=tc):
            return lambda: run_json_decode_test(tc)

        case hydra.testing.TestCaseJsonEncode(value=tc):
            return lambda: run_json_encode_test(tc)

        case hydra.testing.TestCaseJsonRoundtrip(value=tc):
            return lambda: run_json_roundtrip_test(tc)

        case hydra.testing.TestCaseHoistSubterms(value=tc):
            return lambda: run_hoist_subterms_test(tc)

        case hydra.testing.TestCaseHoistCaseStatements(value=tc):
            return lambda: run_hoist_case_statements_test(tc)

        case hydra.testing.TestCaseHoistLetBindings(value=tc):
            return lambda: run_hoist_let_bindings_test(tc)

        case hydra.testing.TestCaseHoistPolymorphicLetBindings(value=tc):
            return lambda: run_hoist_polymorphic_let_bindings_test(tc)

        case hydra.testing.TestCaseSubstInType(value=tc):
            return lambda: run_subst_in_type_test(tc)

        case hydra.testing.TestCaseVariableOccursInType(value=tc):
            return lambda: run_variable_occurs_in_type_test(tc)

        case hydra.testing.TestCaseUnifyTypes(value=tc):
            return lambda: run_unify_types_test(tc)

        case hydra.testing.TestCaseJoinTypes(value=tc):
            return lambda: run_join_types_test(tc)

        case hydra.testing.TestCaseUnshadowVariables(value=tc):
            return lambda: run_unshadow_variables_test(tc)

        case _:
            # Fail on unhandled test case types to catch missing implementations
            case_type = type(tcase.case).__name__
            def fail_unhandled():
                pytest.fail(f"Unhandled test case type: {case_type}")
            return fail_unhandled


def run_case_conversion_test(test_case: hydra.testing.CaseConversionTestCase) -> None:
    """
    Execute a case conversion test.

    Args:
        test_case: The case conversion test data

    Raises:
        AssertionError: If the conversion doesn't match expected output
    """
    from_conv = test_case.from_convention
    to_conv = test_case.to_convention
    from_str = test_case.from_string
    expected = test_case.to_string

    actual = hydra.formatting.convert_case(from_conv, to_conv, from_str)

    assert actual == expected, (
        f"Case conversion failed:\n"
        f"  Input: {from_str!r}\n"
        f"  Expected: {expected!r}\n"
        f"  Actual: {actual!r}"
    )


def run_inference_test(desc: str, test_case: hydra.testing.InferenceTestCase) -> None:
    """
    Execute a type inference test.

    Args:
        desc: Test description for error messages
        test_case: The inference test data

    Raises:
        AssertionError: If inference fails or produces unexpected results
    """
    input_term = test_case.input
    expected_scheme = test_case.output

    # Get cached test graph
    graph = get_test_graph()
    cx = _empty_context()

    # Infer the type — returns Either[InContext[Error], ((Term, TypeScheme), Context)]
    result = hydra.inference.infer_type_of(cx, graph, input_term)

    match result:
        case Left(value=in_ctx):
            pytest.fail(
                f"Type inference failed for {desc}:\n"
                f"  Error: {in_ctx.object}\n"
                f"  Term: {hydra.show.core.term(input_term)}"
            )
            return
        case Right(value=((inferred_term, inferred_scheme), _cx2)):
            pass

    # Compare type schemes
    expected_str = hydra.show.core.type_scheme(expected_scheme)
    inferred_str = hydra.show.core.type_scheme(inferred_scheme)

    assert inferred_str == expected_str, (
        f"Type inference mismatch for {desc}:\n"
        f"  Expected: {expected_str}\n"
        f"  Inferred: {inferred_str}\n"
        f"  Term: {hydra.show.core.term(input_term)}"
    )

    # Compare terms (with types removed for comparison)
    input_normalized = hydra.show.core.term(hydra.rewriting.remove_types_from_term(input_term))
    inferred_normalized = hydra.show.core.term(hydra.rewriting.remove_types_from_term(inferred_term))

    assert inferred_normalized == input_normalized, (
        f"Term reconstruction mismatch for {desc}:\n"
        f"  Input: {input_normalized}\n"
        f"  Inferred: {inferred_normalized}"
    )


def run_inference_failure_test(desc: str, test_case: hydra.testing.InferenceFailureTestCase) -> None:
    """
    Execute a test that expects type inference to fail.

    Args:
        desc: Test description for error messages
        test_case: The inference failure test data

    Raises:
        AssertionError: If inference succeeds when it should fail
    """
    input_term = test_case.input

    # Get cached test graph
    graph = get_test_graph()
    cx = _empty_context()

    # Try to infer the type — returns Either
    result = hydra.inference.infer_type_of(cx, graph, input_term)

    match result:
        case Left(_):
            # Expected failure - test passes
            pass
        case Right(value=((_, inferred_scheme), _)):
            pytest.fail(
                f"Type inference should have failed for {desc}:\n"
                f"  Term: {hydra.show.core.term(input_term)}\n"
                f"  Inferred: {hydra.show.core.type_scheme(inferred_scheme)}"
            )


def run_alpha_conversion_test(test_case: hydra.testing.AlphaConversionTestCase) -> None:
    """Execute an alpha conversion test."""
    result = hydra.reduction.alpha_convert(
        test_case.old_variable,
        test_case.new_variable,
        test_case.term
    )
    assert result == test_case.result, (
        f"Alpha conversion failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.result)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_deannotate_term_test(test_case: hydra.testing.DeannotateTermTestCase) -> None:
    """Execute a deannotate term test."""
    result = hydra.rewriting.deannotate_term(test_case.input)
    assert result == test_case.output, (
        f"Deannotate term failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_deannotate_type_test(test_case: hydra.testing.DeannotateTypeTestCase) -> None:
    """Execute a deannotate type test."""
    result = hydra.rewriting.deannotate_type(test_case.input)
    assert result == test_case.output, (
        f"Deannotate type failed:\n"
        f"  Expected: {hydra.show.core.type(test_case.output)}\n"
        f"  Actual: {hydra.show.core.type(result)}"
    )


def run_eta_expansion_test(desc: str, test_case: hydra.testing.EtaExpansionTestCase) -> None:
    """Execute an eta expansion test.

    Note: Uses eta_expand_typed_term (not eta_expand_term) to match Haskell behavior.
    The typed version uses type information to avoid expanding bare primitives at top level.
    """
    graph = get_test_graph()
    cx = _empty_context()

    result = hydra.reduction.eta_expand_typed_term(cx, graph, test_case.input)

    match result:
        case Left(value=err):
            pytest.fail(f"Eta expansion failed for {desc}: {err.object.value}")
        case Right(value=expanded):
            pass
    assert expanded == test_case.output, (
        f"Eta expansion failed for {desc}:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(expanded)}"
    )


def run_flatten_let_terms_test(test_case: hydra.testing.FlattenLetTermsTestCase) -> None:
    """Execute a flatten let terms test."""
    result = hydra.rewriting.flatten_let_terms(test_case.input)
    assert result == test_case.output, (
        f"Flatten let terms failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_free_variables_test(test_case: hydra.testing.FreeVariablesTestCase) -> None:
    """Execute a free variables test."""
    result = hydra.rewriting.free_variables_in_term(test_case.input)
    assert result == test_case.output, (
        f"Free variables failed:\n"
        f"  Expected: {test_case.output}\n"
        f"  Actual: {result}"
    )


def run_evaluation_test(desc: str, test_case: hydra.testing.EvaluationTestCase) -> None:
    """Execute a term evaluation test."""
    graph = get_test_graph()
    cx = _empty_context()

    eager = test_case.evaluation_style == hydra.testing.EvaluationStyle.EAGER

    result = hydra.reduction.reduce_term(cx, graph, eager, test_case.input)

    match result:
        case Left(value=err):
            pytest.fail(
                f"Evaluation failed for {desc}:\n"
                f"  Error: {err.object.value}\n"
                f"  Term: {hydra.show.core.term(test_case.input)}"
            )
            return
        case Right(value=reduced):
            pass

    assert reduced == test_case.output, (
        f"Evaluation mismatch for {desc}:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(reduced)}"
    )


def run_lift_lambda_above_let_test(test_case: hydra.testing.LiftLambdaAboveLetTestCase) -> None:
    """Execute a lift lambda above let test."""
    result = hydra.rewriting.lift_lambda_above_let(test_case.input)
    assert result == test_case.output, (
        f"Lift lambda above let failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_serialization_test(test_case: hydra.testing.SerializationTestCase) -> None:
    """Execute an AST serialization test."""
    result = hydra.serialization.print_expr(
        hydra.serialization.parenthesize(test_case.input)
    )
    assert result == test_case.output, (
        f"Serialization failed:\n"
        f"  Expected: {test_case.output!r}\n"
        f"  Actual: {result!r}"
    )


def run_simplify_term_test(test_case: hydra.testing.SimplifyTermTestCase) -> None:
    """Execute a simplify term test."""
    result = hydra.rewriting.simplify_term(test_case.input)
    assert result == test_case.output, (
        f"Simplify term failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_unshadow_variables_test(test_case: hydra.testing.UnshadowVariablesTestCase) -> None:
    """Execute an unshadow variables test."""
    result = hydra.rewriting.unshadow_variables(test_case.input)
    assert result == test_case.output, (
        f"Unshadow variables failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_topological_sort_test(test_case: hydra.testing.TopologicalSortTestCase) -> None:
    """Execute a topological sort test."""
    result = hydra.sorting.topological_sort(test_case.adjacency_list)
    assert result == test_case.expected, (
        f"Topological sort failed:\n"
        f"  Expected: {test_case.expected}\n"
        f"  Actual: {result}"
    )


def run_topological_sort_bindings_test(test_case: hydra.testing.TopologicalSortBindingsTestCase) -> None:
    """Execute a topological sort bindings test."""
    # Convert list of tuples to FrozenDict
    binding_map = FrozenDict({name: term for name, term in test_case.bindings})
    result = hydra.rewriting.topological_sort_binding_map(binding_map)

    # Convert result and expected to sets for comparison (order within SCC doesn't matter)
    result_sets = frozenset(frozenset(group) for group in result)
    expected_sets = frozenset(frozenset(tuple(pair) for pair in group) for group in test_case.expected)

    assert result_sets == expected_sets, (
        f"Topological sort bindings failed:\n"
        f"  Expected: {test_case.expected}\n"
        f"  Actual: {list(result)}"
    )


def run_topological_sort_scc_test(test_case: hydra.testing.TopologicalSortSCCTestCase) -> None:
    """Execute a topological sort SCC test."""
    result = hydra.sorting.topological_sort_components(test_case.adjacency_list)
    assert result == test_case.expected, (
        f"Topological sort SCC failed:\n"
        f"  Expected: {test_case.expected}\n"
        f"  Actual: {result}"
    )


def run_type_checking_test(desc: str, test_case: hydra.testing.TypeCheckingTestCase) -> None:
    """Execute a type checking test."""
    # Get cached test graph
    graph = get_test_graph()
    cx = _empty_context()

    # Infer the type — returns Either
    result = hydra.inference.infer_type_of(cx, graph, test_case.input)

    match result:
        case Left(value=in_ctx):
            pytest.fail(
                f"Type checking failed for {desc}:\n"
                f"  Error: {in_ctx.object}\n"
                f"  Term: {hydra.show.core.term(test_case.input)}"
            )
            return
        case Right(value=((inferred_term, inferred_scheme), _cx2)):
            pass

    # Compare the inferred type with expected output type
    # Convert the TypeScheme to a Type with forall quantifiers for comparison
    expected_type_str = hydra.show.core.type(test_case.output_type)
    inferred_type = hydra.rewriting.type_scheme_to_f_type(inferred_scheme)
    inferred_type_str = hydra.show.core.type(inferred_type)

    assert inferred_type_str == expected_type_str, (
        f"Type checking type mismatch for {desc}:\n"
        f"  Expected type: {expected_type_str}\n"
        f"  Inferred type: {inferred_type_str}"
    )

    # Compare the annotated terms
    expected_term_str = hydra.show.core.term(test_case.output_term)
    inferred_term_str = hydra.show.core.term(inferred_term)

    assert inferred_term_str == expected_term_str, (
        f"Type checking term mismatch for {desc}:\n"
        f"  Expected term: {expected_term_str}\n"
        f"  Inferred term: {inferred_term_str}"
    )


def run_type_reduction_test(desc: str, test_case: hydra.testing.TypeReductionTestCase) -> None:
    """Execute a type reduction test."""
    # Use cached test graph - type reduction needs the graph with schema types
    graph = get_test_graph()
    cx = _empty_context()

    # beta_reduce_type now takes (cx, graph, typ) and returns Either[str, Type]
    result = hydra.reduction.beta_reduce_type(cx, graph, test_case.input)

    match result:
        case Left(value=err):
            pytest.fail(
                f"Type reduction failed for {desc}:\n"
                f"  Error: {err}\n"
                f"  Type: {hydra.show.core.type(test_case.input)}"
            )
            return
        case Right(value=reduced):
            pass

    assert reduced == test_case.output, (
        f"Type reduction mismatch for {desc}:\n"
        f"  Expected: {hydra.show.core.type(test_case.output)}\n"
        f"  Actual: {hydra.show.core.type(reduced)}"
    )


def run_normalize_type_variables_test(test_case: hydra.testing.NormalizeTypeVariablesTestCase) -> None:
    """Execute a normalize type variables test."""
    result = hydra.rewriting.normalize_type_variables_in_term(test_case.input)
    assert result == test_case.output, (
        f"Normalize type variables failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_fold_over_term_test(test_case: hydra.testing.FoldOverTermTestCase) -> None:
    """Execute a fold over term test."""
    order = test_case.traversal_order
    op = test_case.operation
    input_term = test_case.input

    match op:
        case hydra.testing.FoldOperation.SUM_INT32_LITERALS:
            def sum_int32(acc: int, t: hydra.core.Term) -> int:
                match t:
                    case hydra.core.TermLiteral(value=hydra.core.LiteralInteger(value=hydra.core.IntegerValueInt32(value=n))):
                        return acc + n
                    case _:
                        return acc
            result_val = hydra.rewriting.fold_over_term(order, sum_int32, 0, input_term)
            result = hydra.core.TermLiteral(
                hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(result_val))
            )

        case hydra.testing.FoldOperation.COLLECT_LIST_LENGTHS:
            def collect_list_lengths(acc: list, t: hydra.core.Term) -> list:
                match t:
                    case hydra.core.TermList(value=elems):
                        return acc + [len(elems)]
                    case _:
                        return acc
            result_val = hydra.rewriting.fold_over_term(order, collect_list_lengths, [], input_term)
            result = hydra.core.TermList(
                tuple(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(n))) for n in result_val)
            )

        case hydra.testing.FoldOperation.COLLECT_LABELS:
            def collect_labels(acc: list, t: hydra.core.Term) -> list:
                match t:
                    case hydra.core.TermPair(value=(hydra.core.TermLiteral(value=hydra.core.LiteralString(value=s)), _)):
                        return acc + [hydra.core.LiteralString(s)]
                    case _:
                        return acc
            result_val = hydra.rewriting.fold_over_term(order, collect_labels, [], input_term)
            result = hydra.core.TermList(
                tuple(hydra.core.TermLiteral(lit) for lit in result_val)
            )

    assert result == test_case.output, (
        f"Fold over term failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_rewrite_term_test(test_case: hydra.testing.RewriteTermTestCase) -> None:
    """Execute a rewrite term test."""
    rewriter = test_case.rewriter

    match rewriter:
        case hydra.testing.TermRewriter.REPLACE_FOO_WITH_BAR:
            def rewrite(recurse, term):
                match term:
                    case hydra.core.TermLiteral(value=hydra.core.LiteralString(value="foo")):
                        return hydra.core.TermLiteral(hydra.core.LiteralString("bar"))
                    case _:
                        return recurse(term)
            result = hydra.rewriting.rewrite_term(rewrite, test_case.input)

        case hydra.testing.TermRewriter.REPLACE_INT32_WITH_INT64:
            def rewrite(recurse, term):
                match term:
                    case hydra.core.TermLiteral(value=hydra.core.LiteralInteger(value=hydra.core.IntegerValueInt32(value=n))):
                        return hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt64(n)))
                    case _:
                        return recurse(term)
            result = hydra.rewriting.rewrite_term(rewrite, test_case.input)

    assert result == test_case.output, (
        f"Rewrite term failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_rewrite_type_test(test_case: hydra.testing.RewriteTypeTestCase) -> None:
    """Execute a rewrite type test."""
    rewriter = test_case.rewriter

    match rewriter:
        case hydra.testing.TypeRewriter.REPLACE_STRING_WITH_INT32:
            def rewrite(recurse, typ):
                match typ:
                    case hydra.core.TypeLiteral(value=hydra.core.LiteralTypeString()):
                        return hydra.core.TypeLiteral(
                            hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)
                        )
                    case _:
                        return recurse(typ)
            result = hydra.rewriting.rewrite_type(rewrite, test_case.input)

    assert result == test_case.output, (
        f"Rewrite type failed:\n"
        f"  Expected: {hydra.show.core.type(test_case.output)}\n"
        f"  Actual: {hydra.show.core.type(result)}"
    )


def run_json_decode_test(test_case: hydra.testing.JsonDecodeTestCase) -> None:
    """Execute a JSON decode test."""
    import hydra.json.decode as json_decode

    empty_types = FrozenDict({})
    decode_result = json_decode.from_json(empty_types, test_case.type, test_case.json)

    match test_case.expected:
        case Left(value=_err_msg):
            # Expected failure
            match decode_result:
                case Left(_):
                    pass  # Expected failure, got failure
                case Right(value=result):
                    pytest.fail(
                        f"Expected decode failure but got success: "
                        f"{hydra.show.core.term(result)}"
                    )
        case Right(value=expected_term):
            # Expected success
            match decode_result:
                case Left(value=err):
                    pytest.fail(f"JSON decode failed: {err}")
                case Right(value=result):
                    assert result == expected_term, (
                        f"JSON decode mismatch:\n"
                        f"  Expected: {hydra.show.core.term(expected_term)}\n"
                        f"  Actual: {hydra.show.core.term(result)}"
                    )


def run_json_encode_test(test_case: hydra.testing.JsonEncodeTestCase) -> None:
    """Execute a JSON encode test."""
    import hydra.json.encode as json_encode

    encode_result = json_encode.to_json(test_case.term)

    match test_case.expected:
        case Left(value=_err_msg):
            # Expected failure
            match encode_result:
                case Left(_):
                    pass  # Expected failure, got failure
                case Right(value=result):
                    pytest.fail(
                        f"Expected encode failure but got success: {result}"
                    )
        case Right(value=expected_json):
            # Expected success
            match encode_result:
                case Left(value=err):
                    pytest.fail(f"JSON encode failed: {err}")
                case Right(value=result):
                    assert result == expected_json, (
                        f"JSON encode mismatch:\n"
                        f"  Expected: {expected_json}\n"
                        f"  Actual: {result}"
                    )


def run_json_roundtrip_test(test_case: hydra.testing.JsonRoundtripTestCase) -> None:
    """Execute a JSON roundtrip test."""
    import hydra.json.encode as json_encode
    import hydra.json.decode as json_decode

    # Encode
    encode_result = json_encode.to_json(test_case.term)

    match encode_result:
        case Left(value=err):
            pytest.fail(f"Failed to encode term to JSON: {err}")
            return
        case Right(value=json):
            pass

    # Decode the JSON back
    empty_types = FrozenDict({})
    decode_result = json_decode.from_json(empty_types, test_case.type, json)

    match decode_result:
        case Left(value=err):
            pytest.fail(f"Failed to decode JSON back to term: {err}")
        case Right(value=decoded):
            assert decoded == test_case.term, (
                f"JSON roundtrip mismatch:\n"
                f"  Original: {hydra.show.core.term(test_case.term)}\n"
                f"  After roundtrip: {hydra.show.core.term(decoded)}"
            )


def run_hoist_subterms_test(test_case: hydra.testing.HoistSubtermsTestCase) -> None:
    """Execute a hoist subterms test."""
    empty_graph = hydra.lexical.empty_graph()

    # Build the predicate function based on the predicate type
    def predicate_fn(path_term: tuple) -> bool:
        path, term = path_term
        match test_case.predicate:
            case hydra.testing.HoistPredicate.NOTHING:
                return False
            case hydra.testing.HoistPredicate.LISTS:
                return isinstance(term, hydra.core.TermList)
            case hydra.testing.HoistPredicate.APPLICATIONS:
                return isinstance(term, hydra.core.TermApplication)
            case hydra.testing.HoistPredicate.CASE_STATEMENTS:
                # Case statements are elimination functions
                if isinstance(term, hydra.core.TermFunction):
                    return isinstance(term.value, hydra.core.FunctionElimination)
                return False

    result = hydra.hoisting.hoist_subterms(predicate_fn, empty_graph, test_case.input)

    assert result == test_case.output, (
        f"Hoist subterms failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_hoist_case_statements_test(test_case: hydra.testing.HoistCaseStatementsTestCase) -> None:
    """Execute a hoist case statements test."""
    empty_graph = hydra.lexical.empty_graph()

    result = hydra.hoisting.hoist_case_statements(empty_graph, test_case.input)

    assert result == test_case.output, (
        f"Hoist case statements failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_hoist_let_bindings_test(test_case: hydra.testing.HoistLetBindingsTestCase) -> None:
    """Execute a hoist let bindings test with hoistAll=True."""
    # hoist_all_let_bindings hoists ALL nested bindings, not just polymorphic ones
    result = hydra.hoisting.hoist_all_let_bindings(test_case.input)

    assert result == test_case.output, (
        f"Hoist let bindings failed:\n"
        f"  Expected: {repr(test_case.output)}\n"
        f"  Actual: {repr(result)}"
    )


def run_hoist_polymorphic_let_bindings_test(test_case: hydra.testing.HoistPolymorphicLetBindingsTestCase) -> None:
    """Execute a hoist polymorphic let bindings test."""
    result = hydra.hoisting.hoist_polymorphic_let_bindings(lambda _: True, test_case.input)

    assert result == test_case.output, (
        f"Hoist polymorphic let bindings failed:\n"
        f"  Expected: {repr(test_case.output)}\n"
        f"  Actual: {repr(result)}"
    )


def run_subst_in_type_test(test_case: hydra.testing.SubstInTypeTestCase) -> None:
    """Execute a type substitution test."""
    # Build TypeSubst from list of (Name, Type) pairs
    subst_map = FrozenDict({pair[0]: pair[1] for pair in test_case.substitution})
    subst = hydra.typing.TypeSubst(subst_map)
    result = hydra.substitution.subst_in_type(subst, test_case.input)
    assert result == test_case.output, (
        f"Type substitution failed:\n"
        f"  Expected: {hydra.show.core.type(test_case.output)}\n"
        f"  Actual: {hydra.show.core.type(result)}"
    )


def run_variable_occurs_in_type_test(test_case: hydra.testing.VariableOccursInTypeTestCase) -> None:
    """Execute a variable occurs in type test."""
    result = hydra.unification.variable_occurs_in_type(test_case.variable, test_case.type)
    assert result == test_case.expected, (
        f"Variable occurs in type failed:\n"
        f"  Variable: {test_case.variable}\n"
        f"  Type: {hydra.show.core.type(test_case.type)}\n"
        f"  Expected: {test_case.expected}\n"
        f"  Actual: {result}"
    )


def run_unify_types_test(test_case: hydra.testing.UnifyTypesTestCase) -> None:
    """Execute a type unification test."""
    # Build schema types map from the list of names
    # Each schema name gets a trivial type scheme (no free variables)
    schema_types = FrozenDict({
        n: hydra.core.TypeScheme(
            variables=(),
            type=hydra.core.TypeVariable(n),
            constraints=Nothing()
        )
        for n in test_case.schema_types
    })

    # Run unification — now takes Context as first param, returns Either directly
    cx = _empty_context()
    unify_result = hydra.unification.unify_types(cx, schema_types, test_case.left, test_case.right, "test")

    match test_case.expected:
        case Left(value=_err_substring):
            # Expected failure
            match unify_result:
                case Left(_):
                    pass  # Expected failure, got failure
                case Right(value=result):
                    pytest.fail(
                        f"Expected unification failure but got success: {result}"
                    )
        case Right(value=expected_subst):
            # Expected success
            match unify_result:
                case Left(value=err):
                    pytest.fail(f"Expected unification success but got failure: {err}")
                case Right(value=actual_subst):
                    assert actual_subst == expected_subst, (
                        f"Unification result mismatch:\n"
                        f"  Expected: {expected_subst}\n"
                        f"  Actual: {actual_subst}"
                    )


def run_join_types_test(test_case: hydra.testing.JoinTypesTestCase) -> None:
    """Execute a type join test."""
    # Run join — now takes Context as first param, returns Either directly
    cx = _empty_context()
    join_result = hydra.unification.join_types(cx, test_case.left, test_case.right, "test")

    match test_case.expected:
        case Left(value=_):
            # Expected failure (Left () indicates failure)
            match join_result:
                case Left(_):
                    pass  # Expected failure, got failure
                case Right(value=result):
                    pytest.fail(
                        f"Expected join failure but got success with constraints: {result}"
                    )
        case Right(value=expected_constraints):
            # Expected success
            match join_result:
                case Left(value=err):
                    pytest.fail(f"Expected join success but got failure: {err}")
                case Right(value=actual_constraints):
                    assert actual_constraints == expected_constraints, (
                        f"Join constraints mismatch:\n"
                        f"  Expected: {expected_constraints}\n"
                        f"  Actual: {actual_constraints}"
                    )


def _start_timer(path: str) -> None:
    """Record the start time for a benchmark group."""
    _benchmark_timers[path] = time.perf_counter_ns()

def _stop_timer(path: str) -> None:
    """Record the end time for a benchmark group and compute elapsed ms."""
    if path in _benchmark_timers:
        elapsed_ns = time.perf_counter_ns() - _benchmark_timers[path]
        _benchmark_results[path] = elapsed_ns / 1_000_000.0  # convert to ms


def _count_test_cases(group: hydra.testing.TestGroup, runner: TestRunner) -> tuple[int, int]:
    """Count (runnable, skipped) test cases in a group and all subgroups."""
    runnable = 0
    skipped = 0
    for tcase in group.cases:
        if should_skip_test(tcase):
            skipped += 1
        elif runner(group.name, tcase) is not None:
            runnable += 1
        else:
            skipped += 1
    for subgroup_item in group.subgroups:
        if isinstance(subgroup_item, str):
            sg = getattr(test_suite, subgroup_item)
        elif callable(subgroup_item):
            sg = subgroup_item()
        else:
            sg = subgroup_item
        r, s = _count_test_cases(sg, runner)
        runnable += r
        skipped += s
    return runnable, skipped


def _group_to_json_value(
    group: hydra.testing.TestGroup,
    parent_path: str,
    runner: TestRunner,
    results: dict[str, float]
) -> "hydra.json.model.ValueObject":
    """Convert a test group to a Hydra JSON value for benchmark output."""
    import hydra.json.model as json

    path = f"{parent_path}/{group.name}" if parent_path else group.name
    runnable, skipped = _count_test_cases(group, runner)
    time_ms = results.get(path, 0.0)

    fields = {
        "path": json.ValueString(path),
        "passed": json.ValueNumber(Decimal(runnable)),
        "failed": json.ValueNumber(Decimal(0)),
        "skipped": json.ValueNumber(Decimal(skipped)),
        "totalTimeMs": json.ValueNumber(Decimal(str(round(time_ms, 1)))),
    }

    subgroups = []
    for subgroup_item in group.subgroups:
        if isinstance(subgroup_item, str):
            sg = getattr(test_suite, subgroup_item)
        elif callable(subgroup_item):
            sg = subgroup_item()
        else:
            sg = subgroup_item
        subgroups.append(_group_to_json_value(sg, path, runner, results))

    if subgroups:
        fields["subgroups"] = json.ValueArray(tuple(subgroups))

    return json.ValueObject(FrozenDict(fields))


def _write_benchmark_json(output_path: str, runner: TestRunner) -> None:
    """Write benchmark results as JSON using Hydra's JSON writer."""
    import hydra.json.model as json
    import hydra.json.writer as json_writer

    root_group = test_suite.all_tests()
    root_path = root_group.name

    # Collect git metadata
    def git_output(args: list[str]) -> str:
        try:
            result = subprocess.run(
                ["git"] + args, capture_output=True, text=True, timeout=5)
            return result.stdout.strip()
        except Exception:
            return ""

    timestamp = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    branch = git_output(["rev-parse", "--abbrev-ref", "HEAD"])
    commit = git_output(["rev-parse", "--short", "HEAD"])
    commit_msg = git_output(["log", "-1", "--format=%s"])

    total_runnable, total_skipped = _count_test_cases(root_group, runner)
    root_time = _benchmark_results.get(root_path, 0.0)

    # Add initialization time as a separate group (not attributed to any test group)
    init_time_ms = _benchmark_results.get("common/_initialization", 0.0)

    # Build group JSON values for children of root
    group_values = []
    if init_time_ms > 0:
        group_values.append(json.ValueObject(FrozenDict({
            "path": json.ValueString("common/_initialization"),
            "passed": json.ValueNumber(Decimal(0)),
            "failed": json.ValueNumber(Decimal(0)),
            "skipped": json.ValueNumber(Decimal(0)),
            "totalTimeMs": json.ValueNumber(Decimal(str(round(init_time_ms, 1)))),
        })))
    for subgroup_item in root_group.subgroups:
        if isinstance(subgroup_item, str):
            sg = getattr(test_suite, subgroup_item)
        elif callable(subgroup_item):
            sg = subgroup_item()
        else:
            sg = subgroup_item
        group_values.append(_group_to_json_value(sg, root_path, runner, _benchmark_results))

    json_value = json.ValueObject(FrozenDict({
        "metadata": json.ValueObject(FrozenDict({
            "timestamp": json.ValueString(timestamp),
            "language": json.ValueString("python"),
            "branch": json.ValueString(branch),
            "commit": json.ValueString(commit),
            "commitMessage": json.ValueString(commit_msg),
        })),
        "groups": json.ValueArray(tuple(group_values)),
        "summary": json.ValueObject(FrozenDict({
            "totalPassed": json.ValueNumber(Decimal(total_runnable)),
            "totalFailed": json.ValueNumber(Decimal(0)),
            "totalSkipped": json.ValueNumber(Decimal(total_skipped)),
            "totalTimeMs": json.ValueNumber(Decimal(str(round(root_time, 1)))),
        })),
    }))

    json_str = json_writer.print_json(json_value)
    with open(output_path, "w") as f:
        f.write(json_str)
    print(f"Benchmark written to: {output_path}")


def run_test_case(parent_desc: str, runner: TestRunner, tcase: hydra.testing.TestCaseWithMetadata) -> None:
    """
    Run a single test case using the provided runner.

    Args:
        parent_desc: Description from parent test groups
        runner: The test runner function to use
        tcase: The test case to run
    """
    name = tcase.name
    desc_suffix = f": {tcase.description}" if tcase.description else ""
    desc = name + desc_suffix
    full_desc = f"{parent_desc}, {desc}" if parent_desc else desc

    test_fn = runner(full_desc, tcase)

    if test_fn:
        test_fn()


def run_test_group(parent_desc: str, runner: TestRunner, tgroup: hydra.testing.TestGroup) -> None:
    """
    Recursively run all tests in a test group.

    Args:
        parent_desc: Description from parent test groups
        runner: The test runner function to use
        tgroup: The test group to run
    """
    name = tgroup.name
    desc_suffix = f" ({tgroup.description})" if tgroup.description else ""
    desc = name + desc_suffix
    full_desc = f"{parent_desc}, {desc}" if parent_desc else desc

    # Run test cases in this group
    for tcase in tgroup.cases:
        run_test_case(full_desc, runner, tcase)

    # Recursively run subgroups
    for subgroup in tgroup.subgroups:
        run_test_group(full_desc, runner, subgroup)


def generate_pytest_tests(group: hydra.testing.TestGroup, runner: TestRunner, prefix: str = "", hydra_path: str = "") -> list:
    """
    Generate pytest test functions from a test group.

    This function generates individual pytest test functions for each test case,
    properly organized by test group hierarchy.

    When HYDRA_BENCHMARK_OUTPUT is set, sentinel timer functions are inserted at
    the start and end of each group to measure wall-clock time.

    Args:
        group: The test group to generate tests from
        runner: The test runner function
        prefix: Prefix for test names (built up from parent groups)
        hydra_path: The Hydra path (preserving original names for cross-language correlation)

    Returns:
        List of (test_name, hydra_path, test_function) tuples
    """
    tests = []

    # Generate a safe test name from the group name (for Python)
    safe_group_name = group.name.replace(" ", "_").replace("-", "_").lower()
    new_prefix = f"{prefix}{safe_group_name}_" if prefix else f"{safe_group_name}_"

    # Build the Hydra path (preserving original names with spaces/caps)
    new_hydra_path = f"{hydra_path}/{group.name}" if hydra_path else group.name

    # Insert timer start sentinel for benchmark mode
    if BENCHMARK_OUTPUT:
        timer_start_name = f"{new_prefix}000_TIMER_START"
        def make_start(p=new_hydra_path):
            _start_timer(p)
        tests.append((timer_start_name, new_hydra_path, make_start))

    # Generate tests for cases in this group
    for i, tcase in enumerate(group.cases, 1):
        test_name = f"{new_prefix}case_{i}"
        case_hydra_path = f"{new_hydra_path}/{tcase.name}"
        desc = f"{group.name}, {tcase.name}"

        # Check if test should be skipped based on tags
        if should_skip_test(tcase):
            def make_skip_test():
                """Create a skip test for slow/disabled tests"""
                def test_fn():
                    pytest.skip("Test is disabled or too slow")
                return test_fn
            tests.append((test_name, case_hydra_path, make_skip_test()))
            continue

        def make_test(test_desc: str, test_case: hydra.testing.TestCaseWithMetadata):
            """Closure to capture test_desc and test_case"""
            def test_fn():
                test_func = runner(test_desc, test_case)
                if test_func:
                    test_func()
                else:
                    pytest.skip("Test is disabled or not supported")
            return test_fn

        tests.append((test_name, case_hydra_path, make_test(desc, tcase)))

    # Recursively generate tests for subgroups
    # Note: subgroups can be strings (names to look up), TestGroup objects directly, or functions that return TestGroups
    for subgroup_item in group.subgroups:
        if isinstance(subgroup_item, str):
            # It's a string name, look it up in test_suite module
            subgroup_obj = getattr(test_suite, subgroup_item)
        elif callable(subgroup_item):
            # It's a function that returns a TestGroup - call it
            subgroup_obj = subgroup_item()
        else:
            # It's already a TestGroup object
            subgroup_obj = subgroup_item
        tests.extend(generate_pytest_tests(subgroup_obj, runner, new_prefix, new_hydra_path))

    # Insert timer stop sentinel for benchmark mode
    if BENCHMARK_OUTPUT:
        timer_stop_name = f"{new_prefix}999_TIMER_END"
        def make_stop(p=new_hydra_path):
            _stop_timer(p)
        tests.append((timer_stop_name, new_hydra_path, make_stop))

    return tests


# Generate all test functions from the test suite
_all_tests = generate_pytest_tests(test_suite.all_tests(), default_test_runner)

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
    atexit.register(_write_benchmark_json, BENCHMARK_OUTPUT, default_test_runner)
