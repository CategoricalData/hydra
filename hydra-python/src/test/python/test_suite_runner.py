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
import hydra.lib.flows
import hydra.reduction
import hydra.serialization
import hydra.sorting
import hydra.util
import hydra.monads
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.sources.libraries
import hydra.testing
import hydra.typing
from hydra.dsl.python import FrozenDict, Just, Left, Nothing, Right

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


def is_disabled(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case is marked as disabled."""
    disabled_tag = hydra.testing.Tag("disabled")
    return disabled_tag in tcase.tags


def is_requires_interp(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case requires interpreter primitives."""
    requires_interp_tag = hydra.testing.Tag("requiresInterp")
    return requires_interp_tag in tcase.tags


def should_skip_test(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case should be skipped.

    Only skip tests that are:
    - disabled: explicitly marked as not working
    - requiresInterp: requires interpreter primitives not available in Python
    """
    return is_disabled(tcase) or is_requires_interp(tcase)


def graph_to_inference_context() -> hydra.compute.Flow[None, hydra.typing.InferenceContext]:
    """
    Create an inference context from the test graph.

    This mirrors the Haskell testGraph setup which includes:
    - Test types (LatLon, Person, etc.) in a schema graph
    - Test terms (testDataArthur, etc.) as elements
    - Standard library primitives (hydra.lib.*)

    Returns:
        Flow[None, InferenceContext]: A flow that produces an inference context
    """
    from hydra.dsl.python import FrozenDict, Nothing, Just
    import hydra.encode.core

    # Get the test types from test_graph (e.g., LatLon, Person, etc.)
    test_types_dict = test_graph.test_types()

    # Build type bindings for the schema graph
    # Each type becomes a Binding with the type encoded as a term
    type_bindings = {}
    for name, typ in test_types_dict.items():
        # Encode the type as a term (like Haskell's EncodeCore.type_)
        type_term = hydra.encode.core.type(typ)
        # Create a binding with type scheme for Type
        type_scheme = hydra.core.TypeScheme(
            variables=(),
            type=hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))
        )
        type_bindings[name] = hydra.core.Binding(
            name=name,
            term=type_term,
            type=Just(type_scheme)
        )

    # Create the schema graph with test types
    schema_graph = hydra.graph.Graph(
        elements=FrozenDict(type_bindings),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("schema")),
        primitives=FrozenDict({}),
        schema=Nothing()
    )

    # Get test terms from test_graph
    test_terms_dict = test_graph.test_terms()

    # Build term bindings for the main graph
    term_bindings = {}
    for name, term in test_terms_dict.items():
        term_bindings[name] = hydra.core.Binding(
            name=name,
            term=term,
            type=Nothing()
        )

    # Get standard library primitives (hydra.lib.*)
    # This mirrors how Haskell includes kernelTermsModules in testGraph
    primitives = hydra.sources.libraries.standard_library()

    # Create the main test graph with schema and primitives
    graph = hydra.graph.Graph(
        elements=FrozenDict(term_bindings),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("test")),
        primitives=FrozenDict(primitives),
        schema=Just(schema_graph)
    )
    return hydra.schemas.graph_to_inference_context(graph)


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

        case hydra.testing.TestCaseJsonCoder(value=tc):
            # JSON coder tests require JSON coder module - skip for now
            return None

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

    return None


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

    # Create inference context
    cx_flow = graph_to_inference_context()
    cx_state = cx_flow.value(None, hydra.monads.empty_trace())

    match cx_state.value:
        case Nothing():
            # Extract error messages from trace
            errors = "\n".join(cx_state.trace.messages)
            pytest.fail(f"Failed to create inference context:\n{errors}")
            return
        case Just(value=cx):
            pass

    # Infer the type
    result_flow = hydra.inference.infer_type_of(cx, input_term)
    result_state = result_flow.value(None, hydra.monads.empty_trace())

    match result_state.value:
        case Nothing():
            # Extract error messages from trace
            errors = "\n".join(result_state.trace.messages)
            pytest.fail(
                f"Type inference failed for {desc}:\n"
                f"  Errors: {errors}\n"
                f"  Term: {hydra.show.core.term(input_term)}"
            )
            return
        case Just(value=(inferred_term, inferred_scheme)):
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

    # Create inference context
    cx_flow = graph_to_inference_context()
    cx_state = cx_flow.value(None, hydra.monads.empty_trace())

    match cx_state.value:
        case Nothing():
            # Extract error messages from trace
            errors = "\n".join(cx_state.trace.messages)
            pytest.fail(f"Failed to create inference context:\n{errors}")
            return
        case Just(value=cx):
            pass

    # Try to infer the type
    result_flow = hydra.inference.infer_type_of(cx, input_term)
    result_state = result_flow.value(None, hydra.monads.empty_trace())

    match result_state.value:
        case Nothing():
            # Expected failure - test passes
            pass
        case Just(value=(_, inferred_scheme)):
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
    """Execute an eta expansion test."""
    # Get test graph for eta expansion
    from hydra.dsl.python import FrozenDict
    test_types_dict = test_graph.test_types()
    test_terms_dict = test_graph.test_terms()
    primitives = hydra.sources.libraries.standard_library()

    term_bindings = {}
    for name, term in test_terms_dict.items():
        term_bindings[name] = hydra.core.Binding(
            name=name,
            term=term,
            type=Nothing()
        )

    graph = hydra.graph.Graph(
        elements=FrozenDict(term_bindings),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("test")),
        primitives=FrozenDict(primitives),
        schema=Nothing()
    )

    result = hydra.reduction.eta_expand_term(graph, test_case.input)
    assert result == test_case.output, (
        f"Eta expansion failed for {desc}:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
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
    # Get test graph for evaluation
    test_types_dict = test_graph.test_types()
    test_terms_dict = test_graph.test_terms()
    primitives = hydra.sources.libraries.standard_library()

    term_bindings = {}
    for name, term in test_terms_dict.items():
        term_bindings[name] = hydra.core.Binding(
            name=name,
            term=term,
            type=Nothing()
        )

    graph = hydra.graph.Graph(
        elements=FrozenDict(term_bindings),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("test")),
        primitives=FrozenDict(primitives),
        schema=Nothing()
    )

    # Determine if eager or lazy evaluation
    eager = test_case.evaluation_style == hydra.testing.EvaluationStyle.EAGER

    result_flow = hydra.reduction.reduce_term(eager, test_case.input)
    result_state = result_flow.value(graph, hydra.monads.empty_trace())

    match result_state.value:
        case Nothing():
            errors = "\n".join(result_state.trace.messages)
            pytest.fail(
                f"Evaluation failed for {desc}:\n"
                f"  Errors: {errors}\n"
                f"  Term: {hydra.show.core.term(test_case.input)}"
            )
            return
        case Just(value=result):
            pass

    assert result == test_case.output, (
        f"Evaluation mismatch for {desc}:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
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
    # Create inference context
    cx_flow = graph_to_inference_context()
    cx_state = cx_flow.value(None, hydra.monads.empty_trace())

    match cx_state.value:
        case Nothing():
            errors = "\n".join(cx_state.trace.messages)
            pytest.fail(f"Failed to create inference context:\n{errors}")
            return
        case Just(value=cx):
            pass

    # Infer the type
    result_flow = hydra.inference.infer_type_of(cx, test_case.input)
    result_state = result_flow.value(None, hydra.monads.empty_trace())

    match result_state.value:
        case Nothing():
            errors = "\n".join(result_state.trace.messages)
            pytest.fail(
                f"Type checking failed for {desc}:\n"
                f"  Errors: {errors}\n"
                f"  Term: {hydra.show.core.term(test_case.input)}"
            )
            return
        case Just(value=(inferred_term, inferred_scheme)):
            pass

    # Compare the inferred type with expected output type
    expected_type_str = hydra.show.core.type(test_case.output_type)
    inferred_type_str = hydra.show.core.type(inferred_scheme.type)

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
    # Get test graph for type reduction
    test_types_dict = test_graph.test_types()
    test_terms_dict = test_graph.test_terms()
    primitives = hydra.sources.libraries.standard_library()

    # Build schema graph
    type_bindings = {}
    for name, typ in test_types_dict.items():
        type_term = hydra.encode.core.type(typ)
        type_scheme = hydra.core.TypeScheme(
            variables=(),
            type=hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))
        )
        type_bindings[name] = hydra.core.Binding(
            name=name,
            term=type_term,
            type=Just(type_scheme)
        )

    schema_graph = hydra.graph.Graph(
        elements=FrozenDict(type_bindings),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("schema")),
        primitives=FrozenDict({}),
        schema=Nothing()
    )

    result_flow = hydra.reduction.beta_reduce_type(test_case.input)
    result_state = result_flow.value(schema_graph, hydra.monads.empty_trace())

    match result_state.value:
        case Nothing():
            errors = "\n".join(result_state.trace.messages)
            pytest.fail(
                f"Type reduction failed for {desc}:\n"
                f"  Errors: {errors}\n"
                f"  Type: {hydra.show.core.type(test_case.input)}"
            )
            return
        case Just(value=result):
            pass

    assert result == test_case.output, (
        f"Type reduction mismatch for {desc}:\n"
        f"  Expected: {hydra.show.core.type(test_case.output)}\n"
        f"  Actual: {hydra.show.core.type(result)}"
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


def generate_pytest_tests(group: hydra.testing.TestGroup, runner: TestRunner, prefix: str = "") -> list:
    """
    Generate pytest test functions from a test group.

    This function generates individual pytest test functions for each test case,
    properly organized by test group hierarchy.

    Args:
        group: The test group to generate tests from
        runner: The test runner function
        prefix: Prefix for test names (built up from parent groups)

    Returns:
        List of (test_name, test_function) tuples
    """
    tests = []

    # Generate a safe test name from the group name
    safe_group_name = group.name.replace(" ", "_").replace("-", "_").lower()
    new_prefix = f"{prefix}{safe_group_name}_" if prefix else f"{safe_group_name}_"

    # Generate tests for cases in this group
    for i, tcase in enumerate(group.cases, 1):
        test_name = f"{new_prefix}case_{i}"
        desc = f"{group.name}, {tcase.name}"

        def make_test(test_desc: str, test_case: hydra.testing.TestCaseWithMetadata):
            """Closure to capture test_desc and test_case"""
            def test_fn():
                test_func = runner(test_desc, test_case)
                if test_func:
                    test_func()
                else:
                    pytest.skip("Test is disabled or not supported")
            return test_fn

        tests.append((test_name, make_test(desc, tcase)))

    # Recursively generate tests for subgroups
    # Note: subgroups can be either strings (names to look up) or TestGroup objects directly
    for subgroup_item in group.subgroups:
        if isinstance(subgroup_item, str):
            # It's a string name, look it up in test_suite module
            subgroup_obj = getattr(test_suite, subgroup_item)
        else:
            # It's already a TestGroup object
            subgroup_obj = subgroup_item
        tests.extend(generate_pytest_tests(subgroup_obj, runner, new_prefix))

    return tests


# Generate all test functions from the test suite
_all_tests = generate_pytest_tests(test_suite.all_tests(), default_test_runner)

# Dynamically add test functions to module namespace for pytest discovery
for test_name, test_fn in _all_tests:
    globals()[f"test_{test_name}"] = test_fn
