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

import hydra.compute
import hydra.core
import hydra.formatting
import hydra.graph
import hydra.inference
import hydra.lib.flows
# import hydra.meta
import hydra.util
import hydra.monads
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.sources.libraries
import hydra.testing
import hydra.typing
from hydra.dsl.python import Just, Left, Nothing, Right

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
    if is_disabled(tcase):
        return None

    case = tcase.case

    match case:
        case hydra.testing.TestCaseCaseConversion(value=cc):
            return lambda: run_case_conversion_test(cc)

        case hydra.testing.TestCaseEvaluation(value=ev):
            # Skip evaluation tests for now - they require term evaluation support
            return None

        case hydra.testing.TestCaseInference(value=inf):
            return lambda: run_inference_test(desc, inf)

        case hydra.testing.TestCaseInferenceFailure(value=inf_fail):
            return lambda: run_inference_failure_test(desc, inf_fail)

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
