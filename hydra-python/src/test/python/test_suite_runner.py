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
import hydra.hoisting
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


def _load_kernel_term_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """
    Load kernel term bindings from the generated sources modules.

    This loads bindings from hydra.sources.* modules which contain term-encoded
    Module objects. These are the kernel term modules (hydra.monads, hydra.annotations,
    etc.) that are needed for evaluation tests.

    Returns:
        Dictionary mapping binding names to Binding objects
    """
    bindings = {}

    # Import the kernel term sources modules
    # These match kernelPrimaryTermsModules in Hydra.Sources.Kernel.Terms.All
    kernel_term_source_modules = [
        # Primary term modules
        "hydra.sources.adapt.literals",
        "hydra.sources.adapt.modules",
        "hydra.sources.adapt.simple",
        "hydra.sources.adapt.terms",
        "hydra.sources.adapt.utils",
        "hydra.sources.annotations",
        "hydra.sources.arity",
        "hydra.sources.checking",
        "hydra.sources.constants",
        "hydra.sources.decoding",
        "hydra.sources.encoding",
        "hydra.sources.extract.core",
        "hydra.sources.extract.helpers",
        "hydra.sources.extract.util",
        "hydra.sources.formatting",
        "hydra.sources.grammars",
        "hydra.sources.inference",
        "hydra.sources.languages",
        "hydra.sources.lexical",
        "hydra.sources.literals",
        "hydra.sources.monads",
        "hydra.sources.names",
        "hydra.sources.parsers",
        "hydra.sources.reduction",
        "hydra.sources.reflect",
        "hydra.sources.rewriting",
        "hydra.sources.schemas",
        "hydra.sources.serialization",
        "hydra.sources.show.accessors",
        "hydra.sources.show.core",
        "hydra.sources.show.graph",
        "hydra.sources.show.meta",
        "hydra.sources.show.typing",
        "hydra.sources.show.util",
        "hydra.sources.sorting",
        "hydra.sources.substitution",
        "hydra.sources.tarjan",
        "hydra.sources.templates",
        "hydra.sources.unification",
    ]

    for module_name in kernel_term_source_modules:
        try:
            # Dynamically import the module
            import importlib
            source_module = importlib.import_module(module_name)

            # Call module() to get the Module object
            mod = source_module.module()

            # Extract bindings from the module
            for binding in mod.elements:
                bindings[binding.name] = binding

        except ImportError as e:
            # Module not yet generated - skip silently
            pass
        except Exception as e:
            # Log but don't fail - allows tests to run with partial kernel
            import sys
            print(f"Warning: Failed to load {module_name}: {e}", file=sys.stderr)

    return bindings


def _load_kernel_type_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """
    Load kernel type bindings from the generated sources modules.

    This loads bindings from hydra.sources.* modules which contain the type-level
    kernel definitions (hydra.core.Type, hydra.core.Name, etc.). These are added
    to the schema graph so that inference tests can reference kernel types.

    This mirrors how Haskell includes kernelTypesModules in testSchemaGraph:
        kernelElements = L.concat $ fmap moduleElements kernelTypesModules

    Returns:
        Dictionary mapping binding names to Binding objects
    """
    bindings = {}

    # Import the kernel type sources modules
    # These match kernelTypesModules in Hydra.Sources.Kernel.Types.All
    kernel_type_source_modules = [
        "hydra.sources.accessors",
        "hydra.sources.ast",
        "hydra.sources.classes",
        "hydra.sources.coders",
        "hydra.sources.compute",
        "hydra.sources.constraints",
        "hydra.sources.core",       # Contains hydra.core.Type, hydra.core.Name, etc.
        "hydra.sources.grammar",
        "hydra.sources.graph",
        "hydra.sources.json.model",
        "hydra.sources.module",
        "hydra.sources.parsing",
        "hydra.sources.phantoms",
        "hydra.sources.query",
        "hydra.sources.relational",
        "hydra.sources.tabular",
        "hydra.sources.testing",
        "hydra.sources.topology",
        "hydra.sources.typing",
        "hydra.sources.util",
        "hydra.sources.variants",
        "hydra.sources.workflow",
    ]

    for module_name in kernel_type_source_modules:
        try:
            # Dynamically import the module
            import importlib
            source_module = importlib.import_module(module_name)

            # Call module() to get the Module object
            mod = source_module.module()

            # Extract bindings from the module
            for binding in mod.elements:
                bindings[binding.name] = binding

        except ImportError as e:
            # Module not yet generated - skip silently
            pass
        except Exception as e:
            # Log but don't fail - allows tests to run with partial kernel
            import sys
            print(f"Warning: Failed to load {module_name}: {e}", file=sys.stderr)

    return bindings


def is_disabled(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case is marked as disabled."""
    disabled_tag = hydra.testing.Tag("disabled")
    return disabled_tag in tcase.tags

def is_disabled_for_python(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case is marked as disabled for Python.

    These are tests that are too slow in Python (typically type inference tests
    with complex polymorphism) and should be skipped until performance is improved.
    """
    disabled_for_python_tag = hydra.testing.Tag("disabledForPython")
    return disabled_for_python_tag in tcase.tags

def requires_flow_decoding(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case requires decoding Flow values back to Terms.

    These tests fail because when primitives like flows.pure are evaluated,
    they create native Flow objects that cannot be decoded back to Term
    representations. This is a fundamental limitation in both Haskell and Python.
    """
    requires_flow_decoding_tag = hydra.testing.Tag("requiresFlowDecoding")
    return requires_flow_decoding_tag in tcase.tags

import os

# Environment variable to force running slow tests (disabledForPython)
# Set HYDRA_RUN_SLOW_TESTS=1 to run these tests
RUN_SLOW_TESTS = os.environ.get("HYDRA_RUN_SLOW_TESTS", "0") == "1"

def should_skip_test(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    """Check if a test case should be skipped.

    Skip tests that are:
    - disabled: explicitly marked as not working
    - disabledForPython: causes RecursionError in Python
      unless HYDRA_RUN_SLOW_TESTS=1 is set
    - requiresFlowDecoding: requires decoding Flow values back to Terms
      (unsupported in both Haskell and Python)
    """
    if is_disabled(tcase):
        return True
    if is_disabled_for_python(tcase) and not RUN_SLOW_TESTS:
        return True
    if requires_flow_decoding(tcase):
        return True
    return False


def build_test_graph() -> hydra.graph.Graph:
    """
    Build the test graph with schema and primitives.

    This mirrors the Haskell testGraph setup which includes:
    - Test types (LatLon, Person, etc.) in a schema graph
    - Test terms (testDataArthur, etc.) as elements
    - Standard library primitives (hydra.lib.*)

    Returns:
        Graph: The test graph
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
            type=hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type")),
            constraints=Nothing()
        )
        type_bindings[name] = hydra.core.Binding(
            name=name,
            term=type_term,
            type=Just(type_scheme)
        )

    # Add additional types needed by tests that aren't in test_types
    # hydra.compute.Coder is needed by eta_expansion tests
    coder_name = hydra.core.Name("hydra.compute.Coder")
    # Coder<s1, s2, v1, v2> is a record with:
    #   encode: v1 -> Flow s1 v2
    #   decode: v2 -> Flow s2 v1
    s1 = hydra.core.TypeVariable(hydra.core.Name("s1"))
    s2 = hydra.core.TypeVariable(hydra.core.Name("s2"))
    v1 = hydra.core.TypeVariable(hydra.core.Name("v1"))
    v2 = hydra.core.TypeVariable(hydra.core.Name("v2"))
    flow_name = hydra.core.Name("hydra.compute.Flow")

    # Flow s a = s -> FlowState s a  (simplified as application)
    flow_s1_v2 = hydra.core.TypeApplication(hydra.core.ApplicationType(
        hydra.core.TypeApplication(hydra.core.ApplicationType(
            hydra.core.TypeVariable(flow_name), s1
        )), v2
    ))
    flow_s2_v1 = hydra.core.TypeApplication(hydra.core.ApplicationType(
        hydra.core.TypeApplication(hydra.core.ApplicationType(
            hydra.core.TypeVariable(flow_name), s2
        )), v1
    ))

    encode_type = hydra.core.TypeFunction(hydra.core.FunctionType(v1, flow_s1_v2))
    decode_type = hydra.core.TypeFunction(hydra.core.FunctionType(v2, flow_s2_v1))

    coder_record = hydra.core.TypeRecord(hydra.core.RowType(
        coder_name,
        (
            hydra.core.FieldType(hydra.core.Name("encode"), encode_type),
            hydra.core.FieldType(hydra.core.Name("decode"), decode_type),
        )
    ))

    # Wrap in forall for all 4 type variables
    coder_type = hydra.core.TypeForall(hydra.core.ForallType(
        hydra.core.Name("s1"),
        hydra.core.TypeForall(hydra.core.ForallType(
            hydra.core.Name("s2"),
            hydra.core.TypeForall(hydra.core.ForallType(
                hydra.core.Name("v1"),
                hydra.core.TypeForall(hydra.core.ForallType(
                    hydra.core.Name("v2"),
                    coder_record
                ))
            ))
        ))
    ))

    coder_term = hydra.encode.core.type(coder_type)
    coder_type_scheme = hydra.core.TypeScheme(
        variables=(),
        type=hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type")),
        constraints=Nothing()
    )
    type_bindings[coder_name] = hydra.core.Binding(
        name=coder_name,
        term=coder_term,
        type=Just(coder_type_scheme)
    )

    # hydra.coders.CoderDirection is an enum with encode and decode variants
    coder_direction_name = hydra.core.Name("hydra.coders.CoderDirection")
    coder_direction_type = hydra.core.TypeUnion(hydra.core.RowType(
        coder_direction_name,
        (
            hydra.core.FieldType(hydra.core.Name("encode"), hydra.core.TypeUnit()),
            hydra.core.FieldType(hydra.core.Name("decode"), hydra.core.TypeUnit()),
        )
    ))
    coder_direction_term = hydra.encode.core.type(coder_direction_type)
    type_bindings[coder_direction_name] = hydra.core.Binding(
        name=coder_direction_name,
        term=coder_direction_term,
        type=Just(coder_type_scheme)  # Reuse the same scheme for Type
    )

    # Load kernel type bindings (hydra.core.Type, hydra.core.Name, etc.)
    # This mirrors how Haskell includes kernelTypesModules in testSchemaGraph:
    #   kernelElements = L.concat $ fmap moduleElements kernelTypesModules
    kernel_types = _load_kernel_type_bindings()

    # Merge test types with kernel types (test types take precedence)
    all_type_bindings = {**kernel_types, **type_bindings}

    # Create the schema graph with test types and kernel types
    # Note: elements is now a list of Bindings, not a Map Name Binding
    schema_graph = hydra.graph.Graph(
        elements=tuple(all_type_bindings.values()),
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

    # Add kernel term bindings (hydra.monads.*, hydra.annotations.*, etc.)
    # This mirrors how Haskell includes kernelTermsModules in testGraph
    # Load from generated sources modules (hydra/sources/*.py)
    kernel_terms = _load_kernel_term_bindings()
    term_bindings.update(kernel_terms)

    # Get standard library primitives (hydra.lib.*)
    # This mirrors how Haskell includes kernelTermsModules in testGraph
    primitives = hydra.sources.libraries.standard_library()

    # Create the main test graph with schema and primitives
    # Note: elements is now a list of Bindings, not a Map Name Binding
    return hydra.graph.Graph(
        elements=tuple(term_bindings.values()),
        environment=FrozenDict({}),
        types=FrozenDict({}),
        body=hydra.core.TermLiteral(hydra.core.LiteralString("test")),
        primitives=FrozenDict(primitives),
        schema=Just(schema_graph)
    )


# Cache the test graph, inference context, and type context at module level.
# This mirrors the Haskell approach where cx is computed once and reused,
# avoiding the ~19 second overhead per test of rebuilding the inference context.
_test_graph: Optional[hydra.graph.Graph] = None
_inference_context: Optional[hydra.typing.InferenceContext] = None
_type_context: Optional[hydra.typing.TypeContext] = None


def get_test_graph() -> hydra.graph.Graph:
    """Get the cached test graph, building it if necessary."""
    global _test_graph
    if _test_graph is None:
        _test_graph = build_test_graph()
    return _test_graph


def get_inference_context() -> hydra.typing.InferenceContext:
    """
    Get the cached inference context, building it if necessary.

    This is the key performance optimization: in Haskell, the inference context
    is computed once at module level (line 135 of TestSuiteSpec.hs):
        cx = fromFlow emptyInferenceContext () $ graphToInferenceContext testGraph

    Without caching, each test was rebuilding the inference context from scratch,
    which involves processing the entire schema graph - taking ~19 seconds per test.
    """
    global _inference_context
    if _inference_context is None:
        graph = get_test_graph()
        cx_flow = hydra.schemas.graph_to_inference_context(graph)
        cx_state = cx_flow.value(None, hydra.monads.empty_trace())

        match cx_state.value:
            case Nothing():
                errors = "\n".join(cx_state.trace.messages)
                raise RuntimeError(f"Failed to create inference context:\n{errors}")
            case Just(value=cx):
                _inference_context = cx
    return _inference_context


def get_type_context() -> hydra.typing.TypeContext:
    """
    Get the cached type context, building it if necessary.

    This is used for typed eta expansion tests. In Haskell:
        tx <- graphToTypeContext testGraph
    """
    global _type_context
    if _type_context is None:
        graph = get_test_graph()
        tx_flow = hydra.schemas.graph_to_type_context(graph)
        tx_state = tx_flow.value(graph, hydra.monads.empty_trace())

        match tx_state.value:
            case Nothing():
                errors = "\n".join(tx_state.trace.messages)
                raise RuntimeError(f"Failed to create type context:\n{errors}")
            case Just(value=tx):
                _type_context = tx
    return _type_context


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

    # Get cached inference context (this is the key optimization)
    cx = get_inference_context()

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

    # Get cached inference context (this is the key optimization)
    cx = get_inference_context()

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
    """Execute an eta expansion test.

    Note: Uses eta_expand_typed_term (not eta_expand_term) to match Haskell behavior.
    The typed version uses type information to avoid expanding bare primitives at top level.
    """
    # Use cached test graph and type context
    graph = get_test_graph()
    tx = get_type_context()

    # Run the typed eta expansion (matches Haskell's etaExpandTypedTerm)
    flow_result = hydra.reduction.eta_expand_typed_term(tx, test_case.input)
    flow_state = flow_result.value(graph, hydra.compute.Trace((), (), hydra.lib.maps.empty()))

    match flow_state.value:
        case Nothing():
            messages = flow_state.trace.messages
            error_msg = "; ".join(messages) if messages else "unknown error"
            pytest.fail(f"Eta expansion failed for {desc}: {error_msg}")
        case Just(value=result):
            pass
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
    # Use cached test graph
    graph = get_test_graph()

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
    # Get cached inference context (this is the key optimization)
    cx = get_inference_context()

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
    # Convert the TypeScheme to a Type with forall quantifiers for comparison
    expected_type_str = hydra.show.core.type(test_case.output_type)
    inferred_type = hydra.schemas.type_scheme_to_f_type(inferred_scheme)
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
    # Use cached test graph - type reduction needs the schema graph
    graph = get_test_graph()
    # Type reduction runs with the schema graph as context
    schema_graph = graph.schema.value if isinstance(graph.schema, Just) else graph

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


def run_json_decode_test(test_case: hydra.testing.JsonDecodeTestCase) -> None:
    """Execute a JSON decode test."""
    import hydra.ext.org.json.coder as json_coder

    graph = get_test_graph()

    # Create a JSON coder for the specified type
    coder_flow = json_coder.json_coder(test_case.type)
    coder_state = coder_flow.value(graph, hydra.monads.empty_trace())

    match coder_state.value:
        case Nothing():
            errors = "\n".join(coder_state.trace.messages)
            pytest.fail(f"Failed to create JSON coder: {errors}")
            return
        case Just(value=coder):
            pass

    # Try to decode the JSON
    decode_flow = coder.decode(test_case.json)
    decode_state = decode_flow.value(graph, hydra.monads.empty_trace())

    match test_case.expected:
        case Left(value=_err_msg):
            # Expected failure
            match decode_state.value:
                case Nothing():
                    pass  # Expected failure, got failure
                case Just(value=result):
                    pytest.fail(
                        f"Expected decode failure but got success: "
                        f"{hydra.show.core.term(result)}"
                    )
        case Right(value=expected_term):
            # Expected success
            match decode_state.value:
                case Nothing():
                    errors = "\n".join(decode_state.trace.messages)
                    pytest.fail(f"JSON decode failed: {errors}")
                case Just(value=result):
                    assert result == expected_term, (
                        f"JSON decode mismatch:\n"
                        f"  Expected: {hydra.show.core.term(expected_term)}\n"
                        f"  Actual: {hydra.show.core.term(result)}"
                    )


def run_json_encode_test(test_case: hydra.testing.JsonEncodeTestCase) -> None:
    """Execute a JSON encode test.

    Note: Encoding without a type is challenging since we need a coder.
    For now, we skip these tests as they require type information.
    """
    # Without a type in the test case, we can't create a coder
    # This matches the Haskell behavior which marks these as pending
    pytest.skip("JSON encode tests require type information to create coder")


def run_json_roundtrip_test(test_case: hydra.testing.JsonRoundtripTestCase) -> None:
    """Execute a JSON roundtrip test."""
    import hydra.ext.org.json.coder as json_coder

    graph = get_test_graph()

    # Create a JSON coder for the specified type
    coder_flow = json_coder.json_coder(test_case.type)
    coder_state = coder_flow.value(graph, hydra.monads.empty_trace())

    match coder_state.value:
        case Nothing():
            errors = "\n".join(coder_state.trace.messages)
            pytest.fail(f"Failed to create JSON coder: {errors}")
            return
        case Just(value=coder):
            pass

    # Encode the term
    encode_flow = coder.encode(test_case.term)
    encode_state = encode_flow.value(graph, hydra.monads.empty_trace())

    match encode_state.value:
        case Nothing():
            errors = "\n".join(encode_state.trace.messages)
            pytest.fail(f"Failed to encode term to JSON: {errors}")
            return
        case Just(value=json):
            pass

    # Decode the JSON back
    decode_flow = coder.decode(json)
    decode_state = decode_flow.value(graph, hydra.monads.empty_trace())

    match decode_state.value:
        case Nothing():
            errors = "\n".join(decode_state.trace.messages)
            pytest.fail(f"Failed to decode JSON back to term: {errors}")
        case Just(value=decoded):
            assert decoded == test_case.term, (
                f"JSON roundtrip mismatch:\n"
                f"  Original: {hydra.show.core.term(test_case.term)}\n"
                f"  After roundtrip: {hydra.show.core.term(decoded)}"
            )


def run_hoist_subterms_test(test_case: hydra.testing.HoistSubtermsTestCase) -> None:
    """Execute a hoist subterms test."""
    # Create an empty type context (matches Haskell's emptyTypeContext)
    empty_inference_context = hydra.typing.InferenceContext(
        FrozenDict({}), FrozenDict({}), FrozenDict({}), FrozenDict({}), False
    )
    empty_type_context = hydra.typing.TypeContext(
        FrozenDict({}), FrozenDict({}), frozenset(), frozenset(), frozenset(), empty_inference_context
    )

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

    result = hydra.hoisting.hoist_subterms(predicate_fn, empty_type_context, test_case.input)

    assert result == test_case.output, (
        f"Hoist subterms failed:\n"
        f"  Expected: {hydra.show.core.term(test_case.output)}\n"
        f"  Actual: {hydra.show.core.term(result)}"
    )


def run_hoist_case_statements_test(test_case: hydra.testing.HoistCaseStatementsTestCase) -> None:
    """Execute a hoist case statements test."""
    # Create an empty type context (matches Haskell's emptyTypeContext)
    empty_inference_context = hydra.typing.InferenceContext(
        FrozenDict({}), FrozenDict({}), FrozenDict({}), FrozenDict({}), False
    )
    empty_type_context = hydra.typing.TypeContext(
        FrozenDict({}), FrozenDict({}), frozenset(), frozenset(), frozenset(), empty_inference_context
    )

    result = hydra.hoisting.hoist_case_statements(empty_type_context, test_case.input)

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

    return tests


# Generate all test functions from the test suite
_all_tests = generate_pytest_tests(test_suite.all_tests(), default_test_runner)

# Build a mapping from Python test names to Hydra paths for cross-language benchmarking
# This can be imported by benchmark tools to correlate test results across implementations
HYDRA_PATH_MAP: dict[str, str] = {f"test_{name}": path for name, path, _ in _all_tests}

# Dynamically add test functions to module namespace for pytest discovery
for test_name, hydra_path, test_fn in _all_tests:
    globals()[f"test_{test_name}"] = test_fn
