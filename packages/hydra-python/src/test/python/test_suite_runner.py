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

from typing import Callable, Optional
import pytest

import hydra.core
import hydra.graph
import hydra.lexical
import hydra.rewriting
import hydra.testing
from hydra.dsl.python import FrozenDict, Nothing
import hydra.context

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

    json_dir = "../hydra-kernel/src/gen-main/json"

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
        hydra.core.Name("hydra.show.core"),
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
    bindings = {}
    for mod in term_mods:
        for d in mod.definitions:
            if isinstance(d, DefinitionTerm):
                td = d.value
                bindings[td.name] = Binding(td.name, td.term, td.type)

    return bindings


def _load_bootstrap_type_schemes() -> FrozenDict:
    """
    Load bootstrap type schemes for the test schema graph.

    Uses hydra.json.bootstrap.types_by_name (the same bootstrap type map
    used for JSON decoding) to build a Map[Name, TypeScheme] suitable for
    the test graph's schema_types. This provides type definitions for
    hydra.core, hydra.util, hydra.context, hydra.error, hydra.graph,
    and hydra.module — all the types needed by inference tests.

    This mirrors Java's Generation.bootstrapTypeSchemes().
    """
    from hydra.json.bootstrap import types_by_name
    from hydra.scoping import f_type_to_type_scheme

    result = {}
    for name, typ in types_by_name.items():
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
    # The bootstrap type schemes provide types for hydra.core, hydra.util,
    # hydra.context, hydra.error, hydra.graph, and hydra.module.
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

    All test cases are now UniversalTestCase (string comparison).

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
        case hydra.testing.TestCaseUniversal(value=tc):
            expected, actual = tc.expected, tc.actual
            def run_universal():
                if actual != expected:
                    raise AssertionError(f"expected {expected!r} but got {actual!r}")
            return run_universal

        case _:
            # Fail on unhandled test case types to catch missing implementations
            case_type = type(tcase.case).__name__
            def fail_unhandled():
                pytest.fail(f"Unhandled test case type: {case_type}")
            return fail_unhandled


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
