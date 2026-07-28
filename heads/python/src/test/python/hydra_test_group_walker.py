"""
Generic pytest test-generation walker over a Hydra TestGroup tree, shared by every
per-package test-suite runner (hydra-kernel's test_suite_runner.py, hydra-build's
build_test_suite_runner.py, and any future package's runner — #547). Handles universal
and effectful test cases, skip tags, timeouts (via the caller's pytest wrapping), and
optional benchmark JSON output. Does not build any test graph or primitive environment —
that is package-specific and stays in each caller.
"""

from __future__ import annotations

import time
from decimal import Decimal
from typing import Callable, Optional

import pytest

import hydra.testing

TestRunner = Callable[[str, hydra.testing.TestCaseWithMetadata], Optional[Callable[[], None]]]


def is_disabled(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    disabled_tag = hydra.testing.Tag("disabled")
    return disabled_tag in tcase.tags


def should_skip_test(tcase: hydra.testing.TestCaseWithMetadata) -> bool:
    return is_disabled(tcase)


def _resolve_subgroup(subgroup_item, suite_module):
    if isinstance(subgroup_item, str):
        return getattr(suite_module, subgroup_item)
    elif callable(subgroup_item):
        return subgroup_item()
    else:
        return subgroup_item


def default_test_runner(desc: str, tcase: hydra.testing.TestCaseWithMetadata) -> Optional[Callable[[], None]]:
    """Default test runner: handles UniversalTestCase and EffectfulTestCase, both string comparisons."""
    if should_skip_test(tcase):
        return None

    case = tcase.case

    match case:
        case hydra.testing.TestCaseUniversal(value=tc):
            def run_universal():
                actual = tc.actual(None)
                expected = tc.expected(None)
                if actual != expected:
                    raise AssertionError(f"expected {expected!r} but got {actual!r}")
            return run_universal

        case hydra.testing.TestCaseEffectful(value=tc):
            def run_effectful():
                prepare_effectful_temp_dir()
                actual = tc.actual(None)
                expected = tc.expected(None)
                if actual != expected:
                    raise AssertionError(f"expected {expected!r} but got {actual!r}")
            return run_effectful

        case _:
            case_type = type(tcase.case).__name__
            def fail_unhandled():
                pytest.fail(f"Unhandled test case type: {case_type}")
            return fail_unhandled


# Canonical root directory for effectful (file I/O) test cases. Must match the testDir
# constant in Hydra.Sources.Test.Lib.Files and the effectfulTestDir in the other host
# runners. Hard-coded *nix path for now (#494).
EFFECTFUL_TEST_DIR = "/tmp/hydra-testing"


def prepare_effectful_temp_dir() -> None:
    import shutil
    import os
    if os.path.isdir(EFFECTFUL_TEST_DIR):
        shutil.rmtree(EFFECTFUL_TEST_DIR)
    os.makedirs(EFFECTFUL_TEST_DIR, exist_ok=True)


def _count_test_cases(group: hydra.testing.TestGroup, runner: TestRunner, suite_module) -> tuple[int, int]:
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
        sg = _resolve_subgroup(subgroup_item, suite_module)
        r, s = _count_test_cases(sg, runner, suite_module)
        runnable += r
        skipped += s
    return runnable, skipped


def _group_to_json_value(group, parent_path, runner, results, suite_module):
    import hydra.json.model as json

    path = f"{parent_path}/{group.name}" if parent_path else group.name
    runnable, skipped = _count_test_cases(group, runner, suite_module)
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
        sg = _resolve_subgroup(subgroup_item, suite_module)
        subgroups.append(_group_to_json_value(sg, path, runner, results, suite_module))

    if subgroups:
        fields["subgroups"] = json.ValueArray(tuple(subgroups))

    return json.ValueObject(tuple(fields.items()))


def write_benchmark_json(
    output_path: str,
    runner: TestRunner,
    root_group: hydra.testing.TestGroup,
    suite_module,
    language: str,
    benchmark_results: dict[str, float],
) -> None:
    """Write benchmark results as JSON using Hydra's JSON writer."""
    import subprocess
    import hydra.json.model as json
    import hydra.json.writer as json_writer

    root_path = root_group.name

    def git_output(args: list[str]) -> str:
        try:
            result = subprocess.run(["git"] + args, capture_output=True, text=True, timeout=5)
            return result.stdout.strip()
        except Exception:
            return ""

    timestamp = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    branch = git_output(["rev-parse", "--abbrev-ref", "HEAD"])
    commit = git_output(["rev-parse", "--short", "HEAD"])
    commit_msg = git_output(["log", "-1", "--format=%s"])

    total_runnable, total_skipped = _count_test_cases(root_group, runner, suite_module)
    root_time = benchmark_results.get(root_path, 0.0)

    init_time_ms = benchmark_results.get(f"{root_path}/_initialization", 0.0)

    group_values = []
    if init_time_ms > 0:
        group_values.append(json.ValueObject((
            ("path", json.ValueString(f"{root_path}/_initialization")),
            ("passed", json.ValueNumber(Decimal(0))),
            ("failed", json.ValueNumber(Decimal(0))),
            ("skipped", json.ValueNumber(Decimal(0))),
            ("totalTimeMs", json.ValueNumber(Decimal(str(round(init_time_ms, 1))))),
        )))
    for subgroup_item in root_group.subgroups:
        sg = _resolve_subgroup(subgroup_item, suite_module)
        group_values.append(_group_to_json_value(sg, root_path, runner, benchmark_results, suite_module))

    json_value = json.ValueObject((
        ("metadata", json.ValueObject((
            ("timestamp", json.ValueString(timestamp)),
            ("language", json.ValueString(language)),
            ("branch", json.ValueString(branch)),
            ("commit", json.ValueString(commit)),
            ("commitMessage", json.ValueString(commit_msg)),
        ))),
        ("groups", json.ValueArray(tuple(group_values))),
        ("summary", json.ValueObject((
            ("totalPassed", json.ValueNumber(Decimal(total_runnable))),
            ("totalFailed", json.ValueNumber(Decimal(0))),
            ("totalSkipped", json.ValueNumber(Decimal(total_skipped))),
            ("totalTimeMs", json.ValueNumber(Decimal(str(round(root_time, 1))))),
        ))),
    ))

    json_str = json_writer.print_json(json_value)
    with open(output_path, "w") as f:
        f.write(json_str)
    print(f"Benchmark written to: {output_path}")


def generate_pytest_tests(
    group: hydra.testing.TestGroup,
    runner: TestRunner,
    suite_module,
    prefix: str = "",
    hydra_path: str = "",
    benchmark_output: str = "",
    benchmark_timers: dict[str, int] | None = None,
    benchmark_results: dict[str, float] | None = None,
) -> list:
    """Generate pytest test functions from a test group, organized by group hierarchy."""
    tests = []

    safe_group_name = group.name.replace(" ", "_").replace("-", "_").lower()
    new_prefix = f"{prefix}{safe_group_name}_" if prefix else f"{safe_group_name}_"
    new_hydra_path = f"{hydra_path}/{group.name}" if hydra_path else group.name

    if benchmark_output and benchmark_timers is not None:
        timer_start_name = f"{new_prefix}000_TIMER_START"
        def make_start(p=new_hydra_path):
            benchmark_timers[p] = time.perf_counter_ns()
        tests.append((timer_start_name, new_hydra_path, make_start))

    for i, tcase in enumerate(group.cases, 1):
        test_name = f"{new_prefix}case_{i}"
        case_hydra_path = f"{new_hydra_path}/{tcase.name}"
        desc = f"{group.name}, {tcase.name}"

        if should_skip_test(tcase):
            def make_skip_test():
                def test_fn():
                    pytest.skip("Test is disabled or too slow")
                return test_fn
            tests.append((test_name, case_hydra_path, make_skip_test()))
            continue

        def make_test(test_desc: str, test_case: hydra.testing.TestCaseWithMetadata):
            def test_fn():
                test_func = runner(test_desc, test_case)
                if test_func:
                    test_func()
                else:
                    pytest.skip("Test is disabled or not supported")
            return test_fn

        tests.append((test_name, case_hydra_path, make_test(desc, tcase)))

    for subgroup_item in group.subgroups:
        sg = _resolve_subgroup(subgroup_item, suite_module)
        tests.extend(generate_pytest_tests(
            sg, runner, suite_module, new_prefix, new_hydra_path,
            benchmark_output, benchmark_timers, benchmark_results))

    if benchmark_output and benchmark_timers is not None and benchmark_results is not None:
        timer_stop_name = f"{new_prefix}999_TIMER_END"
        def make_stop(p=new_hydra_path):
            if p in benchmark_timers:
                elapsed_ns = time.perf_counter_ns() - benchmark_timers[p]
                benchmark_results[p] = elapsed_ns / 1_000_000.0
        tests.append((timer_stop_name, new_hydra_path, make_stop))

    return tests
