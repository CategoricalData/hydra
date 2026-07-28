"""
Test runner for hydra-build's own test suite.

Package-scoped counterpart to test_suite_runner.py (hydra-kernel); part of #547's
per-package test aggregation. pytest auto-discovers this file alongside
test_suite_runner.py, so no composition/registration step is needed.

All of hydra-build's test cases are universal (pure string comparison); no test-graph
construction is needed here (unlike test_suite_runner.py, hydra-build's tests don't
evaluate against a primitive-backed Graph). Uses the same generic walker
(hydra_test_group_walker) as the kernel runner.
"""

from __future__ import annotations

import sys
from pathlib import Path

_root = Path(__file__).parent.parent.parent
_main_path = _root / "main" / "python"
_gen_main_path = _root / "gen-main" / "python"
_gen_test_path = _root / "gen-test" / "python"

for _p in [str(_gen_test_path), str(_gen_main_path), str(_main_path)]:
    if _p not in sys.path:
        sys.path.insert(0, _p)

import hydra.test.build.test_suite as build_test_suite

from hydra_test_group_walker import default_test_runner, generate_pytest_tests

_all_tests = generate_pytest_tests(build_test_suite.all_tests(), default_test_runner, build_test_suite)

HYDRA_PATH_MAP: dict[str, str] = {f"test_{name}": path for name, path, _ in _all_tests}

for test_name, hydra_path, test_fn in _all_tests:
    globals()[f"test_{test_name}"] = test_fn
