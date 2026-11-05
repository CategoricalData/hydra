"""Phantom-type fidelity conformance for the Python DSL (#732, D2 signature tightening).

Runs pyright once over heads/python/src/test/python/phantom_fixtures/ and checks:
  - adversarial.py: every mistake-class expression is rejected (a pyright error).
  - positive.py: well-typed composition type-checks with zero errors.

Mirrors the M1-M5 mistake-class battery from #410/#672's phantom-fidelity measurement
(the write-up lives in docs/history/dsl-phantom-fidelity.md on the still-open
task_410_dsl_review branch, not yet merged to main). M4 (type-class op on wrong type)
and M5 (record field mismatch) are out of scope for #732 (D1/generated-constructor
concerns, not the D2 arrow-signature fix) -- this module covers M1-M3 only.

Deliberately does NOT rely on heads/python/pyproject.toml's own [tool.pyright]
extraPaths -- these fixtures only need the kernel dist tree (hydra.core.*), so they
get their own minimal pyrightconfig.json instead of the full project config.

Also deliberately runs pyright with a stripped PATH (no "<venv>/bin" prefix): when a
venv's own bin/ directory is first on PATH, the pyright-python package's Node-based
language server silently fails to resolve extraPaths (reproducibly: identical pyright
invocation, identical config, succeeds with a plain PATH and fails with the venv-bin-
prefixed PATH that `uv run` / an activated venv sets). Confirmed empirically across many
isolated repros; stripping the venv's own bin/ before invoking pyright is the reliable
fix here, whatever upstream mechanism causes it.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

FIXTURES_DIR = Path(__file__).parent / "phantom_fixtures"
# heads/python/src/test/python/test_phantom_fidelity.py -> heads/python -> ../../dist
_HEADS_PYTHON_DIR = Path(__file__).parent.parent.parent.parent
_HYDRA_KERNEL_MAIN = (_HEADS_PYTHON_DIR / "../../dist/python/hydra-kernel/src/main/python").resolve()
_CONFIG_PATH = FIXTURES_DIR / "pyrightconfig.json"

# 1-based line numbers where adversarial.py must produce a pyright error.
ADVERSARIAL_ERROR_LINES = {15, 19, 23, 28}

_PYRIGHT_BIN = shutil.which("pyright") or str(Path(sys.executable).parent / "pyright")
_VENV_BIN = str(Path(sys.executable).parent)


def _env_without_venv_bin() -> dict:
    env = os.environ.copy()
    path_entries = [p for p in env.get("PATH", "").split(os.pathsep) if p != _VENV_BIN]
    env["PATH"] = os.pathsep.join(path_entries)
    return env


_PYRIGHT_TIMEOUT_SECONDS = 60


def _run_pyright(*files: Path) -> dict:
    # A config file placed directly in the fixtures directory (not /tmp) so pyright
    # resolves extraPaths relative to the fixtures' own location, matching the
    # directory the module's imports actually need to resolve against.
    _CONFIG_PATH.write_text(json.dumps({"extraPaths": [str(_HYDRA_KERNEL_MAIN)]}))
    try:
        result = subprocess.run(
            [_PYRIGHT_BIN, "--outputjson", *[str(f) for f in files]],
            capture_output=True,
            text=True,
            cwd=str(FIXTURES_DIR),
            env=_env_without_venv_bin(),
            timeout=_PYRIGHT_TIMEOUT_SECONDS,
        )
        return json.loads(result.stdout)
    finally:
        _CONFIG_PATH.unlink(missing_ok=True)


@pytest.fixture(scope="module")
def pyright_report():
    return _run_pyright(FIXTURES_DIR / "adversarial.py", FIXTURES_DIR / "positive.py")


def test_adversarial_mistakes_are_rejected(pyright_report):
    adversarial_errors = {
        diag["range"]["start"]["line"] + 1
        for diag in pyright_report["generalDiagnostics"]
        if diag["file"].endswith("adversarial.py") and diag["severity"] == "error"
    }
    missing = ADVERSARIAL_ERROR_LINES - adversarial_errors
    assert not missing, (
        f"Expected pyright errors on adversarial.py lines {sorted(missing)}, "
        f"but pyright accepted them (phantom fidelity regression)"
    )


def test_well_typed_composition_passes_clean(pyright_report):
    positive_errors = [
        diag
        for diag in pyright_report["generalDiagnostics"]
        if diag["file"].endswith("positive.py") and diag["severity"] == "error"
    ]
    assert not positive_errors, (
        f"positive.py should type-check with zero errors, got: {positive_errors}"
    )
