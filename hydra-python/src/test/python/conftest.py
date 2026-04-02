"""Configure pytest for Hydra tests."""

from __future__ import annotations

import sys
from pathlib import Path

# Add source paths to Python path for test imports
root = Path(__file__).parent.parent.parent
main_path = root / "main" / "python"
gen_main_path = root / "gen-main" / "python"
gen_test_path = root / "gen-test" / "python"

# Insert at beginning of sys.path to ensure they're found first
sys.path.insert(0, str(gen_test_path.resolve()))
sys.path.insert(0, str(gen_main_path.resolve()))
sys.path.insert(0, str(main_path.resolve()))


def pytest_configure(config):
    """Eagerly initialize test infrastructure before test collection."""
    try:
        import test_suite_runner
        test_suite_runner.get_test_graph()
        test_suite_runner.get_inference_context()
        test_suite_runner.get_type_context()
    except Exception:
        pass
