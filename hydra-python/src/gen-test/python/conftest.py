"""Configure pytest for Hydra tests."""

from __future__ import annotations

import sys
from pathlib import Path

# Add source paths to Python path for test imports
root = Path(__file__).parent.parent.parent
main_path = root / "main" / "python"
gen_test_path = root / "gen-test" / "python"

# Insert at beginning of sys.path to ensure they're found first
# IMPORTANT: main_path must come before gen_test_path so that the full hydra
# package from src/main/python is found first, then hydra.test from gen-test
sys.path.insert(0, str(gen_test_path.resolve()))
sys.path.insert(0, str(main_path.resolve()))
