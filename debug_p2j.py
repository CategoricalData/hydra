#!/usr/bin/env python3
"""Test python-to-java generation."""

import sys
sys.setrecursionlimit(50000)
sys.path.insert(0, "hydra-python/src/gen-main/python")
sys.path.insert(0, "hydra-python/src/main/python")

from hydra.generation import load_modules_from_json, read_manifest_field, write_java, filter_kernel_modules

json_dir = "hydra-haskell/src/gen-main/json"
print("Loading modules from JSON...", flush=True)
main_ns = read_manifest_field(json_dir, "mainModules")
eval_ns = read_manifest_field(json_dir, "evalLibModules")
mods = load_modules_from_json(json_dir, main_ns + eval_ns)
mods = filter_kernel_modules(mods)
print(f"Loaded {len(mods)} kernel modules", flush=True)

print("\nGenerating Java code...", flush=True)
try:
    write_java("/tmp/debug-p2j/src/gen-main", mods, mods)
    print("SUCCESS!")
except Exception as e:
    print(f"\n{type(e).__name__}: {e}")
    import traceback
    traceback.print_exc()
