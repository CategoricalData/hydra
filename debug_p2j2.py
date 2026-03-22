#!/usr/bin/env python3
"""Test python-to-java generation with debug."""

import sys
sys.setrecursionlimit(50000)
sys.path.insert(0, "hydra-python/src/gen-main/python")
sys.path.insert(0, "hydra-python/src/main/python")

# Monkey-patch java_class_type to see what qual() returns
import hydra.ext.java.utils as utils
_orig_jct = utils.java_class_type

def debug_java_class_type(args, pkg, id):
    result = _orig_jct(args, pkg, id)
    if result.qualifier is None:
        print(f"\nDEBUG: java_class_type returned ClassType with qualifier=None!", flush=True)
        print(f"  id={id}, pkg={pkg}", flush=True)
    return result

utils.java_class_type = debug_java_class_type

from hydra.generation import load_modules_from_json, read_manifest_field, write_java, filter_kernel_modules

json_dir = "hydra-haskell/src/gen-main/json"
main_ns = read_manifest_field(json_dir, "mainModules")
eval_ns = read_manifest_field(json_dir, "evalLibModules")
mods = load_modules_from_json(False, json_dir, main_ns + eval_ns)
mods = filter_kernel_modules(mods)
print(f"Loaded {len(mods)} kernel modules", flush=True)

print("Generating Java code...", flush=True)
try:
    write_java("/tmp/debug-p2j/src/gen-main", mods, mods)
    print("SUCCESS!")
except Exception as e:
    print(f"\n{type(e).__name__}: {e}")
