#!/usr/bin/env python3
"""Debug the write_annotation error."""

import sys
sys.setrecursionlimit(50000)
sys.path.insert(0, "hydra-python/src/gen-main/python")
sys.path.insert(0, "hydra-python/src/main/python")

import hydra.ext.java.serde
import hydra.ext.java.syntax

# Monkey-patch write_annotation to print what it receives
_orig = hydra.ext.java.serde.write_annotation
def _debug_write_annotation(ann):
    try:
        return _orig(ann)
    except AssertionError:
        print(f"\nwrite_annotation failed!")
        print(f"  type(ann) = {type(ann)}")
        print(f"  ann = {ann}")
        print(f"  isinstance checks:")
        print(f"    AnnotationNormal: {isinstance(ann, hydra.ext.java.syntax.AnnotationNormal)}")
        print(f"    AnnotationMarker: {isinstance(ann, hydra.ext.java.syntax.AnnotationMarker)}")
        print(f"    AnnotationSingleElement: {isinstance(ann, hydra.ext.java.syntax.AnnotationSingleElement)}")
        # Check if it's an lru_cache wrapper
        print(f"    callable: {callable(ann)}")
        if callable(ann) and not isinstance(ann, (hydra.ext.java.syntax.AnnotationNormal, hydra.ext.java.syntax.AnnotationMarker, hydra.ext.java.syntax.AnnotationSingleElement)):
            print(f"  Calling ann():")
            result = ann()
            print(f"    type(result) = {type(result)}")
            print(f"    result = {result}")
        raise

hydra.ext.java.serde.write_annotation = _debug_write_annotation

# Now run
from hydra.generation import load_modules_from_json, read_manifest_field, write_java, filter_kernel_modules

json_dir = "hydra-haskell/src/gen-main/json"
print("Loading modules from JSON...", flush=True)
main_ns = read_manifest_field(json_dir, "mainModules")
eval_ns = read_manifest_field(json_dir, "evalLibModules")
mods = load_modules_from_json(False, json_dir, main_ns + eval_ns)
mods = filter_kernel_modules(mods)
print(f"Loaded {len(mods)} kernel modules", flush=True)

print("\nGenerating Java code...", flush=True)
try:
    write_java("/tmp/debug-p2j/src/gen-main", mods, mods)
except Exception as e:
    print(f"\n{type(e).__name__}: {e}")
