#!/usr/bin/env python3.12
"""Debug P2J recursion issue by running with a recursion limit that produces a traceback."""
import sys
sys.setrecursionlimit(500)

sys.path.insert(0, 'hydra-python/src/gen-main/python')
sys.path.insert(0, 'hydra-python/src/main/python')

from hydra.generation import bootstrap_graph, empty_context, generate_source_files, strip_all_term_types, load_modules_from_json, read_manifest_field
from hydra.ext.java.coder import module_to_java
from hydra.ext.java.language import java_language

import traceback

# Load modules
json_dir = 'hydra-haskell/src/gen-main/json'
main_namespaces = read_manifest_field(json_dir, "mainModules")
eval_lib_namespaces = read_manifest_field(json_dir, "evalLibModules")
all_kernel_namespaces = main_namespaces + eval_lib_namespaces
main_mods = load_modules_from_json(False, json_dir, all_kernel_namespaces)

# Filter to kernel
kernel_mods = [m for m in main_mods if not m.namespace.value.startswith("hydra.ext.")]
print(f"Loaded {len(kernel_mods)} kernel modules")

# Try just a few modules to find which one triggers the recursion
bs_graph = bootstrap_graph()
cx = empty_context()

for i, mod in enumerate(kernel_mods):
    name = mod.namespace.value
    try:
        result = generate_source_files(
            module_to_java, java_language(),
            False, True, False, True,
            bs_graph, tuple(kernel_mods), (mod,), cx)
        # Check if result is Left or Right
        from hydra.dsl.python import Left, Right
        if isinstance(result, Left):
            print(f"  [{i}] {name}: Left error: {result.value}")
        else:
            files = result.value
            print(f"  [{i}] {name}: OK ({len(files)} files)")
    except RecursionError:
        print(f"  [{i}] {name}: RECURSION ERROR")
        traceback.print_exc()
        # Print just the last 30 unique frames
        break
    except Exception as e:
        print(f"  [{i}] {name}: ERROR: {type(e).__name__}: {e}")
