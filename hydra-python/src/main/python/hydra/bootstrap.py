"""Bootstrapping entry point: loads Hydra modules from JSON and generates
code for a target language. Demonstrates that Python can independently
regenerate Hydra from a language-independent JSON representation.

Usage:
    python -m hydra.bootstrap --target <haskell|java|python> --json-dir <path> [--output <dir>] [--types-only] [--kernel-only]
"""

import argparse
import os
import sys
import time

from hydra.generation import (
    filter_kernel_modules,
    filter_type_modules,
    kernel_modules,
    load_modules_from_json,
    read_manifest_field,
    write_haskell,
    write_java,
    write_python,
)


def _format_time(seconds):
    """Format elapsed time for display."""
    if seconds < 1:
        return f"{seconds * 1000:.0f}ms"
    elif seconds < 60:
        return f"{seconds:.1f}s"
    else:
        mins = int(seconds // 60)
        secs = seconds % 60
        return f"{mins}m {secs:.1f}s"


def _count_files(directory, ext):
    """Count files with a given extension in a directory tree."""
    count = 0
    for _, _, files in os.walk(directory):
        for f in files:
            if f.endswith(ext):
                count += 1
    return count


def main():
    # The generated JSON decoder uses recursive variant matching which can
    # exceed Python's default recursion limit for deeply nested types.
    sys.setrecursionlimit(10000)

    total_start = time.time()

    parser = argparse.ArgumentParser(
        description="Bootstrap Hydra code generation from JSON modules")
    parser.add_argument("--target", required=True, choices=["haskell", "java", "python"],
                        help="Target language for code generation")
    parser.add_argument("--json-dir", required=True,
                        help="Directory containing JSON module files")
    parser.add_argument("--output", default="/tmp/hydra-bootstrapping-demo",
                        help="Output base directory")
    parser.add_argument("--types-only", action="store_true",
                        help="Only generate type-defining modules")
    parser.add_argument("--kernel-only", action="store_true",
                        help="Only generate kernel modules (exclude hydra.ext.*)")
    args = parser.parse_args()

    out_dir = os.path.join(args.output, f"python-to-{args.target}")

    print("==========================================", flush=True)
    print(f"Bootstrapping Hydra from JSON into {args.target} (via Python host)", flush=True)
    print("==========================================", flush=True)
    print(f"  Host language:   Python", flush=True)
    print(f"  Target language: {args.target}", flush=True)
    print(f"  JSON directory:  {args.json_dir}", flush=True)
    print(f"  Output directory:{out_dir}", flush=True)
    if args.types_only:
        print("  Filter:          types only", flush=True)
    if args.kernel_only:
        print("  Filter:          kernel only (excluding hydra.ext.*)", flush=True)
    print("==========================================", flush=True)
    print(flush=True)

    # Step 1: Load all modules from JSON (main + eval lib)
    print("Step 1: Loading modules from JSON...", flush=True)
    print(f"  Source: {args.json_dir}", flush=True)
    step_start = time.time()
    main_namespaces = read_manifest_field(args.json_dir, "mainModules")
    eval_lib_namespaces = read_manifest_field(args.json_dir, "evalLibModules")
    all_namespaces = main_namespaces + eval_lib_namespaces
    km = kernel_modules()
    raw_mods = load_modules_from_json(False, args.json_dir, km, all_namespaces)
    step_time = time.time() - step_start
    total_bindings = sum(len(m.elements) for m in raw_mods)
    print(f"  Loaded {len(raw_mods)} modules ({total_bindings} bindings).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Main modules keep their type annotations from JSON (strip_type_schemes=False).
    # This allows the pipeline to skip pre-adaptation inference (Step 3 in
    # data_graph_to_definitions), which is a major performance win.
    all_mods = raw_mods

    # Step 3: Filter modules
    mods_to_generate = all_mods
    if args.kernel_only:
        before = len(mods_to_generate)
        mods_to_generate = filter_kernel_modules(mods_to_generate)
        # For main code generation, use kernel-only universe to match Java/Haskell host behavior.
        # The full all_mods (including ext) is preserved for test code generation below,
        # where test modules may reference ext bindings.
        main_universe = filter_kernel_modules(all_mods)
        print("Step 3: Filtering to kernel modules...", flush=True)
        print(f"  Before: {before} modules", flush=True)
        print(f"  After:  {len(mods_to_generate)} kernel modules "
              f"(excluded {before - len(mods_to_generate)} ext modules)", flush=True)
        print(flush=True)
    else:
        main_universe = all_mods
    if args.types_only:
        before = len(mods_to_generate)
        mods_to_generate = filter_type_modules(mods_to_generate)
        print("Step 3b: Filtering to type modules...", flush=True)
        print(f"  Before: {before} modules", flush=True)
        print(f"  After:  {len(mods_to_generate)} type modules "
              f"(excluded {before - len(mods_to_generate)} non-type modules)", flush=True)
        print(flush=True)

    # Step 5: Generate code for the target language
    out_main = os.path.join(out_dir, "src/gen-main")
    print(f"Step 4: Mapping modules from JSON to {args.target} (via Python host)...", flush=True)
    print(f"  Universe: {len(main_universe)} modules", flush=True)
    print(f"  Generating: {len(mods_to_generate)} modules:", flush=True)
    for m in mods_to_generate:
        bindings = len(m.elements)
        print(f"    - {m.namespace.value} ({bindings} bindings)", flush=True)
    print(f"  Output: {out_dir}", flush=True)
    print(flush=True)
    print("  Generating (this may take several minutes)...", flush=True)

    step_start = time.time()

    if args.target == "haskell":
        write_haskell(os.path.join(out_main, "haskell"), main_universe, mods_to_generate)
    elif args.target == "java":
        write_java(os.path.join(out_main, "java"), main_universe, mods_to_generate)
    elif args.target == "python":
        write_python(os.path.join(out_main, "python"), main_universe, mods_to_generate)

    step_time = time.time() - step_start

    # Count main output files
    ext = {"java": ".java", "python": ".py", "haskell": ".hs"}[args.target]
    main_file_count = _count_files(os.path.join(out_dir, "src/gen-main"), ext)

    print(f"  Python to {args.target}: done generating main modules ({main_file_count} files).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Step 6: Load and generate test modules
    test_json_dir = args.json_dir.replace("gen-main/json", "gen-test/json")
    print("Step 5: Loading test modules from JSON...", flush=True)
    print(f"  Source: {test_json_dir}", flush=True)
    step_start = time.time()
    # Load test modules WITHOUT stripping TypeSchemes (strip_type_schemes=False).
    # Test modules need their types preserved so inference can be skipped.
    test_namespaces = read_manifest_field(args.json_dir, "testModules")
    test_mods = load_modules_from_json(False, test_json_dir, km, test_namespaces)
    step_time = time.time() - step_start
    test_bindings = sum(len(m.elements) for m in test_mods)
    print(f"  Loaded {len(test_mods)} test modules ({test_bindings} bindings).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    all_universe = all_mods + test_mods
    out_test = os.path.join(out_dir, "src/gen-test")
    print(f"Step 6: Mapping test suite from JSON to {args.target} (via Python host)...", flush=True)
    print(f"  Universe: {len(all_universe)} modules", flush=True)
    print(f"  Generating: {len(test_mods)} test modules", flush=True)
    print(f"  Output: {out_test}", flush=True)
    print(flush=True)
    print("  Generating (this may take several minutes)...", flush=True)

    step_start = time.time()

    if args.target == "haskell":
        write_haskell(os.path.join(out_test, "haskell"), all_universe, test_mods)
    elif args.target == "java":
        write_java(os.path.join(out_test, "java"), all_universe, test_mods)
    elif args.target == "python":
        write_python(os.path.join(out_test, "python"), all_universe, test_mods)

    step_time = time.time() - step_start
    test_file_count = _count_files(out_test, ext)

    print(f"  Python to {args.target}: done generating test modules ({test_file_count} files).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    total_time = time.time() - total_start

    print("==========================================", flush=True)
    print(f"Python to {args.target}: done generating all modules", flush=True)
    print("==========================================", flush=True)
    print(f"  Modules loaded:    {len(raw_mods)} main + {len(test_mods)} test", flush=True)
    print(f"  Modules generated: {len(mods_to_generate)} main + {len(test_mods)} test", flush=True)
    print(f"  Output files:      {main_file_count} main + {test_file_count} test", flush=True)
    print(f"  Output directory:  {out_dir}", flush=True)
    print(f"  Total time:        {_format_time(total_time)}", flush=True)
    print("==========================================", flush=True)


if __name__ == "__main__":
    main()
