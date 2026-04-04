"""Bootstrapping entry point: loads Hydra modules from JSON and generates
code for a target language. Demonstrates that Python can independently
regenerate Hydra from a language-independent JSON representation.

Usage:
    python -m hydra.bootstrap --target <haskell|java|python> --json-dir <path> [OPTIONS]

Options:
    --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
    --include-coders       Also load and generate ext coder modules
    --include-tests        Also load and generate kernel test modules
    --ext-json-dir <dir>   Directory containing ext JSON modules (for --include-coders)
    --kernel-only          Only generate kernel modules (exclude hydra.ext.*)
    --types-only           Only generate type-defining modules
"""

import argparse
import os
import sys
import time

from hydra.generation import (
    filter_kernel_modules,
    filter_type_modules,
    load_modules_from_json,
    read_manifest_field,
    write_haskell,
    write_java,
    write_python,
    write_lisp_dialect,
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
    """Count files with a given extension in a directory tree, excluding __init__.py."""
    count = 0
    for _, _, files in os.walk(directory):
        for f in files:
            if f.endswith(ext) and f != "__init__.py":
                count += 1
    return count


def main():
    # The generated JSON decoder uses recursive variant matching which can
    # exceed Python's default recursion limit for deeply nested types.
    sys.setrecursionlimit(10000)

    total_start = time.time()

    parser = argparse.ArgumentParser(
        description="Bootstrap Hydra code generation from JSON modules")
    parser.add_argument("--target", required=True,
                        choices=["haskell", "java", "python", "scala",
                                 "clojure", "scheme", "common-lisp", "emacs-lisp"],
                        help="Target language for code generation")
    parser.add_argument("--json-dir", required=True,
                        help="Directory containing JSON module files")
    parser.add_argument("--ext-json-dir", default=None,
                        help="Directory containing ext JSON modules (for --include-coders)")
    parser.add_argument("--output", default="/tmp/hydra-bootstrapping-demo",
                        help="Output base directory")
    parser.add_argument("--include-coders", action="store_true",
                        help="Also load and generate ext coder modules")
    parser.add_argument("--include-tests", action="store_true",
                        help="Also load and generate kernel test modules")
    parser.add_argument("--types-only", action="store_true",
                        help="Only generate type-defining modules")
    parser.add_argument("--kernel-only", action="store_true",
                        help="Only generate kernel modules (exclude hydra.ext.*)")
    args = parser.parse_args()

    if args.include_coders and not args.ext_json_dir:
        print("Error: --include-coders requires --ext-json-dir")
        sys.exit(1)

    target_cap = args.target.capitalize()
    out_dir = os.path.join(args.output, f"python-to-{args.target}")

    print("==========================================", flush=True)
    print(f"Mapping JSON to {target_cap} (via Python host)", flush=True)
    print("==========================================", flush=True)
    print(f"  Host language:   Python", flush=True)
    print(f"  Target language: {target_cap}", flush=True)
    print(f"  JSON directory:  {args.json_dir}", flush=True)
    print(f"  Output:          {out_dir}", flush=True)
    print(f"  Include coders:  {args.include_coders}", flush=True)
    print(f"  Include tests:   {args.include_tests}", flush=True)
    if args.types_only:
        print("  Filter:          types only", flush=True)
    if args.kernel_only:
        print("  Filter:          kernel only", flush=True)
    print("==========================================", flush=True)
    print(flush=True)

    # Step 1: Load main + eval lib modules from JSON
    print("Step 1: Loading main modules from JSON...", flush=True)
    print(f"  Source: {args.json_dir}", flush=True)
    step_start = time.time()
    main_namespaces = read_manifest_field(args.json_dir, "mainModules")
    eval_lib_namespaces = read_manifest_field(args.json_dir, "evalLibModules")
    all_kernel_namespaces = main_namespaces + eval_lib_namespaces
    main_mods = load_modules_from_json(args.json_dir, all_kernel_namespaces)
    step_time = time.time() - step_start
    total_bindings = sum(len(m.definitions) for m in main_mods)
    print(f"  Loaded {len(main_mods)} modules ({total_bindings} bindings).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Step 2: Optionally load ext coder modules
    coder_mods = []
    if args.include_coders:
        print("Step 2: Loading hydra-ext coder modules from JSON...", flush=True)
        coder_namespaces = read_manifest_field(args.ext_json_dir, "hydraBootstrapCoderModules")
        # Filter out modules already loaded as part of kernel
        kernel_ns_set = {ns.value for ns in all_kernel_namespaces}
        ext_coder_namespaces = [ns for ns in coder_namespaces if ns.value not in kernel_ns_set]
        step_start = time.time()
        coder_mods = load_modules_from_json(args.ext_json_dir, ext_coder_namespaces)
        step_time = time.time() - step_start
        print(f"  Loaded {len(coder_mods)} modules.", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)
    else:
        print("Step 2: Skipping ext coder modules", flush=True)
        print(flush=True)

    all_main_mods = main_mods + coder_mods

    # Apply filters
    mods_to_generate = all_main_mods
    if args.kernel_only:
        before = len(mods_to_generate)
        mods_to_generate = filter_kernel_modules(mods_to_generate)
        all_main_mods = filter_kernel_modules(all_main_mods)
        print("Filtering to kernel modules...", flush=True)
        print(f"  Before: {before} modules", flush=True)
        print(f"  After:  {len(mods_to_generate)} kernel modules", flush=True)
        print(flush=True)
    if args.types_only:
        before = len(mods_to_generate)
        mods_to_generate = filter_type_modules(mods_to_generate)
        print("Filtering to type modules...", flush=True)
        print(f"  Before: {before} modules", flush=True)
        print(f"  After:  {len(mods_to_generate)} type modules", flush=True)
        print(flush=True)

    # Keep full module list for test universe
    full_mods = main_mods + coder_mods

    # Generate main modules
    out_main = os.path.join(out_dir, "src/gen-main")
    print(f"Mapping {len(mods_to_generate)} modules to {target_cap}...", flush=True)
    print(f"  Universe: {len(all_main_mods)} modules", flush=True)
    print(f"  Output: {out_main}", flush=True)
    print(flush=True)

    step_start = time.time()

    _lisp_dialects = {
        "clojure": ("clojure", "clj"),
        "scheme": ("scheme", "scm"),
        "common-lisp": ("common_lisp", "lisp"),
        "emacs-lisp": ("emacs_lisp", "el"),
    }

    if args.target == "haskell":
        write_haskell(os.path.join(out_main, "haskell"), all_main_mods, mods_to_generate)
    elif args.target == "java":
        write_java(os.path.join(out_main, "java"), all_main_mods, mods_to_generate)
    elif args.target == "python":
        write_python(os.path.join(out_main, "python"), all_main_mods, mods_to_generate)
    elif args.target == "scala":
        from hydra.generation import write_scala
        write_scala(os.path.join(out_main, "scala"), all_main_mods, mods_to_generate)
    elif args.target in _lisp_dialects:
        dialect_name, _ext = _lisp_dialects[args.target]
        write_lisp_dialect(os.path.join(out_main, args.target), dialect_name, _ext,
                           all_main_mods, mods_to_generate)

    step_time = time.time() - step_start

    ext = {"java": ".java", "python": ".py", "haskell": ".hs", "scala": ".scala",
           "clojure": ".clj", "scheme": ".scm", "common-lisp": ".lisp",
           "emacs-lisp": ".el"}[args.target]
    main_file_count = _count_files(os.path.join(out_dir, "src/gen-main"), ext)

    print(f"  Generated {main_file_count} files.", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Optionally load and generate test modules
    test_file_count = 0
    if args.include_tests:
        test_json_dir = args.json_dir.replace("gen-main/json", "gen-test/json")
        print("Loading test modules from JSON...", flush=True)
        print(f"  Source: {test_json_dir}", flush=True)
        step_start = time.time()
        test_namespaces = read_manifest_field(args.json_dir, "testModules")
        test_mods = load_modules_from_json(test_json_dir, test_namespaces)
        step_time = time.time() - step_start
        test_bindings = sum(len(m.definitions) for m in test_mods)
        print(f"  Loaded {len(test_mods)} test modules ({test_bindings} bindings).", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)

        all_universe = full_mods + test_mods
        out_test = os.path.join(out_dir, "src/gen-test")

        # When --kernel-only is active, ext modules are excluded from the main
        # generation targets. But test modules may depend on ext modules (e.g.
        # hydra.test.serialization depends on hydra.ext.haskell.operators).
        # Generate those ext modules to outMain so test code can reference them.
        if args.kernel_only:
            test_ext_deps = set()
            for m in test_mods:
                for ns in m.term_dependencies:
                    if ns.value.startswith("hydra.ext."):
                        test_ext_deps.add(ns.value)
            if test_ext_deps:
                ext_mods_for_tests = [m for m in full_mods if m.namespace.value in test_ext_deps]
                if ext_mods_for_tests:
                    print(f"Generating {len(ext_mods_for_tests)} ext module(s) needed by tests...", flush=True)
                    out_main_sub = os.path.join(out_dir, "src/gen-main")
                    if args.target == "haskell":
                        write_haskell(os.path.join(out_main_sub, "haskell"), all_universe, ext_mods_for_tests)
                    elif args.target == "java":
                        write_java(os.path.join(out_main_sub, "java"), all_universe, ext_mods_for_tests)
                    elif args.target == "python":
                        write_python(os.path.join(out_main_sub, "python"), all_universe, ext_mods_for_tests)
                    elif args.target == "scala":
                        from hydra.generation import write_scala
                        write_scala(os.path.join(out_main_sub, "scala"), all_universe, ext_mods_for_tests)
                    elif args.target in _lisp_dialects:
                        d, e = _lisp_dialects[args.target]
                        write_lisp_dialect(os.path.join(out_main_sub, args.target), d, e,
                                           all_universe, ext_mods_for_tests)
                    print(flush=True)

        print(f"Mapping test modules to {target_cap}...", flush=True)
        print(f"  Universe: {len(all_universe)} modules", flush=True)
        print(f"  Generating: {len(test_mods)} test modules", flush=True)
        print(f"  Output: {out_test}", flush=True)
        print(flush=True)

        step_start = time.time()

        if args.target == "haskell":
            write_haskell(os.path.join(out_test, "haskell"), all_universe, test_mods)
        elif args.target == "java":
            write_java(os.path.join(out_test, "java"), all_universe, test_mods)
        elif args.target == "python":
            write_python(os.path.join(out_test, "python"), all_universe, test_mods)
        elif args.target == "scala":
            from hydra.generation import write_scala
            write_scala(os.path.join(out_test, "scala"), all_universe, test_mods)
        elif args.target in _lisp_dialects:
            d, e = _lisp_dialects[args.target]
            write_lisp_dialect(os.path.join(out_test, args.target), d, e,
                               all_universe, test_mods)

        step_time = time.time() - step_start
        test_file_count = _count_files(os.path.join(out_dir, "src/gen-test"), ext)

        print(f"  Generated {test_file_count} test files.", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)

    total_time = time.time() - total_start

    print("==========================================", flush=True)
    test_str = f" + {test_file_count} test" if args.include_tests else ""
    print(f"Done: {main_file_count} main{test_str} files", flush=True)
    print(f"  Output: {out_dir}", flush=True)
    print(f"  Total time: {_format_time(total_time)}", flush=True)
    print("==========================================", flush=True)


if __name__ == "__main__":
    main()
