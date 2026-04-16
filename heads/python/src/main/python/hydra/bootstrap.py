"""Bootstrapping entry point: loads Hydra modules from JSON and generates
code for a target language. Demonstrates that Python can independently
regenerate Hydra from a language-independent JSON representation.

Usage:
    python -m hydra.bootstrap --target <lang> --json-dir <dist/json root> [OPTIONS]

Options:
    --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
    --include-coders       Also load coder packages (hydra-java, hydra-python, hydra-scala, hydra-lisp)
    --include-tests        Also load and generate kernel test modules
    --kernel-only          Only generate kernel modules (exclude coder packages)
    --types-only           Only generate type-defining modules
    --ext-json-dir <dir>   Legacy flag; ignored under the per-package layout.
"""

import argparse
import json
import os
import sys
import time

from hydra.generation import (
    filter_type_modules,
    load_modules_from_json,
    read_manifest_field,
    write_haskell,
    write_java,
    write_python,
    write_lisp_dialect,
)

# Coder packages loaded on top of the kernel + Haskell baseline when
# --include-coders is set.
_CODER_PACKAGES = ["hydra-java", "hydra-python", "hydra-scala", "hydra-lisp"]


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


def _legacy_json_dir_to_root(path):
    """Map a legacy --json-dir value ending in <pkg>/src/main/json to the dist/json root.

    If the path does not match the expected shape, return it unchanged.
    """
    parts = path.rstrip(os.sep).split(os.sep)
    if len(parts) >= 4 and parts[-3:] == ["src", "main", "json"]:
        # parts[-4] is the package name; drop the last 4 components
        return os.sep.join(parts[:-4]) if parts[:-4] else "."
    return path


def _package_main_dir(root, pkg):
    """Return the JSON directory for a package's main modules."""
    return os.path.join(root, pkg, "src", "main", "json")


def _read_manifest_field_or_empty(pkg_dir, field_name):
    """Read a field from manifest.json, returning [] if the field or file is missing."""
    manifest_path = os.path.join(pkg_dir, "manifest.json")
    if not os.path.isfile(manifest_path):
        return []
    with open(manifest_path, "r", encoding="utf-8") as f:
        manifest = json.load(f)
    if field_name not in manifest:
        return []
    # Reuse Namespace-wrapping behavior from read_manifest_field.
    from hydra.packaging import Namespace
    return [Namespace(ns) for ns in manifest[field_name]]


def _load_package_main(root, pkg):
    """Load the mainModules + evalLibModules from a package's manifest."""
    pkg_dir = _package_main_dir(root, pkg)
    main_ns = _read_manifest_field_or_empty(pkg_dir, "mainModules")
    eval_ns = _read_manifest_field_or_empty(pkg_dir, "evalLibModules")
    all_ns = main_ns + eval_ns
    if not all_ns:
        return []
    print(f"  {pkg}: {len(all_ns)} modules from {pkg_dir}", flush=True)
    return load_modules_from_json(pkg_dir, all_ns)


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
                        help="Root of the per-package JSON directories (dist/json/)")
    parser.add_argument("--ext-json-dir", default=None,
                        help="Legacy flag; ignored under the per-package layout")
    parser.add_argument("--output", default="/tmp/hydra-bootstrapping-demo",
                        help="Output base directory")
    parser.add_argument("--include-coders", action="store_true",
                        help="Also load and generate coder package modules")
    parser.add_argument("--include-tests", action="store_true",
                        help="Also load and generate kernel test modules")
    parser.add_argument("--types-only", action="store_true",
                        help="Only generate type-defining modules")
    parser.add_argument("--kernel-only", action="store_true",
                        help="Only generate kernel modules (exclude coder packages)")
    args = parser.parse_args()

    # Backward compatibility: accept an old-style --json-dir ending in
    # <pkg>/src/main/json and strip down to the dist/json root.
    dist_json_root = _legacy_json_dir_to_root(args.json_dir)

    target_cap = args.target.capitalize()
    out_dir = os.path.join(args.output, f"python-to-{args.target}")

    print("==========================================", flush=True)
    print(f"Mapping JSON to {target_cap} (via Python host)", flush=True)
    print("==========================================", flush=True)
    print(f"  Host language:   Python", flush=True)
    print(f"  Target language: {target_cap}", flush=True)
    print(f"  JSON root:       {dist_json_root}", flush=True)
    print(f"  Output:          {out_dir}", flush=True)
    print(f"  Include coders:  {args.include_coders}", flush=True)
    print(f"  Include tests:   {args.include_tests}", flush=True)
    if args.types_only:
        print("  Filter:          types only", flush=True)
    if args.kernel_only:
        print("  Filter:          kernel only", flush=True)
    print("==========================================", flush=True)
    print(flush=True)

    # Step 1: Load baseline packages (hydra-kernel + hydra-haskell).
    print("Step 1: Loading baseline main modules from JSON...", flush=True)
    step_start = time.time()
    kernel_mods = _load_package_main(dist_json_root, "hydra-kernel")
    haskell_mods = _load_package_main(dist_json_root, "hydra-haskell")
    baseline_mods = kernel_mods + haskell_mods
    step_time = time.time() - step_start
    total_bindings = sum(len(m.definitions) for m in baseline_mods)
    print(f"  Loaded {len(baseline_mods)} baseline modules ({total_bindings} bindings).", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Namespaces owned by the kernel package (used for --kernel-only filtering).
    kernel_ns_set = {m.namespace.value for m in kernel_mods}

    # Step 2: Optionally load coder packages.
    coder_mods = []
    if args.include_coders:
        print("Step 2: Loading coder package modules from JSON...", flush=True)
        step_start = time.time()
        for pkg in _CODER_PACKAGES:
            coder_mods.extend(_load_package_main(dist_json_root, pkg))
        step_time = time.time() - step_start
        print(f"  Loaded {len(coder_mods)} coder modules.", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)
    else:
        print("Step 2: Skipping coder packages", flush=True)
        print(flush=True)

    all_main_mods = baseline_mods + coder_mods

    # Apply filters
    mods_to_generate = all_main_mods
    if args.kernel_only:
        before = len(mods_to_generate)
        mods_to_generate = [m for m in mods_to_generate if m.namespace.value in kernel_ns_set]
        all_main_mods = [m for m in all_main_mods if m.namespace.value in kernel_ns_set]
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
    full_mods = baseline_mods + coder_mods

    # Generate main modules
    out_main = os.path.join(out_dir, "src/main")
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
    main_file_count = _count_files(os.path.join(out_dir, "src/main"), ext)

    print(f"  Generated {main_file_count} files.", flush=True)
    print(f"  Time: {_format_time(step_time)}", flush=True)
    print(flush=True)

    # Optionally load and generate test modules. Test modules live under
    # hydra-kernel/src/test/json/, and the kernel's main manifest lists them.
    test_file_count = 0
    if args.include_tests:
        kernel_main_dir = _package_main_dir(dist_json_root, "hydra-kernel")
        test_json_dir = os.path.join(dist_json_root, "hydra-kernel", "src", "test", "json")
        print("Loading test modules from JSON...", flush=True)
        print(f"  Source: {test_json_dir}", flush=True)
        step_start = time.time()
        test_namespaces = read_manifest_field(kernel_main_dir, "testModules")
        test_mods = load_modules_from_json(test_json_dir, test_namespaces)
        step_time = time.time() - step_start
        test_bindings = sum(len(m.definitions) for m in test_mods)
        print(f"  Loaded {len(test_mods)} test modules ({test_bindings} bindings).", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)

        all_universe = full_mods + test_mods
        out_test = os.path.join(out_dir, "src/test")

        # When --kernel-only is active, ext modules are excluded from the main
        # generation targets. But test modules may depend on ext modules (e.g.
        # hydra.test.serialization depends on hydra.haskell.operators).
        # Generate those ext modules to outMain so test code can reference them.
        if args.kernel_only:
            test_ext_deps = set()
            for m in test_mods:
                for ns in m.term_dependencies:
                    if ns.value.startswith("hydra."):
                        test_ext_deps.add(ns.value)
            if test_ext_deps:
                ext_mods_for_tests = [m for m in full_mods if m.namespace.value in test_ext_deps]
                if ext_mods_for_tests:
                    print(f"Generating {len(ext_mods_for_tests)} ext module(s) needed by tests...", flush=True)
                    out_main_sub = os.path.join(out_dir, "src/main")
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
        test_file_count = _count_files(os.path.join(out_dir, "src/test"), ext)

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
