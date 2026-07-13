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
import re
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

# Fallback hydra.lib.* sub-namespaces, used only if _lib_subs_for_target()'s overlay-directory
# existence check can't reach the source tree (e.g. a relocated/packaged invocation).
_LIB_SUBS_FALLBACK = ["chars", "effects", "eithers", "equality", "files", "hashing", "lists",
                      "literals", "logic", "maps", "math", "optionals", "pairs", "regex", "sets",
                      "strings", "system", "text"]

_OVERLAY_DIR_SEGMENT = {"common-lisp": "common_lisp", "emacs-lisp": "emacs_lisp"}


def _overlay_dir_segment(target):
    return _OVERLAY_DIR_SEGMENT.get(target, target)


def _lib_subs_for_target(repo_root, target):
    """#568 structural fix: derive the redirectable hydra.lib.<sub> sub-namespaces for TARGET by
    checking which overlay/<target>/hydra-kernel/.../hydra/overlay/<seg>/lib/ files actually
    exist, rather than maintaining a hand-written allowlist. Existence on disk IS the signal for
    "this host ships a native impl for this sub" -- correct by construction for any future
    hydra.lib.<sub> module whether or not it has a relocated overlay impl (hydra.lib.defaults has
    none and is therefore never redirected, replacing the old by-name exclusion). Falls back to
    _LIB_SUBS_FALLBACK if the overlay source tree isn't reachable from repo_root.
    """
    seg = _overlay_dir_segment(target)
    lib_dir = os.path.join(repo_root, "overlay", target, "hydra-kernel", "src", "main", target,
                            "hydra", "overlay", seg, "lib")
    if not os.path.isdir(lib_dir):
        return _LIB_SUBS_FALLBACK
    subs = []
    for name in os.listdir(lib_dir):
        path = os.path.join(lib_dir, name)
        sub = name if os.path.isdir(path) else os.path.splitext(name)[0]
        if sub.lower() in ("libraries",) or sub in ("__init__", "PrimitiveType"):
            continue
        subs.append(sub)
    return subs or _LIB_SUBS_FALLBACK

# Lisp dialect arg -> (coder dialect name, file extension). Module-level so the #473 lib pass can
# reach it as well as main().
_lisp_dialects = {
    "clojure": ("clojure", "clj"),
    "scheme": ("scheme", "scm"),
    "common-lisp": ("common_lisp", "lisp"),
    "emacs-lisp": ("emacs_lisp", "el"),
}


def _is_lib_module(m):
    return m.name.value.startswith("hydra.lib.")


def _run_lib_pass(target, lang_dir, all_main_mods, mods_to_generate):
    """#473 lib pass: emit the hydra.lib.* PrimitiveDefinition def-modules from their LOWERED form.

    Mirrors genForDirLib in bootstrap-from-json/Main.hs. The lib modules are filtered from
    mods_to_generate and lowered; the universe lowers ONLY the lib modules so a lib
    default-implementation referencing another primitive resolves to the primitive, not a lowered
    binding.
    """
    import hydra.codegen
    from hydra.generation import (write_java, write_python, write_scala,
                                  write_typescript, write_lisp_dialect)

    lib_mods = [hydra.codegen.lower_primitive_definitions(m)
                for m in mods_to_generate if _is_lib_module(m)]
    if not lib_mods:
        return
    lib_universe = [hydra.codegen.lower_primitive_definitions(m) if _is_lib_module(m) else m
                    for m in all_main_mods]

    print(f"Lib pass: emitting {len(lib_mods)} hydra.lib.* definition modules to {target}...",
          flush=True)
    if target == "java":
        write_java(lang_dir, lib_universe, lib_mods)
    elif target == "python":
        write_python(lang_dir, lib_universe, lib_mods)
    elif target == "scala":
        write_scala(lang_dir, lib_universe, lib_mods)
    elif target == "typescript":
        write_typescript(lang_dir, lib_universe, lib_mods)
    elif target in _lisp_dialects:
        dialect_name, ext = _lisp_dialects[target]
        write_lisp_dialect(lang_dir, dialect_name, ext, lib_universe, lib_mods)


def _is_lib_def_or_registry_file(p_slash):
    """Files under hydra/lib/ are the lib-pass def-modules; the overlay Libraries registry
    (overlay/<lang>/.../Libraries.<ext>, copied into the generated tree as
    hydra/overlay/<lang>/Libraries.<ext>) deliberately imports BOTH the relocated impl and the
    def-module (aliased, for `def_X.fn.name`). Neither must be redirected. #569 Defect B fix: the
    previous guard checked "sources/libraries.py" (wrong directory -- never matched the actual
    generated path "hydra/overlay/<lang>/Libraries.py"), so the registry's def-module import got
    wrongly redirected onto the impl, breaking `.name` member access.
    """
    if "/hydra/lib/" in p_slash:
        return True
    m = re.search(r"/hydra/overlay/[^/]+/(Libraries|libraries)\.[^/]+$", p_slash)
    return m is not None


def _redirect_dotted(s, lang_seg, subs):
    """Dotted-language redirect (python/scala/clojure), protecting quoted primitive-NAME strings."""
    sentinel = "@@HYDRA_LIB_NAME@@"  # improbable token; never appears in generated source
    out = s.replace('"hydra.lib.', '"' + sentinel)
    for sub in subs:
        old = "hydra.lib." + sub
        new = "hydra." + lang_seg + ".lib." + sub
        out = out.replace(old + ".", new + ".")
        out = out.replace(old + "\n", new + "\n")
        out = out.replace(old + " ", new + " ")
        out = out.replace("hydra.lib import " + sub, "hydra." + lang_seg + ".lib import " + sub)
    return out.replace(sentinel, "hydra.lib.")


def _redirect_scheme(s, subs):
    """Scheme (R7RS) redirect: `(hydra lib <sub>)` -> `(hydra overlay scheme lib <sub>)`. Call
    sites use the flattened identifier hydra_lib_<sub>_<fn> (unchanged, resolved via the renamed
    import); primitive NAME strings are dotted "hydra.lib..." (untouched by this rewrite)."""
    out = s
    for sub in subs:
        out = out.replace(f"(hydra lib {sub})", f"(hydra overlay scheme lib {sub})")
    return out


def _redirect_lisp_flat(s, subs, lang_seg):
    """Common Lisp / Emacs Lisp redirect: rename consumer call sites
    hydra_lib_<sub>_ -> hydra_overlay_<lang_seg>_lib_<sub>_, and drop the def-module
    ":hydra.lib.<sub>" token from consumer defpackage (:use ...) clauses."""
    out = s
    for sub in subs:
        out = out.replace(f"hydra_lib_{sub}_", f"hydra_overlay_{lang_seg}_lib_{sub}_")
    for sub in subs:
        out = out.replace(f" :hydra.lib.{sub}", "")
    return out


def _redirect_lib_calls(repo_root, target, lang_dir):
    """#473/#568 redirect: rewrite generated CONSUMER call-sites so they resolve to the relocated
    native hydra.overlay.<lang>.lib.* impls instead of the hydra.lib.* def-modules
    (PrimitiveDefinition data, not callable). Dispatches per target's actual reference shape,
    mirroring redirectFor / redirectSchemeFor / redirectLispFlat in bootstrap-from-json/Main.hs:
    dotted (python/scala/clojure), R7RS sexp (scheme), flat identifier (common-lisp/emacs-lisp).
    No-op for java/typescript/haskell (typescript's coder resolves lib references via a generated
    import alias, never raw dotted call-site text). The sub-list comes from
    _lib_subs_for_target()'s overlay-directory existence check (#568), not a hand-maintained
    allowlist.
    """
    if target in ("java", "typescript", "haskell"):
        return
    if not os.path.isdir(lang_dir):
        return
    subs = _lib_subs_for_target(repo_root, target)
    dotted_seg = {"python": "overlay.python", "scala": "overlay.scala", "clojure": "overlay.clojure"}.get(target)
    for dirpath, _dirs, files in os.walk(lang_dir):
        for fn in files:
            p = os.path.join(dirpath, fn)
            p_slash = p.replace(os.sep, "/")
            if _is_lib_def_or_registry_file(p_slash):
                continue
            with open(p, "r", encoding="utf-8") as fh:
                s = fh.read()
            if dotted_seg is not None:
                if "hydra.lib." not in s:
                    continue
                out = _redirect_dotted(s, dotted_seg, subs)
            elif target == "scheme":
                if "(hydra lib " not in s:
                    continue
                out = _redirect_scheme(s, subs)
            elif target == "common-lisp":
                if "hydra_lib_" not in s and ":hydra.lib." not in s:
                    continue
                out = _redirect_lisp_flat(s, subs, "common_lisp")
            elif target == "emacs-lisp":
                if "hydra_lib_" not in s and ":hydra.lib." not in s:
                    continue
                out = _redirect_lisp_flat(s, subs, "emacs_lisp")
            else:
                continue
            if out != s:
                with open(p, "w", encoding="utf-8") as fh:
                    fh.write(out)


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
    from hydra.packaging import ModuleName
    return [ModuleName(ns) for ns in manifest[field_name]]


def _load_package_main(root, pkg):
    """Load the mainModules from a package's manifest."""
    pkg_dir = _package_main_dir(root, pkg)
    all_ns = _read_manifest_field_or_empty(pkg_dir, "mainModules")
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
                        choices=["haskell", "java", "python", "scala", "typescript",
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
    # #568: repo root (parent of dist/json's parent dist/), used to locate overlay/<lang>/ lib
    # directories for the existence-based redirect sub-list. Falls back to _LIB_SUBS_FALLBACK in
    # _lib_subs_for_target() if this guess is wrong.
    repo_root = os.path.dirname(os.path.dirname(os.path.abspath(dist_json_root)))

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

    # Namespaces in the bootstrap baseline (used for --kernel-only filtering).
    # Both hydra-kernel and hydra-haskell are baseline: hydra-haskell provides
    # the runtime AST modules that the generated DSL source modules import.
    kernel_ns_set = {m.name.value for m in baseline_mods}

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
        mods_to_generate = [m for m in mods_to_generate if m.name.value in kernel_ns_set]
        all_main_mods = [m for m in all_main_mods if m.name.value in kernel_ns_set]
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

    # #546/#547: the kernel test suite references hydra.test.build.* -> hydra.build.*
    # (Option A). When emitting tests, hydra-build's main modules must be in the
    # universe so those refs type-check; otherwise cross-host generation fails with
    # "Unknown variable: hydra.test.build.modules.allTests". Mirrors the Haskell
    # bootstrap-from-json + Java Bootstrap fix.
    if args.include_tests:
        full_mods = full_mods + _load_package_main(dist_json_root, "hydra-build")

    # Generate main modules
    out_main = os.path.join(out_dir, "src/main")
    print(f"Mapping {len(mods_to_generate)} modules to {target_cap}...", flush=True)
    print(f"  Universe: {len(all_main_mods)} modules", flush=True)
    print(f"  Output: {out_main}", flush=True)
    print(flush=True)

    step_start = time.time()

    if args.target == "haskell":
        write_haskell(os.path.join(out_main, "haskell"), all_main_mods, mods_to_generate)
    elif args.target == "java":
        write_java(os.path.join(out_main, "java"), all_main_mods, mods_to_generate)
    elif args.target == "python":
        write_python(os.path.join(out_main, "python"), all_main_mods, mods_to_generate)
    elif args.target == "scala":
        from hydra.generation import write_scala
        write_scala(os.path.join(out_main, "scala"), all_main_mods, mods_to_generate)
    elif args.target == "typescript":
        from hydra.generation import write_typescript
        write_typescript(os.path.join(out_main, "typescript"), all_main_mods, mods_to_generate)
    elif args.target in _lisp_dialects:
        dialect_name, _ext = _lisp_dialects[args.target]
        write_lisp_dialect(os.path.join(out_main, args.target), dialect_name, _ext,
                           all_main_mods, mods_to_generate)

    # #473 Step 0 — lib pass + redirect. The hydra.lib.* primitive IMPLEMENTATIONS live at
    # hydra.overlay.<lang>.lib.* (the analog of Haskell's Hydra.Overlay.Haskell.Lib.*), so hydra.lib.* is free for
    # the generated PrimitiveDefinition def-modules. Mirrors the Haskell driver's twoPassLib logic in
    # bootstrap-from-json/Main.hs: when the Python host generates a target that consumes def-modules
    # (everything except haskell, which uses the registry), it must (1) emit the hydra.lib.* def-modules
    # from their LOWERED form (lib pass), and (2) redirect generated CONSUMER call-sites
    # hydra.lib.<sub>.<fn> -> hydra.<lang>.lib.<sub>.<fn> so they resolve to the relocated impls.
    # Without this, self-host (python-to-python, python-to-java, ...) emits impls/consumers that
    # reference the def-module namespace as if callable -> "PrimitiveDefinition object is not callable"
    # / "cannot find symbol". See project_473_self_host_lib_pass_gap.
    # Lib pass emits the hydra.lib.* def-modules now (alongside the main pass output). The redirect,
    # however, must run LAST — after the test + ext-for-tests passes below also write into
    # out_main/<target> — so every generated consumer file (whichever pass produced it) gets its
    # hydra.lib.* impl call-sites redirected. Running it here would miss files written later.
    if args.target != "haskell":
        _run_lib_pass(args.target, os.path.join(out_main, args.target), all_main_mods, mods_to_generate)

    step_time = time.time() - step_start

    ext = {"java": ".java", "python": ".py", "haskell": ".hs", "scala": ".scala",
           "typescript": ".ts",
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
        # #546/#547: also load hydra-build's OWN test modules (hydra.test.build.*),
        # imported by the kernel test suite (Option A) but living in hydra-build's
        # test tree, not hydra-kernel's.
        build_main_dir = _package_main_dir(dist_json_root, "hydra-build")
        build_test_json_dir = os.path.join(dist_json_root, "hydra-build", "src", "test", "json")
        build_test_ns = read_manifest_field(build_main_dir, "testModules")
        if build_test_ns:
            test_mods = test_mods + load_modules_from_json(build_test_json_dir, build_test_ns)
        step_time = time.time() - step_start
        test_bindings = sum(len(m.definitions) for m in test_mods)
        print(f"  Loaded {len(test_mods)} test modules ({test_bindings} bindings).", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)

        all_universe = full_mods + test_mods

        # Filter skip-emit test namespaces (e.g. hydra.test.testEnv): these are
        # type-only stubs in the DSL whose hand-written per-language
        # counterparts are the source of truth. Emitting them would overwrite
        # hand-written code that registers primitives for the test graph.
        # Mirrors testSkipEmitModuleNames in Hydra.Sources.Test.All.
        _test_skip_emit = {"hydra.test.testEnv"}
        test_mods = [m for m in test_mods if m.name.value not in _test_skip_emit]
        out_test = os.path.join(out_dir, "src/test")

        # When --kernel-only is active, ext modules are excluded from the main
        # generation targets. But test modules may depend on ext modules (e.g.
        # hydra.test.serialization depends on hydra.haskell.operators).
        # Generate those ext modules to outMain so test code can reference them.
        if args.kernel_only:
            test_ext_deps = set()
            for m in test_mods:
                for dep in m.dependencies:
                    if dep.module.value.startswith("hydra."):
                        test_ext_deps.add(dep.module.value)
            if test_ext_deps:
                ext_mods_for_tests = [m for m in full_mods if m.name.value in test_ext_deps]
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
                    elif args.target == "typescript":
                        from hydra.generation import write_typescript
                        write_typescript(os.path.join(out_main_sub, "typescript"), all_universe, ext_mods_for_tests)
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
        elif args.target == "typescript":
            from hydra.generation import write_typescript
            write_typescript(os.path.join(out_test, "typescript"), all_universe, test_mods)
        elif args.target in _lisp_dialects:
            d, e = _lisp_dialects[args.target]
            write_lisp_dialect(os.path.join(out_test, args.target), d, e,
                               all_universe, test_mods)

        step_time = time.time() - step_start
        test_file_count = _count_files(os.path.join(out_dir, "src/test"), ext)

        print(f"  Generated {test_file_count} test files.", flush=True)
        print(f"  Time: {_format_time(step_time)}", flush=True)
        print(flush=True)

    # #473 redirect — run LAST, over every generated dir (main + test), so consumer call-sites
    # written by any pass (main, lib, test, ext-for-tests) have their hydra.lib.* impl references
    # rewritten to hydra.<lang>.lib.*. See the lib-pass note above and project_473_self_host_lib_pass_gap.
    if args.target != "haskell":
        _redirect_lib_calls(repo_root, args.target, os.path.join(out_main, args.target))
        if args.include_tests:
            _redirect_lib_calls(repo_root, args.target, os.path.join(out_dir, "src/test", args.target))

    total_time = time.time() - total_start

    print("==========================================", flush=True)
    test_str = f" + {test_file_count} test" if args.include_tests else ""
    print(f"Done: {main_file_count} main{test_str} files", flush=True)
    print(f"  Output: {out_dir}", flush=True)
    print(f"  Total time: {_format_time(total_time)}", flush=True)
    print("==========================================", flush=True)


if __name__ == "__main__":
    main()
