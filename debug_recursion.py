#!/usr/bin/env python3
"""Debug script to catch infinite recursion in subst_in_type_non_empty."""

import sys
sys.setrecursionlimit(300)  # Low limit to catch it fast
sys.path.insert(0, "hydra-python/src/gen-main/python")
sys.path.insert(0, "hydra-python/src/main/python")

import hydra.substitution
import hydra.core
import hydra.typing

# Monkey-patch subst_in_type_non_empty to log calls
_orig_subst = hydra.substitution.subst_in_type_non_empty

_call_depth = 0

def _collect_type_vars(typ, vars_set, depth=0):
    if depth > 20:
        return
    match typ:
        case hydra.core.TypeVariable(value=v):
            vars_set.add(v)
        case hydra.core.TypeFunction(value=f):
            _collect_type_vars(f.domain, vars_set, depth+1)
            _collect_type_vars(f.codomain, vars_set, depth+1)
        case hydra.core.TypeList(value=t):
            _collect_type_vars(t, vars_set, depth+1)
        case hydra.core.TypeSet(value=t):
            _collect_type_vars(t, vars_set, depth+1)
        case hydra.core.TypeMap(value=mt):
            _collect_type_vars(mt.key, vars_set, depth+1)
            _collect_type_vars(mt.value, vars_set, depth+1)
        case hydra.core.TypeOptional(value=t):
            _collect_type_vars(t, vars_set, depth+1)
        case hydra.core.TypeApplication(value=app):
            _collect_type_vars(app.function, vars_set, depth+1)
            _collect_type_vars(app.argument, vars_set, depth+1)
        case hydra.core.TypeForall(value=ft):
            _collect_type_vars(ft.body, vars_set, depth+1)
            vars_set.discard(ft.parameter)
        case hydra.core.TypeRecord(value=rt):
            for f in rt.fields:
                _collect_type_vars(f.type, vars_set, depth+1)
        case hydra.core.TypeUnion(value=rt):
            for f in rt.fields:
                _collect_type_vars(f.type, vars_set, depth+1)
        case hydra.core.TypeProduct(value=ts):
            for t in ts:
                _collect_type_vars(t, vars_set, depth+1)
        case hydra.core.TypeSum(value=ts):
            for t in ts:
                _collect_type_vars(t, vars_set, depth+1)

def _type_summary(typ, depth=0):
    if depth > 5:
        return "..."
    match typ:
        case hydra.core.TypeVariable(value=v):
            return f"Var({v})"
        case hydra.core.TypeFunction(value=f):
            return f"({_type_summary(f.domain, depth+1)} -> {_type_summary(f.codomain, depth+1)})"
        case hydra.core.TypeList(value=t):
            return f"[{_type_summary(t, depth+1)}]"
        case hydra.core.TypeOptional(value=t):
            return f"?{_type_summary(t, depth+1)}"
        case hydra.core.TypeApplication(value=app):
            return f"App({_type_summary(app.function, depth+1)}, {_type_summary(app.argument, depth+1)})"
        case hydra.core.TypeForall(value=ft):
            return f"∀{ft.parameter}.{_type_summary(ft.body, depth+1)}"
        case hydra.core.TypeRecord(value=rt):
            return f"Rec({rt.name})"
        case hydra.core.TypeUnion(value=rt):
            return f"Un({rt.name})"
        case hydra.core.TypeNominal(value=n):
            return f"Nom({n})"
        case _:
            return type(typ).__name__

def _debug_subst(subst, typ0):
    global _call_depth
    _call_depth += 1

    if _call_depth > 100:
        print(f"\n=== DEEP RECURSION at depth {_call_depth} ===")
        print(f"Type: {_type_summary(typ0)}")
        print(f"Subst size: {len(subst.value)}")

        # Check for self-referential substitutions
        self_refs = []
        for k, v in subst.value.items():
            free_vars = set()
            _collect_type_vars(v, free_vars)
            if k in free_vars:
                self_refs.append((k, v))

        if self_refs:
            print(f"\nSELF-REFERENTIAL SUBSTITUTIONS ({len(self_refs)}):")
            for k, v in self_refs[:10]:
                print(f"  {k} -> {_type_summary(v)}")
        else:
            print("\nNo self-referential substitutions found.")
            print("Substitution map:")
            for k, v in list(subst.value.items())[:20]:
                print(f"  {k} -> {_type_summary(v)}")

        if _call_depth > 150:
            raise RecursionError("Caught infinite recursion for debugging")

    try:
        result = _orig_subst(subst, typ0)
        return result
    finally:
        _call_depth -= 1

hydra.substitution.subst_in_type_non_empty = _debug_subst

# Also patch the public entry point
_orig_subst_in_type = hydra.substitution.subst_in_type
def _debug_subst_in_type(subst, typ0):
    import hydra.lib.maps
    if hydra.lib.maps.null(subst.value):
        return typ0
    return _debug_subst(subst, typ0)
hydra.substitution.subst_in_type = _debug_subst_in_type

# Now run the bootstrap
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
except RecursionError as e:
    print(f"\nCaught: {e}")
except Exception as e:
    print(f"\nError: {type(e).__name__}: {e}")
    import traceback
    traceback.print_exc()
