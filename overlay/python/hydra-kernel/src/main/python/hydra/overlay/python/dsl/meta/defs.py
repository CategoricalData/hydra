"""Meta-DSL helper for verifying module def-registration completeness.

Mirrors the role of Java's Defs.checkComplete (overlay/java/hydra-kernel/.../dsl/meta/Defs.java):
each Hydra definition in a Python source module is authored as a zero-argument, underscore-prefixed
module-level function (e.g. `def _fooBar(): return _def("fooBar")...`), then separately registered by
name in the module's definitions tuple (built by `_build_module()` or an inline list literal). Omitting
a def-producing function from that tuple silently drops it from the generated module. check_complete
catches this by calling every def-producing function found in the module and confirming its name
appears among the registered definitions.
"""

import inspect

from hydra.typed import TypedBinding


def _definition_name(d):
    """Extract the fully-qualified Name from a Definition (or a bare TypedBinding)."""
    if isinstance(d, TypedBinding):
        return d.name
    value = getattr(d, "value", None)
    return getattr(value, "name", None)


def _is_def_producer_candidate(fn):
    """A def-producing function is zero-argument and locally defined (not imported)."""
    try:
        params = inspect.signature(fn).parameters
    except (TypeError, ValueError):
        return False
    return len(params) == 0


def check_complete(module, registered):
    """Verify every def-producing function declared in `module` is present in `registered`.

    `registered` is the definitions tuple/list actually assembled for the module (e.g.
    `module_.definitions`). Walks every module-level, zero-argument function whose name starts with a
    single underscore, calls it, and checks whether the resulting definition's name is among
    `registered`'s names. A call result that isn't Definition/TypedBinding-shaped (i.e. has no
    extractable name) is treated as "not a def-producer" and skipped — not every `_foo` helper
    produces a definition. Raises AssertionError naming any orphaned function.
    """
    registered_names = {_definition_name(d) for d in registered}
    registered_names.discard(None)

    orphans = []
    for attr_name, fn in inspect.getmembers(module, inspect.isfunction):
        if fn.__module__ != module.__name__:
            continue
        if not attr_name.startswith("_") or attr_name.startswith("__"):
            continue
        if not _is_def_producer_candidate(fn):
            continue
        result = fn()
        name = _definition_name(result)
        if name is None:
            continue
        if name not in registered_names:
            orphans.append(attr_name)

    if orphans:
        raise AssertionError(
            f"check_complete: {module.__name__} declares def-producing function(s) missing from "
            f"its definitions list: {', '.join(sorted(orphans))}. Add each to the module's "
            f"definitions tuple, or remove the unused function."
        )
