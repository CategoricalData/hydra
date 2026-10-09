"""Regression test for #779: the Python coder must not break on nested module
names (one module's namespace a strict dotted prefix of another's), which
validation.md permits outside Hydra's own packages (SHOULD-class
checkNestedModuleNames).

Without a fix, modules ``a.b`` and ``a.b.c`` generate to ``a/b.py`` and
``a/b/c.py`` with no ``__init__.py``. Python then resolves ``a.b`` to the
plain module, shadowing the ``a/b/`` namespace-package directory, so
``a.b.c`` becomes unimportable. ``hydra.generation._fix_nested_module_paths``
rewrites ``a/b.py`` to ``a/b/__init__.py`` whenever some other generated path
starts with ``a/b/``, making both modules importable.
"""

import os
import subprocess
import sys

from hydra.core.packaging import (
    Module,
    ModuleName,
    ModuleDependency,
    TermDefinition,
    DefinitionTerm,
)
from hydra.core.model import Name, TermLiteral, LiteralString
from hydra.core.overlay.python.dsl.python import None_
from hydra.generation import _fix_nested_module_paths, generate_sources


def _subprocess_env():
    """Propagate this process's sys.path (populated by pytest's `pythonpath`
    ini option, pointing at dist/python/*) to the subprocess as PYTHONPATH --
    a child process does not inherit the parent's sys.path on its own."""
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(p for p in sys.path if p)
    return env


def _term_module(name, dep_names, local_name, literal_value):
    return Module(
        name=ModuleName(name),
        metadata=None_(),
        dependencies=[ModuleDependency(module=ModuleName(d), package=None_()) for d in dep_names],
        definitions=[DefinitionTerm(TermDefinition(
            name=Name(f"{name}.{local_name}"), metadata=None_(), signature=None_(),
            body=TermLiteral(LiteralString(literal_value))))],
    )


def _write_python_with_inference(base_path, universe, mods):
    """Like ``hydra.generation.write_python``, but with ``do_infer=True`` so
    these tests can use plain untyped literal terms as fixtures instead of
    hand-building a full TermSignature/TypeScheme -- write_python itself
    always passes do_infer=False because production callers' modules have
    already been through inference upstream."""
    from hydra.python.coder import module_to_python
    from hydra.python.language import python_language

    known_subs = frozenset({
        "chars", "effects", "eithers", "equality", "files", "functions", "hashing",
        "lists", "literals", "logic", "maps", "math", "optionals", "ordering",
        "pairs", "regex", "sets", "strings", "system", "text",
    })
    generate_sources(
        (lambda mod, defs, cx, g: module_to_python(known_subs, mod, defs, cx, g)),
        python_language(),
        True,
        base_path, universe, mods,
        fix_nested_packages=True)


def test_fix_nested_module_paths_rewrites_nested_pair():
    files = [("a/b.py", "x = 1"), ("a/b/c.py", "y = 2")]
    fixed = dict(_fix_nested_module_paths(files))
    assert fixed == {"a/b/__init__.py": "x = 1", "a/b/c.py": "y = 2"}


def test_fix_nested_module_paths_leaves_non_nested_siblings_alone():
    files = [("a/b.py", "x = 1"), ("a/d.py", "y = 2")]
    fixed = dict(_fix_nested_module_paths(files))
    assert fixed == {"a/b.py": "x = 1", "a/d.py": "y = 2"}


def test_generated_nested_modules_are_both_importable(tmp_path):
    """End-to-end: generate a.b / a.b.c to real files and import both in a
    subprocess (a fresh interpreter, so there's no import-system state left
    over from this test process to mask the bug)."""
    mod_b = _term_module("a.b", [], "x", "from a.b")
    mod_bc = _term_module("a.b.c", ["a.b"], "y", "from a.b.c")

    base_path = str(tmp_path)
    _write_python_with_inference(base_path, [mod_b, mod_bc], [mod_b, mod_bc])

    assert os.path.isfile(os.path.join(base_path, "a", "b", "__init__.py"))
    assert os.path.isfile(os.path.join(base_path, "a", "b", "c.py"))

    script = (
        "import a.b, a.b.c; "
        "assert a.b.x == 'from a.b'; "
        "assert a.b.c.y == 'from a.b.c'"
    )
    subprocess.run(
        [sys.executable, "-c", script],
        cwd=base_path, check=True, env=_subprocess_env(),
    )


def test_generated_non_nested_siblings_still_importable(tmp_path):
    """Control case from the issue: a.b / a.d (no prefix relation) must be
    unaffected by the fix."""
    mod_b = _term_module("a.b", [], "x", "from a.b")
    mod_d = _term_module("a.d", ["a.b"], "y", "from a.b")

    base_path = str(tmp_path)
    _write_python_with_inference(base_path, [mod_b, mod_d], [mod_b, mod_d])

    assert os.path.isfile(os.path.join(base_path, "a", "b.py"))
    assert os.path.isfile(os.path.join(base_path, "a", "d.py"))

    script = "import a.b, a.d; assert a.d.y == a.b.x"
    subprocess.run(
        [sys.executable, "-c", script],
        cwd=base_path, check=True, env=_subprocess_env(),
    )
